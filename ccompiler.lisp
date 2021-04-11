(in-package #:3b-mmath)

;;; compiler for 'concrete' code, compiles meta code to concrete CL
;;; code for specific set of argument types


(defvar *wrapper-aliases* (make-hash-table))
(defmacro defwrap (name lambda-list expansion)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *wrapper-aliases*)
                       (lambda ,lambda-list ,expansion))))

(defun get-var-type (name env)
  (cdr (assoc 'type (nth-value 2 (introspect-environment:variable-information
                                  name env)))))

(defmethod wrapper (type type-args env args)
  (declare (ignorable env))
  (error "unknown wrapper ~s ~s?~% ~s" type type-args args))


(Defmethod wrapper ((type (eql 'cl-vector)) type-args env args)
  (destructuring-bind (var &key (static-offset 0) runtime-offset) args
    (destructuring-bind (shape &key type row-major) type-args
      (when runtime-offset
        (error "runtime offset not implemented yet (or handled by different wrapper?)"))
      (let ((declared-type (introspect-environment:typexpand
                            (get-var-type var env))))
        (setf type (introspect-environment:typexpand type))
        (if (or (not type) (eql type :auto))
            (setf type declared-type)
            (when (and declared-type (not (eql declared-type t)))
              ;; todo: add a coerce when explicit type doesn't match
              ;; declaration?
              (assert (subtypep declared-type type))))
        `(,(if row-major
               'row-major-matrix
               'column-major-matrix)
          (make-instance 'storage-cl-vector
                         :storage ',var
                         :size ,(reduce '* shape)
                         :offset ,static-offset
                         :element-type ',(second (a:ensure-list type)))
          ,@shape)))))

(Defmethod wrapper ((type (eql 'scalar)) type-args env args)
  (destructuring-bind (var) args
    (destructuring-bind (&optional type) type-args
      (let ((declared-type (introspect-environment:typexpand
                            (get-var-type var env))))
        (setf type (introspect-environment:typexpand type))
        (if (or (not type) (eql type :auto))
            (setf type declared-type)
            (when (and declared-type (not (eql declared-type t)))
              ;; todo: add a coerce when explicit type doesn't match
              ;; declaration?
              (assert (subtypep declared-type type))))
        `(make-scalar ',type ',var)))))


#++
(defmacro defcfun (name lambda-list &body body)
  (multiple-value-bind (body2 decl doc)
      (a:parse-body body :documentation t)
    `(defun ,name ,lambda-list
       ,@decl
       ,@ (when doc (list doc))
       (cprogn
        ,@body2))))

;; find free variables, and store any type declarations valid at use
(defclass find-free-vars (scalar-vector-pass) ())

(defvar *free-vars*)

(defmethod w:filter-ast ((node w:variable-reference)
                         (walker find-free-vars))
  (when (and *vector-context*
             (typep (w:binding node) 'w:free-variable))
    (pushnew (w:name (w:binding node)) *free-vars*))
  node)



(defclass expand-wrappers ()
  ((env :Reader env :initarg :env)))

(defun get-wrap (wrap)
  (setf wrap (a:ensure-list wrap))
  (let ((a (gethash (car wrap) *wrapper-aliases*)))
    (when a
      (setf a (apply a (cdr wrap))))
    (if (and a (not (equalp a wrap)))
        (get-wrap a)
        wrap)))


(defmethod w:filter-ast ((node special-form-wrap)
                         (walker expand-wrappers))
  (let ((wrap (get-wrap (rtype node)))
        (args (args node)))
    (w:sexp->ast (wrapper (car wrap) (cdr wrap)
                          (env walker)
                          args))))

(defmethod w:filter-ast :before (node (w expand-wrappers))
  (when (typep node 'w::form-with-scope)
    (assert (w::env node)))
  (assert (not (position nil 3b-walker::*lexical-environment*))))

(defmethod w:cons->ast :around (car whole)
  (let ((a (call-next-method)))
    (when (typep a 'w::form-with-scope)
      (assert (w::env a)))
    a))
(defparameter *ccompiler-passes* '(expand-aliases
                                   wrap-constants
                                   vector-expander
                                   expand-wrappers))

(defun run-passes (code passes)
  (let ((ast (w:sexp->ast code)))
    (loop for pass in passes
          do (setf ast (w:filter-ast ast pass)))
    (w:ast->sexp ast)))


(defun implicit-wrappers (form env)
  (flet ((wrap (v)
           (let ((info (multiple-value-list
                        (introspect-environment:variable-information v env))))
             (let ((type (get-var-type v env)))
               (typecase type
                 ((or (member vector array simple-array)
                      (cons (member vector array simple-array)))
                  (error "implicit wrapper for variable declared with non-scalar type ~s?"
                         type))
                 ((not (or null (eql t)))
                  `(wrap (scalar ,type) ,v))
                 (t
                  `(wrap scalar ,v)))))))
   (let ((*free-vars* nil))
     (run-passes form '(find-free-vars))
     (let ((wraps (loop for i in *free-vars*
                        collect (list i (wrap i)))))
       `(let ,wraps ,form)))))

(defun ccompile (form env &key &allow-other-keys)
  (with-dsl-environment ()
    (let* ((m (run-passes (implicit-wrappers form env)
                          (subst
                           (make-instance 'expand-wrappers :env env)
                           'expand-wrappers
                           *ccompiler-passes*))))
      (let ((r (funcall (compile nil `(lambda () ,m)))))
        (etypecase r
          (scalar
           (scalar-value r))
          (operation
           (let* ((d (array-dimensions (permutation r)))
                  (c (reduce '* d))
                  (el (element-type r)))
             (if d
                 (a:with-gensyms (ret)
                   `(let ((,ret (make-array ,c :element-type ',el)))
                      ,@ (let* ((s (make-instance 'storage-cl-vector
                                                  :storage ret
                                                  :size c
                                                  :element-type el))
                                (a (apply #'column-major-matrix s d))
                                (forms nil))
                           (map-dimensions
                            d (lambda (i)
                                (push (write-element (read-element r i)
                                                     a i
                                                     :type (element-type r))
                                      forms)))
                           (nreverse forms))
                      ,ret))
                 (error "todo, is returning empty operation valid?")))))))))


#++
(defmacro cprogn (&body body)
  (ccompile `(mprogn ,@body)))

(defmacro mm ((&key (runtime-overlap :ignore)
               &allow-other-keys)
              &body body
              &environment env)
  ;; runtime-overlap can be :IGNORE (optimize assuming no aliasing),
  ;; :ALLOW (generate temps assuming arbitrary overlap), :ERROR (like
  ;; ignore, but check and error if arguments alias), :DUPLICATE
  ;; (generate code for ignore and allow, and pick depending on
  ;; runtime test)
  (ccompile `(mprogn ,@body)
            env
            :runtime-overlap runtime-overlap))
