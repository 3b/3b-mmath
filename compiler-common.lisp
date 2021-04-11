(in-package #:3b-mmath)

(defvar *meta-functions* (make-hash-table))
(defun add-meta-function (name)
  (setf (gethash name *meta-functions*) t))

;; adding separately from def to make runtime recompilation easier
;; without losing other changes
(map nil 'add-meta-function '(per-element
                              reduce-dimension
                              reduce
                              ->
                              reshape
                              reshape*
                              replace
                              wrap
                              make-scalar
                              make-literal-matrix))


(defvar *vector-context* nil)

(defvar *enter-vector-context*
  '(mprogn))

(defvar *leave-vector-context*
  '(sprogn make-scalar make-literal-matrix))


(defmacro maybe-vector-context ((node &optional enter) &body body)
  "Evaluate BODY in vector or scalar context depending on operator of
NODE. If specified, bind variable named by ENTER to T when NODE is
included in list of operators that enter vector context. "
  (a:with-gensyms (op)
    (let ((enter (or enter (gensym "enter"))))
      `(let* ((,op (if (typep ,node 'w:normal-application)
                       (w:name (w:binding ,node))
                       (car (w:whole ,node))))
              (,enter (member ,op *enter-vector-context*))
              (*vector-context* (cond
                                  (,enter
                                   t)
                                  ((member ,op *leave-vector-context*)
                                   nil)
                                  (t *vector-context*))))
         ,@body))))



;; base class for compilation steps that need to distinguish
;; scalar/vector context
(defclass scalar-vector-pass ()
  ((enter :accessor enter :initform nil)))

(defmethod w:filter-ast :around ((node w:function-application)
                                 (walker scalar-vector-pass))
  (maybe-vector-context (node enter)
    (if enter
        (setf (enter walker) node)
        (setf (enter walker) nil))
    (call-next-method)))

(defmethod w:filter-ast ((node w:macro-application)
                         (walker scalar-vector-pass))
  (maybe-vector-context (node enter)
    (call-next-method)))

(defmethod w:filter-ast ((node w:special-form-application)
                         (walker scalar-vector-pass))
  (maybe-vector-context (node enter)
    (if enter
        (setf (enter walker) node)
        (setf (enter walker) nil))
    (call-next-method)))

;; expand (+ a b) to (per-element '+ a b) etc in body
(defclass vector-expander (scalar-vector-pass)
  ())

(defmethod w:filter-ast ((node w:function-application)
                         (walker vector-expander))
  ;; bind ENTER so we see value from around method instead of whatever
  ;; was from walking tree
  (let ((enter (eql node (enter walker))))
    (setf node (call-next-method))
    (if (or enter
            (gethash (w:name (w:binding node)) *meta-functions*)
            (not *vector-context*))
        node
        (apply 'w::make-function-call
               ;; not sure if this can accept a binding object or not,
               ;; so extract name (probably can't now, but might be able
               ;; to if α etc are implemented as AST transforms rather
               ;; than macros or sexp transforms?
               'per-element (list 'quote (w:name (w:binding node)))
               (w:args node)))))


(defclass expand-aliases (scalar-vector-pass)
  ())

(defmethod w:filter-ast ((node w:function-application)
                         (walker expand-aliases))
  (let ((enter (eql node (enter walker))))
    (setf node (call-next-method))
    (let ((b (w:binding node)))
      (if (or enter
              (not *vector-context*)
              (or (not (typep b 'w:host-function))
                  (eql (w:name b) (w::designator b))))
          node
          (apply 'w::make-function-call (w::designator b) (w:args node))))))


;; replace literals and host variables with scalars
(defclass wrap-constants (scalar-vector-pass) ())

(defmethod w:filter-ast ((node w:constant-reference)
                         (walker wrap-constants))
  (if *vector-context*
      (w::make-function-call 'make-scalar t (w:name (w:binding node)))
      node))

(defmethod w:filter-ast ((node w:variable-reference)
                         (walker wrap-constants))
  (if (and *vector-context*
           (typep (w:binding node) 'w:host-variable))
      (w::make-function-call 'make-scalar t (w:name (w:binding node)))
      node))

(defmethod w:filter-ast (node (walker wrap-constants))
  (if (and *vector-context*
           (atom node)
           (not (typep node 'w:ast-node))
           (not (symbolp node)))
      (w::make-function-call 'make-scalar t node)
      (call-next-method)))

