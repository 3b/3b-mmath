(in-package #:3b-mmath)

;;; compiler for 'meta' code, mostly just does some minor transforms
;;; like implicit per-element

;; define a 'meta' function that operates on accessors at
;; concrete-function compile time
(defmacro defmfun (name lambda-list &body body)
  (multiple-value-bind (body2 decl doc)
      (a:parse-body body :documentation t)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defun ,name ,lambda-list
         ,@decl
         ,@ (when doc (list doc))
         (mprogn
           ,@body2))
       (add-meta-function ',name))))

(defparameter *mcompiler-passes* '(expand-aliases
                                   wrap-constants
                                   vector-expander))

(defun mcompile (body)
  (with-dsl-environment ()
   (let ((ast (w:sexp->ast body)))
     (loop for pass in *mcompiler-passes*
           do (setf ast (w:filter-ast ast pass)))
     (w:ast->sexp ast))))

(defmacro mprogn (&whole whole &body body)
  (declare (ignore body))
  ;; when evaluated from normal CL code, compiles body using mcompiler
  (let ((*vector-context* t))
    (mcompile whole)))

