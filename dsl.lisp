(in-package #:3b-mmath)

;;; various extra special operators / macros for use in dsl code

(defvar *dsl-environment* (make-instance 'w::lexical-environment))

(w:add-host-function 'reduce-dimension :env *dsl-environment* :alias 'reduce)

(defmacro with-dsl-environment (() &body body)
  `(w::with-host-environment ()
     (let ((w::*lexical-environment* (cons *dsl-environment*
                                           w::*lexical-environment*)))
       ,@body)))

;;; compiles body as vector code
(defclass special-form-mprogn (w:special-form-progn)
  ())

(defmethod w:cons->ast ((car (eql 'mprogn)) whole)
  (destructuring-bind (c &rest body) whole
    (assert (eql c 'mprogn))
    (make-instance 'special-form-sprogn
                   :body (w:progn->ast body)
                   :whole whole)))

;;; compile body as scalar code
(defclass special-form-sprogn (w:special-form-progn)
  ())

(defmethod w:cons->ast ((car (eql 'sprogn)) whole)
  (destructuring-bind (c &rest body) whole
    (assert (eql c 'sprogn))
    (make-instance 'special-form-sprogn
                   :body (w:progn->ast body)
                   :whole whole)))


;;; in concrete code, return contents of an accessor as some CL type
;;; (allocated as needed)
(defclass special-form-return-from-as (w:special-form-return-from)
  ;; (return-from-as block value type), block and type not evaluated
  ((rtype :accessor rtype :initarg :rtype)))

(defmethod w:cons->ast ((car (eql 'return-from-as)) whole)
  (destructuring-bind (c block-name result type) whole
    (assert (eql c 'return-from-as))
    (assert (symbolp block-name))
    (make-instance 'special-form-return-from
                   :binding (w::lookup-block block-name)
                   :result (w:sexp->ast result)
                   :rtype type
                   :whole whole)))

(defmacro return-as (result type)
  `(return-from-as nil ,result ,type))

;;; in concrete code, used to wrap a (combination of) variable(s) in
;;; an accessor.
;;ex: (wrap scalar x) would return a scalar accessor for X

;; (wrap (cl-vector 4 4) x 16) would return a 4x4 matrix accessor for
;; X starting at offset 16

;; (wrap (cl-struct (3 1) double-float) foo x y z) would return a 3x1
;; column vector accessor for X,Y,Z slots of struct in FOO


(defclass special-form-wrap (w:special-form-application)
  ;; (wrap type base-var &rest var),
  ((rtype :accessor rtype :initarg :rtype)
   (args :accessor args :initarg :args)))

(defmethod w:cons->ast ((car (eql 'wrap)) whole)
  (destructuring-bind (c type &rest args) whole
    (assert (eql c 'wrap))
    (make-instance 'special-form-wrap
                   :args args
                   :rtype type
                   :whole whole)))

(defmethod w:walk-ast ((node special-form-wrap)
                       (walker w:ast->sexp))
  (flet ((q (a)
           (if (constantp a) a (list 'quote a))))
    `(wrap ',(rtype node) ,@(mapcar #'q (args node)))))


