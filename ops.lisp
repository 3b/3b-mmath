(in-package #:3b-mmath)

;;; operator accessor constructors

(defclass operation ()
  ((element-type :initarg :element-type :reader element-type)
   (permutation :reader permutation :initarg :permutation)))

(defmethod read-element ((p operation) indices &key)
  (apply 'aref (permutation p) indices))

(defmethod dimensions ((p operation))
  (array-dimensions (permutation p)))

(defparameter *op-types*
  ;; hard-code some relatively common ops that change types
  (a:plist-hash-table '(floor integer
                        round integer
                        truncate integer
                        mod integer
                        rem integer
                        )))

(defparameter *op-type-funs*
  (a:plist-hash-table '()))

(defparameter *ftype-order*
  (a:plist-hash-table '(short-float 1
                        single-float 2
                        double-float 3
                        long-float 4)))
(defun op-type (op types)
  ;; assume NIL in type list is "missing" rather than actual (empty)
  ;; NIL type, which would be treated as a float below
  (setf types (substitute t nil types))
  (or (gethash op *op-types*)
      (and (gethash op *op-type-funs*)
           (funcall (gethash op *op-type-funs*) types))
      ;; heuristics to try to guess a useful type for results. not right
      ;; in general, but hopefully good enough for intended uses.
      (cond
        ;; if some float types, pick largest float type
        ((some (lambda (a)
                 (subtypep a 'float))
               types)
         (flet ((m (a b)
                  (if (>= (gethash a *ftype-order* 0)
                          (gethash b *ftype-order* 0))
                      a b)))
          (loop with m = 'short-float
                for type in types
                do (setf m (m m type))
                finally (return m))))
        ;; if all numeric types, expand to number since we can't predict
        ;; what OP does in general
        ((every (lambda (a) (subtypep a 'number)) types)
         'number)
        ;; all same type, return it? can't think of enough examples
        ;; where this would be correct, so skipping it...
        #++((every (lambda (a) (equalp a (car types))) types)
         (car types))
        ;; otherwise just return T
        (t t))))


;; apply operator per element to accessor(s) (if no accessors are
;; supplied, returns a scalar that contains (op)
(defun per-element (op &rest args)
  (cond
    ;; no args, return `(,op) as a scalar
    ((not args)
     (return-from per-element
       (make-scalar t (list op))))
    ;; all args are scalars, return `(,op ,@args) as a scalar
    ((every 'is-scalar args)
     (return-from per-element
       (make-scalar (op-type op (mapcar 'element-type args))
                    (list* op
                           (loop for a in args
                                 collect (read-element a nil))))))
    (t
     ;; remove NIL since scalars confuse size calc
     (let* ((dims (remove 'nil (map 'list 'dimensions args)))
            (d1 (car dims))
            (odims (make-array (length (car dims)) :initial-contents d1)))
       (loop for d2 in (cdr dims)
             do (loop for a in d1
                      for b in d2
                      for i from 0
                      unless (or (= a b) (= a 0) (= b 0))
                        do (error "dimension mismatch in per-element?~%~{~s~%~}"
                                  dims)
                      do (setf (aref odims i) (max (aref odims i) b))))
       (let ((broadcast (find 0 odims)))
         (when broadcast
           ;; for now, just flatten broadcasts ;; fixme: is this right?
           (setf odims (substitute 1 0 odims)))
         (let ((p (make-array (coerce odims 'list))))
           (map-dimensions (coerce odims 'list)
                           (lambda (x)
                             (setf (apply #'aref p x)
                                   (list* op
                                          (loop for a in args
                                                collect (read-element a x))))))
           (make-instance 'operation :element-type (op-type
                                                    op
                                                    (mapcar 'element-type args))
                                     :permutation p)))))))




;;; reduce ARGs along first dimension using OP, returns a matrix with
;;; 1 fewer dimension
(defun reduce-dimension (op a)
  (cond
    ;; if A is 0-d, error? maybe just return it?
    ((null (dimensions a))
     (error "can't reduce a scalar"))
   ;; if first dimension of A is a broadcast dimension, reshape it to
   ;; remaining dimensions with shape of remaining dimensions and
   ;; containing result of calling OP with 0 arguments? If A was 1d,
   ;; result is 0d (scalar)? (not sure if 1d broadcast should be valid
   ;; since it is just a scalar? allowing for now, but need to think
   ;; if it is reasonable or not)
    ((is-broadcast-dimension a 0)
     (let ((vars (loop for i below (length (dimensions a))
                       collect (gensym (format nil "V~d-" i)))))
       (reshape* a :from vars :to (cdr vars))))

    ;; if first dimension of A is 1, return a slice of A at dim1=0
    ((= 1 (car (dimensions a)))
     (slice a 0 0))

    ;; if first dimension of A is 2, return result of per-element on
    ;; slices at 0 and 1, otherwise repeat starting from that and call
    ;; per-element on result and each remaining slice of A, returning
    ;; the final result
    (t
     (let ((acc (per-element op (slice a 0 0) (slice a 0 1))))
       (loop for i from 2 below (car (dimensions a))
             do (setf acc (per-element op acc (slice a 0 i))))
       acc))))
