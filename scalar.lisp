(in-package #:3b-mmath)

;;; 'scalar' matrix accessor(s)

;;; main representation of a scalar is a 0-d accessor containing a
;;; single value, but might also accept N-d accessor with all
;;; dimensions = 1 (or 0)?

(defclass scalar (permutation)
  ())

(defun make-scalar (element-type value)
  (let ((s (make-storage-literal element-type value)))
    (make-instance 'scalar :base s :permutation #0a0)))

(defmethod read-element ((p scalar) index &key)
  (read-element (base p) (aref (permutation p))))

(defun is-scalar (a)
  (and (typep a 'permutation)
       (let ((d (dimensions a)))
         (or (eql nil d)
             (loop for i in d always (<= 0 i 1))))))

(defun scalar-value (a)
  (assert (is-scalar a))
  (let ((d (dimensions a)))
    (if d
        (read-element a (make-list (length d) :initial-element 0))
        (read-element a ()))))

(defun scalar?-value (a)
  (if (is-scalar a)
      (scalar-value a)
      a))


