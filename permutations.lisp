(in-package #:3b-mmath)


;;; permutation accessors, including storage -> matrix

;;; permutation matrices store things as N-d matrices, and indices
;;; into them, where indices are always in same order as math notation.

;;; so first dimension is row (= Y), second is column (= X), third
;;; is Z, etc.

;;; a 4x1 column matrix will look like #((0) (1) (2) (3)), and a 1x4
;;; row matrix would be #((0 1 2 3))



(defclass permutation ()
  ((base :reader base :initarg :base)
   ;; should be an N-d array, not sure if it is worth storing
   ;; dimensions separately or not?
   (permutation :reader permutation :initarg :permutation)))

(defmethod dimensions ((p permutation))
  (array-dimensions (permutation p)))

(defmethod element-type ((p permutation))
  (element-type (base p)))

#++
(defmethod size ((p permutation))
  (size (base p)))

(defmethod read-element ((p permutation) indices &key)
  (read-element (base p)
                ;; not sure about this API that requires putting
                ;; indices in a list... a bit annoying when writing
                ;; tests, but not sure if "real" code will usually
                ;; have lists or not
                (apply 'aref (permutation p) indices)))

(defmethod write-element (n (p permutation) indices &key type)
  (write-element n
                 (base p)
                 (apply 'aref (permutation p) indices)
                 :type type))

;;; translate linear storage to an N-d matrix


(defclass matrix (permutation)
  ;; not sure if this subclass is actually needed?

  ())


(defun map-dimensions (dimensions function)
  (labels ((r (index dims)
             (loop for i below (max 1 (car dims))
                   when (cdr dims)
                     do (r (cons i index) (cdr dims))
                   else do (funcall function (cons i index)))))
    (if dimensions
        (r nil (reverse dimensions))
        (funcall function nil))))

(defun identity-permutation (base &rest dimensions)
  ;; probably not useful, but just here for testing
  (let ((size (reduce '* dimensions))
        (a (make-array dimensions)))
    ;; for now, assuming size of storage matches size of matrix
    (assert (= size (size base)))
    (assert (equalp dimensions (dimensions base)))
    (map-dimensions dimensions (lambda (c)
                                 (setf (apply #'aref a c)
                                       c)))
    (make-instance 'matrix :base base :permutation a)))

(defun column-major-matrix (base &rest dimensions)
  (let ((size (reduce '* dimensions))
        (a (make-array dimensions)))
    ;; for now, assuming size of storage matches size of matrix
    (assert (= size (size base)))
    (labels ((nd (i d)
               (when d
                 (multiple-value-bind (x r)
                     (floor i (car d))
                   (cons r (nd x (cdr d)))))))
      (loop for i below size
            for c = (nd i dimensions)
            do (setf (apply #'aref a c) i)))
    (make-instance 'matrix :base base :permutation a)))

(defun row-major-matrix (base &rest dimensions)
  (let ((size (reduce '* dimensions))
        (a (make-array dimensions)))
    ;; for now, assuming size of storage matches size of matrix
    (assert (= size (size base)))
    (labels ((nd (i d)
               (when d
                 (multiple-value-bind (x r)
                     (floor i (car d))
                   (cons r (nd x (cdr d)))))))
      (loop for d = (reverse dimensions)
            for i below size
            for c = (nd i d)
            do (setf (apply #'aref a (reverse c)) i)))
    (make-instance 'matrix :base base :permutation a)))

(defun make-literal-matrix (element-type dimensions &rest elements)
  (let ((s (make-storage-literal* element-type elements)))
    (apply 'row-major-matrix s dimensions)))


