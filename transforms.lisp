(in-package #:3b-mmath)


;; generic permutations, not sure if these need a separate class or not?

(defun %make-permutation (dims base)
  ;; make a possibly broadcast permutation with unspecified contents,
  ;; to be filled by caller
  (let* ((dim* (subst 1 0 dims))
         (p (make-array dim* :initial-element nil)))
    (if (find 0 dims)
        (make-instance 'broadcast :dimensions dims
                                  :base base :permutation p)
        (make-instance 'permutation :base base :permutation p))))

(defun slice (accessor dimension index)
  ;; return accessor for (N-1)d matrix corresponding to fixed INDEX
  ;; along DIMENSION. Ex: if A is a 2d matrix, (slice a 0 0) would be
  ;; first row, (slice 1 2) would be 3rd column, etc. if B is 3d,
  ;; (slice b 2 0) would be bottom layer
  (assert (<= 0 dimension (1- (length (dimensions accessor)))))
  (unless (is-broadcast-dimension accessor dimension)
    (assert (<= 0 index (1- (nth dimension (dimensions accessor))))))
  (let* ((dims (loop for i from 0 for d in (dimensions accessor)
                     unless (= i dimension) collect d))
         (p (%make-permutation dims accessor))
         (a (permutation p)))
    (flet ((i (i)
             (append (subseq i 0 dimension)
                     (list index)
                     (subseq i dimension))))
      (map-dimensions (subst 1 0 dims)
                      (lambda (i)
                        (setf (apply #'aref a i) (i i)))))
    p))
