(in-package #:3b-mmath)


;;; special case of permutation matrix where some dimensions are 0,
;;; which is treated as containing same values for all possible values
;;; of that dimension.


(defclass broadcast (permutation)
  ;; we store it internally as a permutation with 1 in broadcast
  ;; dimensions, so we need a separate copy of the intended dimensions
  ;; with 0s preserved
  ((dimensions :reader dimensions :initarg :dimensions)))

(defmethod is-broadcast-dimension (p n) nil)

(defmethod is-broadcast-dimension ((b broadcast) n)
  (zerop (nth n (dimensions b))))

#++
(defun make-broadcast-permutation (dimensions base)
  (assert (some 'zerop dimensions))
  (assert (notany 'zerop (array-dimensions permutation)))
  (let ((pd (array-dimensions permutation)))
    (assert (= (length pd) (length dimensions)))
    (loop for d in dimensions for p in pd
          do (assert (or (= p d) (and (= p 1) (= d 0)))))
    (make-instance 'broadcast :base base :dimensions dimensions
                   permutation permutation)))

;; only way to create a broadcast permutation is with reshape?
(defun reshape* (m &key from to)
  (flet ((has-duplicates (x)
           (let ((n (make-hash-table)))
             (loop for i in x
                     thereis (gethash i n)
                   when (symbolp i) do (setf (gethash i n) i)))))
    (let ((name-map (make-hash-table))
          (broadcast (find 0 to))
          (dims nil))
      (assert (= (length from) (length (dimensions m))))
      (loop for i from 0
            for n in from
            do (assert (symbolp n))
               (setf (gethash n name-map) i))
      (assert (not (has-duplicates from)))
      (assert (not (has-duplicates to)))
      (loop for f in from
            for i from 0
            do (assert (or (member f to) (is-broadcast-dimension m i))))
      (setf dims
            (loop for o in to
                  do (assert (or (eql 0 o) (gethash o name-map)))
                  collect (if (symbolp o)
                              (nth (gethash o name-map)
                                   (dimensions m))
                              o)))
      (let* ((dim* (subst 1 0 dims))
             (p (make-array dim* :initial-element nil)))
        (map-dimensions dim*
                        (lambda (a)
                          (let ((n (make-array (length (dimensions m)))))
                            (loop for i in a
                                  for d in to
                                  unless (numberp d)
                                    do (setf (aref n (gethash d name-map))
                                             i))
                            (setf (apply #'aref p a)
                                  (coerce n 'list)))))

        (if broadcast
            (make-instance 'broadcast :dimensions dims
                                      :base m :permutation p)
            (make-instance 'permutation :base m :permutation p))))))

(defmacro reshape (m &rest spec)
  ;; spec = :from s1 s2 s3 ... :to d1 d2 ...

  ;; where S* are symbols used to name input dimensions, and d* are
  ;; symbols from S* or 0 for a broadcast dimension

  ;; so for example a transpose could be (reshape x :from m n :to n m)

  ;; if any symbol from S* is missing in D* it must be a broadcast
  ;; dimension in input accessor
  (assert (eql :from (car spec)))
  (assert (= 1 (count :from spec)))
  (assert (= 1 (count :to spec)))
  (let ((from nil)
        (to nil))
    (loop with s = nil
          for i in spec
          do (assert (or (symbolp i) (zerop i)))
          do (cond
               ((eql i :from) (setf s :from))
               ((eql i :to) (setf s :to))
               ((eql s :from) (push i from))
               ((eql s :to) (push i to))))
    (setf from (nreverse from))
    (setf to (nreverse to))
    `(reshape* ,m :from ',from :to ',to)))

(defun is-broadcast (p)
  (typep p 'broadcast))

(defmethod read-element ((b broadcast) index &key)
  (let ((d (loop for d in (dimensions b)
                 for i in index
                 if (= d 0) collect 0 else collect i)))
    (call-next-method b d)))
