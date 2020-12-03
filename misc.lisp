(in-package #:3b-mmath/misc)

(defun canonical-endian (endian)
  (ecase endian
    (:native
     #-little-endian
     :big
     #+little-endian
     :little)
    ((:big :be) :big)
    ((:little :le) :little)))

(defun size-for-type (type)
  (macrolet ((c ((ctype size) &rest clauses)
               `(cond
                  ,@ (loop for (c s) in (list* (list ctype size)
                                               clauses)
                           collect `((equalp type ',c)
                                     ,s))
                     (t (error "unknown type ~s?" type)))))
    (c
     ((signed-byte 8) 1)
     ((signed-byte 16) 2)
     ((signed-byte 32) 4)
     ((signed-byte 64) 8)

     ((unsigned-byte 8) 1)
     ((unsigned-byte 16) 2)
     ((unsigned-byte 32) 4)
     ((unsigned-byte 64) 8)

     ((:half-float short-float) 2)
     (single-float 4)
     (double-float 8))))

(defun nibbles-accessor (type endian)
  (setf endian (canonical-endian endian))
  (flet ((end (big little)
           (ecase endian
             (:big big)
             (:little little))))
    (macrolet ((c ((ctype big little) &rest clauses)
                 (print
                  `(cond
                     ,@ (loop for (c b l) in (list* (list ctype big little)
                                                    clauses)
                              collect `((equalp type ',c)
                                        (end ',b ',l)))
                        (t (error "unknown type ~s?" type))))))
      (c
       ;; don't really need the 8 since they could use vec, but
       ;; including for completeness
       ((signed-byte 8) aref aref)
       ((signed-byte 16) nibbles:sb16ref/be nibbles:sb16ref/le)
       ((signed-byte 32) nibbles:sb32ref/be nibbles:sb32ref/le)
       ((signed-byte 64) nibbles:sb64ref/be nibbles:sb64ref/le)

       ((unsigned-byte 8) aref aref)
       ((unsigned-byte 16) nibbles:sb16ref/be nibbles:sb16ref/le)
       ((unsigned-byte 32) nibbles:sb32ref/be nibbles:sb32ref/le)
       ((unsigned-byte 64) nibbles:sb64ref/be nibbles:sb64ref/le)

       ((:half-float short-float) half-ref/be half-ref/le)
       (single-float nibbles:ieee-single-ref/be nibbles:ieee-single-ref/le)
       (double-float nibbles:ieee-double-ref/be nibbles:ieee-double-ref/le)))))

(defun nibbles-setter (type endian)
  (setf endian (canonical-endian endian))
  (flet ((end (big little)
           (ecase endian
             (:big big)
             (:little little))))
    (macrolet ((c ((ctype big little) &rest clauses)
                 (print
                  `(cond
                     ,@ (loop for (c b l) in (list* (list ctype big little)
                                                    clauses)
                              collect `((equalp type ',c)
                                        (end ',b ',l)))
                        (t (error "unknown type ~s?" type))))))
      (c
       ;; don't really need the 8 since they could use vec, but
       ;; including for completeness
       ((signed-byte 8) (setf aref) ((setf aref)))
       ((signed-byte 16) nibbles::sb16set/be nibbles::sb16set/le)
       ((signed-byte 32) nibbles::sb32set/be nibbles::sb32set/le)
       ((signed-byte 64) nibbles::sb64set/be nibbles::sb64set/le)

       ((unsigned-byte 8) (setf aref) (setf aref))
       ((unsigned-byte 16) nibbles::sb16set/be nibbles::sb16set/le)
       ((unsigned-byte 32) nibbles::sb32set/be nibbles::sb32set/le)
       ((unsigned-byte 64) nibbles::sb64set/be nibbles::sb64set/le)

       ((:half-float short-float) (setf half-ref/be) (setf half-ref/le))
       (single-float nibbles::ieee-single-set/be nibbles::ieee-single-set/le)
       (double-float nibbles::ieee-double-set/be nibbles::ieee-double-set/le)))))

(cffi:defctype half (:wrapper :uint16
                     :from-c float-features:bits-short-float
                     :to-c float-features:short-float-bits))

(defun ffi-type (type)
  (macrolet ((c ((ltype ffi) &rest clauses)
               `(cond
                  ,@ (loop for (l f) in (list* (list ltype) clauses)
                           collect `((equalp type ',l)
                                     ',f))
                     (t (error "unknown type ~s?" type)))))
      (c
       ((signed-byte 8) :int8)
       ((signed-byte 16) :int16)
       ((signed-byte 32) :int32)
       ((signed-byte 64) :int64)

       ((unsigned-byte 8) :uint8)
       ((unsigned-byte 16) :uint16)
       ((unsigned-byte 32) :uint32)
       ((unsigned-byte 64) :uint64)

       ((:half-float short-float) half)
       (single-float :float)
       (double-float :double))))



(deftype unsigned-fixnum () '(and fixnum unsigned-byte))

(defun %permute/major (r c row-major)
  (let ((a (make-array (list r c) :initial-element 0
                                  :element-type 'unsigned-fixnum)))
    (loop for i below r
          do (loop for j below c
                   for p = (if row-major
                               (+ j (* i c))
                               (+ i (* j r)))
                   do (setf (aref a i j) p)))
    a))

(defstruct matrix-type
  (rows 4 :type (and fixnum (integer 1)))
  (columns 4 :type (and fixnum (integer 1)))
  (type 'single-float :type symbol)
  (row-major nil :type (or t nil))
  (stride 4 :type (and fixnum (integer 1)))
  (permutation (%permute/major 4 4 nil) :type (simple-array unsigned-fixnum 2)))

;; possibly some of these should be stored directly in struct?
(defun matrix-type-elements (matrix-type)
  (* (matrix-type-rows matrix-type)
     (matrix-type-columns matrix-type)))

(defun matrix-type-octets (matrix-type)
  (* (matrix-type-elements matrix-type)
     (matrix-type-stride matrix-type)))



(defparameter *matrix-types* (make-hash-table :test 'equalp))

(defun intern-matrix-type (rows columns
                           &key (type 'single-float) (row-major nil)
                             (stride (if type (size-for-type type) 1)))
  (a:ensure-gethash (list rows columns type row-major stride)
                    *matrix-types*
                    (make-matrix-type :rows rows :columns columns
                                      :type type :row-major row-major
                                      :stride stride
                                      :permutation (%permute/major
                                                    rows columns row-major))))

;; nibbles-style accessors for half floats
(declaim (inline half-ref/le (setf half-ref/le)
                 half-ref/be (setf half-ref/be)))

(defun half-ref/le (vector index)
  (float-features:bits-short-float (nibbles:ub16ref/le vector index)))

(defun (setf half-ref/le) (new vector index)
  (setf (nibbles:ub16ref/le vector index)
        (float-features:short-float-bits new))
  new)

(defun half-ref/be (vector index)
  (float-features:bits-short-float (nibbles:ub16ref/be vector index)))

(defun (setf half-ref/be) (new vector index)
  (setf (nibbles:ub16ref/be vector index)
        (float-features:short-float-bits new))
  new)

;; TODO: this should probably go away once new permutation stuff is done
#++
(defun index (type i-row j-col &key (row-major nil row-major-p))
  "convert i,j index into linear index for matrix TYPE. Not really
intended for speed. Errors if I,J outside matrix shape."
  ;; possibly should include an optimized "index" function in the matrix?
  (let ((m (matrix-type-rows type))
        (n (matrix-type-columns type))
        (rm (if row-major-p
                row-major
                (matrix-type-row-major type))))
    (assert (<= -1 i-row m))
    (assert (<= -1 j-col n))
    (if rm
        (+ j-col (* i-row n))
        (+ i-row (* j-col m)))))

(defun index (type i-row j-col)
  "get (permuted) linear index of element I,J in type TYPE"
  (aref (matrix-type-permutation type) i-row j-col))




(defun all-of-type (type &rest r)
  (every (lambda (a) (typep a type)) r))

(defun accesses-overlap (a b)
  (and a b
       (destructuring-bind (av as ae) a
         (destructuring-bind (bv bs be) b
           (and (eql av bv)
                (or (<= as bs ae)
                    (<= as be ae)))))))

(defun matrix-type-designator (type)
  (etypecase type
    (matrix-type
     type)
    (symbol
     (matrix-type-designator (symbol-value type)))
    (cons
     (apply #'intern-matrix-type type))))
