(in-package #:3b-mmath/accessor-generator)

(defun permutation-designator (x)
  (etypecase x
    ((array * 2)
     x)
    (matrix-type
     (matrix-type-permutation (matrix-type-designator x)))
    (accessor
     (permute x))))

(defun permute/sub (stype i j prev transpose)
  (setf prev (permutation-designator prev))
  (let* ((sr (matrix-type-rows stype))
         (sc (matrix-type-columns stype))
         (r (array-dimension prev 0))
         (c (array-dimension prev 1))
         (v (make-array (list sr sc) :initial-element 0)))
    (when transpose
      (rotatef sr sc))
    (unless (and (<= sr (- r i))
                 (<= sc (- c j))
                 (>= i 0)
                 (>= j 0))
      (error "can't take ~sx~s submatrix at ~s,~s from ~@[~*(tranposed) ~]~
              ~sx~s matrix"
             (matrix-type-rows stype)
             (matrix-type-columns stype)
             i j
             transpose
             (array-dimension prev 0)
             (array-dimension prev 1)))
    (loop
      for i2 below sr
      do (loop
           for j2 below sc
           for x = (aref prev (+ i i2) (+ j j2))
           do (if transpose
                  (setf (aref v j2 i2) x)
                  (setf (aref v i2 j2) x))))
    v))

(defun permute/slice (stype imask jmask prev)
  " generate a permutation for arbitrary submatrix of PREV

IMASK and JMASK should be integers or bitvectors matching
corresponding dimensions of TYPE, with 1 bit indicating to include
that row/column. (Note that bit 0 of the mask is the first row/column,
so the bits in #* are in opposite order from #b

for example:

i=#*0101 j=#*1111 would access the first and third rows of a 4x4
matrix as if they were a 2x4 matrix.

i=#x*1010 j=#*1010 would acccess elements of a 4x4 marked with X below

X . X .
. . . .
X . X .
.......

as a 2x2 matrix

i=#b11 j=#b11 would be equivalent to 2x2 submatrix at offset 0,0
"
  (setf prev (permutation-designator prev))
  (labels ((mbit (i m)
             ;; accept bitvector or int for masks
             (if (typep m 'bit-vector)
                 (plusp (aref m i))
                 (logbitp i m)))
           (v (x) (coerce x 'vector))
           (mask (mask r)
             (v (loop for i below r
                      when (mbit i mask)
                        collect i))))
    (let* ((sr (matrix-type-rows stype))
           (sc (matrix-type-columns stype))
           (r (array-dimension prev 0))
           (c (array-dimension prev 1))
           (v (make-array (list sr sc) :initial-element 0))
           (imap (mask imask r))
           (jmap (mask jmask c)))
      (unless (and (<= sr r)
                   (<= sc c))
        (error "can't take ~sx~s slice submatrix from ~@[~*(transposed) ~]~sx~s matrix"
               (matrix-type-rows stype)
               (matrix-type-columns stype)
               nil
               (array-dimension prev 0)
               (array-dimension prev 1)))
      (unless (and (= (length imap) sr)
                   (= (length jmap) sc))
        (error "can't write ~sx~s slice into ~sx~s submatrix"
               (length imap) (length jmap) sr sc))


      (loop
        for i below sr
        do (loop
             for j below sc
             for x = (aref prev (aref imap i) (aref jmap j))
             do (setf (aref v i j) x)))
      v)))

(defun permute/diag (stype prev antidiagonal)
  (setf prev (permutation-designator prev))
  (let* ((sr (matrix-type-rows stype))
         (sc (matrix-type-columns stype))
         (r (array-dimension prev 0))
         (c (array-dimension prev 1))
         (v (make-array (list sr sc) :initial-element 0)))
    (unless (and (<= (max sr sc) (min r c)))
      (error "can't take ~sx~s diagonal from ~sx~s matrix"
             sr sc r c))
    (assert (or (= sr 1) (= sc 1)))
    (loop
      for i below (max sr sc)
      for x = (aref prev
                    i
                    (if antidiagonal
                        (- c i 1)
                        i))
      do (if (> sr sc)
             (setf (aref v i 0) x)
             (setf (aref v 0 i) x)))
    v))

(defun permute/transpose (stype prev)
  (setf prev (permutation-designator prev))
  (let* ((sr (matrix-type-rows stype))
         (sc (matrix-type-columns stype))
         (r (array-dimension prev 0))
         (c (array-dimension prev 1))
         (v (make-array (list sr sc) :initial-element 0)))
    (unless (and (<= sr c)
                 (<= sc r))
      (error "can't tranpose ~sx~s matrix into ~sx~s"
             r c sr sc))
    (loop for i below sr
          do (loop for j below sc
                   do (setf (aref v i j)
                            (aref prev j i))))
    v))



(defun submatrix (rows columns i j v)
  "return accessor for a contiguous ROWS x COLUMNS submatrix at offset
I,J from matrix represented by accessore designator V"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (stype (intern-matrix-type* rows columns base)))
    (make-permuted-accessor a stype
                            (permute/sub stype i j a nil))))

(defun row (I v)
  "return accessor for row I of matrix represented by accessore
designator V as a row vector"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (stype (intern-matrix-type* 1 (matrix-type-columns base)
                                     base)))
    (make-permuted-accessor a stype
                            (permute/sub stype i 0 a nil))))

(defun column (j v)
  "return accessor for column J of matrix represented by accessore
designator V as a column vector"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (stype (intern-matrix-type* (matrix-type-rows base) 1
                                     base)))
    (make-permuted-accessor a stype
                            (permute/sub stype 0 j a nil))))

(defun submatrix* (imask jmask v)
  "return accessor for an arbitrary submatrix of matrix represented by
accessore designator V, after removing rows corresponding to 0 in
IMASK and columns corresponding to 0 in JMASK (see permute/slice)"
  (flet ((cc (x default)
           (etypecase x
             (vector
              (count 1 x))
             ;; allow -1 as a shortcut for "keep all"
             ((eql -1)
              default)
             ((integer 1)
              (logcount x)))))
   (let* ((a (accessor-designator v))
          (base (mtype a))
          (stype (intern-matrix-type* (cc imask (matrix-type-rows base))
                                      (cc jmask (matrix-type-rows base))
                                      base
                                      :row-major nil)))
     (make-permuted-accessor a stype
                             (permute/slice stype imask jmask a)))))

(defun diagonal (v)
  "return accessor for diagonal of matrix represented by accessore
designator V as a column vector"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (c (min (matrix-type-rows base)
                 (matrix-type-columns base)))
         (stype (intern-matrix-type* c 1 base)))
    (make-permuted-accessor a stype
                            (permute/diag stype a nil))))

(defun antidiagonal (v)
  "return accessor for diagonal of matrix represented by accessore
designator V as a column vector"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (c (min (matrix-type-rows base)
                 (matrix-type-columns base)))
         (stype (intern-matrix-type* c 1 base)))
    (make-permuted-accessor a stype
                            (permute/diag stype a t))))

(defun transpose (v)
  "return accessor for tranpose of matrix represented by accessore
designator V"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (rows (matrix-type-columns base))
         (columns (matrix-type-rows base))
         (stype (intern-matrix-type* rows columns base)))
    (make-permuted-accessor a stype
                            (permute/transpose stype a))))

(defun bv1 (x)
  (make-array x :element-type 'bit :initial-element 1))

(defun remove-row+column (row column v)
  "return accessor for matrix V with ROW and COLUMN removed"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (rows (matrix-type-rows base))
         (columns (matrix-type-columns base))
         (stype (intern-matrix-type* (1- rows) (1- columns) base))
         (imask (bv1 rows))
         (jmask (bv1 columns)))
    (setf (aref imask row) 0)
    (setf (aref jmask column) 0)
    (make-permuted-accessor a stype
                            (permute/slice stype imask jmask a))))
