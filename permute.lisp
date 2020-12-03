(in-package #:3b-mmath/accessor-generator)


(defun permute/sub (stype i j prev transpose)
  (etypecase prev
    ((array * 2))
    (matrix-type
     (setf prev (matrix-type-permutation (matrix-type-designator prev))))
    (accessor))
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

(defun permute/slice (stype imask jmask prev transpose)
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
  (unless (typep prev '(array * 2))
    (setf prev (matrix-type-permutation (matrix-type-designator prev))))
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
      (when transpose (rotatef sr sc))
      (unless (and (<= sr r)
                   (<= sc c))
        (error "can't take ~sx~s slice submatrix from ~@[~*(transposed) ~]~sx~s matrix"
               (matrix-type-rows stype)
               (matrix-type-columns stype)
               transpose
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
             do (if transpose
                    (setf (aref v j i) x)
                    (setf (aref v i j) x))))
      v)))

(defun permute/diag (stype prev antidiagonal)
  (unless (typep prev '(array * 2))
    (setf prev (matrix-type-permutation (matrix-type-designator prev))))
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
  (unless (typep prev '(array * 2))
    (setf prev (matrix-type-permutation (matrix-type-designator prev))))
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


#++
(defun vec/sub (stype i j type v &key (ofs 0) (base 0) (stride nil)
                                   (transpose nil))
  "Operate on submatrix of type STYPE at offset i,j in matrix of type
TYPE stored in CL vector V, optionally starting from index (+ OFS (*
BASE STRIDE)). Matrix is assumed to be tightly packed in V.

Returns an ACCESSOR object containing info needed to access the matrix"
  #++(setf type (matrix-type-designator type))
  #++(setf stype (matrix-type-designator stype))
  (let ((a (vec type v :ofs ofs :base base :stride stride)))
    (make-permuted-accessor a stype (permute/sub stype i j type transpose))))
#++
(defun vec/slice (stype imask jmask type v
                  &key (ofs 0) (base 0) (stride nil) (transpose nil))
  "Operate on matrix of type STYPE containing intersection of
specified rows/columns of matrix of type TYPE stored in CL vector V,
optionally starting from index (+ OFS (* BASE STRIDE)). Matrix is
assumed to be tightly packed in V.

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

Returns an ACCESSOR object containing info needed to access the matrix"

  (let ((a (vec type v :ofs ofs :base base :stride stride)))
    (make-permuted-accessor a stype
                            (permute/slice stype imask jmask type transpose))))
#++
(defun vec/diagonal (stype type v
                     &key (ofs 0) (base 0) (stride nil) (anti nil))
  "Operate on diagonal or antidiagonal of a matrix of type STYPE,
stored as a vector of type TYPE)

Returns an ACCESSOR object containing info needed to access the matrix"
  (make-permuted-accessor (vec type v :ofs ofs :base base :stride stride)
                          stype
                          (permute/diag stype type anti)))
#++
(defun vec/transpose (stype type v
                      &key (ofs 0) (base 0) (stride nil))
  "Operate on matrix TYPE containing tranpose of matrix of type TYPE
  stored in vector V

Returns an ACCESSOR object containing info needed to access the matrix"
  (make-permuted-accessor (vec type v :ofs ofs :base base :stride stride)
                          stype
                          (permute/transpose stype type)))



(defun submatrix (rows columns i j v)
  "return accessor for a contiguous ROWS x COLUMNS submatrix at offset
I,J from matrix represented by accessore designator V"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (stype (intern-matrix-type* rows columns base)))
    (make-permuted-accessor a stype
                            (permute/sub stype i j base nil))))

(defun row (I v)
  "return accessor for row I of matrix represented by accessore
designator V as a row vector"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (stype (intern-matrix-type* 1 (matrix-type-columns base)
                                     base)))
    (make-permuted-accessor a stype
                            (permute/sub stype i 0 base nil))))

(defun column (j v)
  "return accessor for column J of matrix represented by accessore
designator V as a column vector"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (stype (intern-matrix-type* (matrix-type-rows base) 1
                                     base)))
    (make-permuted-accessor a stype
                            (permute/sub stype 0 j base nil))))

(defun submatrix* (imask jmask v)
  "return accessor for an arbitrary submatrix of matrix represented by
accessore designator V, after removing rows corresponding to 0 in
IMASK and columns corresponding to 0 in JMASK (see permute/slice)"
  (flet ((cc (x)
           (if (typep x 'vector)
               (count 1 x)
               (logcount x))))
   (let* ((a (accessor-designator v))
          (base (mtype a))
          (stype (intern-matrix-type* (cc imask) (cc jmask) base)))
     (make-permuted-accessor a stype
                             (permute/slice stype imask jmask base nil)))))

(defun diagonal (v)
  "return accessor for diagonal of matrix represented by accessore
designator V as a column vector"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (c (min (matrix-type-rows base)
                 (matrix-type-columns base)))
         (stype (intern-matrix-type* c 1 base)))
    (make-permuted-accessor a stype
                            (permute/diag stype base nil))))

(defun antidiagonal (v)
  "return accessor for diagonal of matrix represented by accessore
designator V as a column vector"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (c (min (matrix-type-rows base)
                 (matrix-type-columns base)))
         (stype (intern-matrix-type* c 1 base)))
    (make-permuted-accessor a stype
                            (permute/diag stype base t))))

(defun transpose (v)
  "return accessor for tranpose of matrix represented by accessore
designator V"
  (let* ((a (accessor-designator v))
         (base (mtype a))
         (rows (matrix-type-columns base))
         (columns (matrix-type-rows base))
         (stype (intern-matrix-type* rows columns base)))
    (make-permuted-accessor a stype
                            (permute/transpose stype base))))
