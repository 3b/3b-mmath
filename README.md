### Experimental meta-math-library for computer graphics

(not currently ready for use, but some things work)

It is a "meta library" in the sense that it doesn't directly have
functions for the usual operations like matrix multiply, instead it
has macros for generating multiple variants of those functions. It is
mostly intended for graphics (up to 4x4 matrices), so completely
unrolls all operations. It might eventually gain ability to handle
large matrices more reasonably, but existing libraries probably handle
that better.

For example, the macros allow generating an unrolled and partially
optimized matrix multiply with sources and destination in any of:
(not all done yet)

* the first MxN elements of a CL vector
* the MxN elements of a CL vector at specified offset
* a new MxN CL vector
* MxN elements at arbitrary position in a CL octet vector
* MxN elements at arbitrary location relative to a CFFI pointer
* A set of slots in a struct
* MxN values (numbers or variables)
* submatrix, slices, diagonal, or arbitrary permutation of a larger matrix
* A single value replicated to all elements (source only)
* An X,Y, or Z rotation matrix with a specified angle (source only)

some examples:

```lisp
;; define a 4x4, single float, column-major matrix type
(defconstant m4 (intern-matrix-type 4 4 :type 'single-float :row-major nil))
;; and a double-flat version
(defconstant m4d (intern-matrix-type 4 4 :type double-float :row-major nil))
;; and corresponding CL type
(deftype m4 () '(simple-array single-float (16)))
(deftype m4d () '(simple-array double-float (16)))

;; create a function to multiply 2 of those into a new matrix

(defun m4* (a b)
  (declare (type m4 a b) (optimize speed))
  (matrix-mult (ag:alloc m4) (ag:vec m4 a) (ag:vec m4 b)))

(defun m4d* (a b)
  (declare (type m4d a b) (optimize speed))
  (matrix-mult (ag:alloc m4d) (ag:vec m4d a) (ag:vec m4d b)))

;; create a function to multiply into an existing vector

(defun m4*! (d a b)
  (declare (type m4 a b d) (optimize speed))
  (matrix-mult (ag:vec m4 d) (ag:vec m4 a) (ag:vec m4 b)))

;; create a function to multiply into a particular location in an
;; octet-vector
(defun m4*? (d a b offset)
  (declare (type m4 a b)
           ;; restricted range a bit to avoid consing in bounds checks
           (type (unsigned-byte 60) offset)
           (type (simple-array (unsigned-byte 8) 1) d)
           (optimize speed))
  (matrix-mult (ag:bvec m4 d :ofs offset)
                  (ag:vec m4 a)
                  (ag:vec m4 b)))

;; create a function to rotate a matrix around X axis by A radians,
;; into existing matrix

(defun m4rx! (d m a)
  (declare (type m4 m d) (single-float a) (optimize speed))
  (matrix-mult (ag:vec m4 d)
               (ag:vec m4 m)
               (ag:literal/rot m4 a :x)))

;; create a function to multiply X by a specific matrix
(defun foo (x w)
  (declare (type m4 x) (single-float w) (optimize speed))
  (matrix-mult (ag:alloc m4)
               (ag:vec m4 x)
               (ag:literal m4
                           0 2 0 6
                           3 1 4 1
                           0 2 0 2
                           0 0 0 w)))

;; expansion of MATRIX-MULT for m4rx!

;; (the `COERCE` are optimized out on SBCL since it knows they are
;;  already that type, but would let it work if the inputs and output were
;;  different types)

(LET ((#:C0 (COS A)) (#:S1 (SIN A)))
  (ASSERT (ARRAY-IN-BOUNDS-P M 15))
  NIL
  (ASSERT (ARRAY-IN-BOUNDS-P D 15))
  (LOCALLY
   (DECLARE (OPTIMIZE (SB-C::INSERT-ARRAY-BOUNDS-CHECKS 0)))
   (SETF (AREF D 0) (COERCE (AREF M 0) 'SINGLE-FLOAT))
   (SETF (AREF D 4)
           (COERCE (+ (* (AREF M 4) #:C0) (* (AREF M 8) #:S1)) 'SINGLE-FLOAT))
   (SETF (AREF D 8)
           (COERCE (+ (* (AREF M 4) (- #:S1)) (* (AREF M 8) #:C0))
                   'SINGLE-FLOAT))
   (SETF (AREF D 12) (COERCE (AREF M 12) 'SINGLE-FLOAT))
   (SETF (AREF D 1) (COERCE (AREF M 1) 'SINGLE-FLOAT))
   (SETF (AREF D 5)
           (COERCE (+ (* (AREF M 5) #:C0) (* (AREF M 9) #:S1)) 'SINGLE-FLOAT))
   (SETF (AREF D 9)
           (COERCE (+ (* (AREF M 5) (- #:S1)) (* (AREF M 9) #:C0))
                   'SINGLE-FLOAT))
   (SETF (AREF D 13) (COERCE (AREF M 13) 'SINGLE-FLOAT))
   (SETF (AREF D 2) (COERCE (AREF M 2) 'SINGLE-FLOAT))
   (SETF (AREF D 6)
           (COERCE (+ (* (AREF M 6) #:C0) (* (AREF M 10) #:S1)) 'SINGLE-FLOAT))
   (SETF (AREF D 10)
           (COERCE (+ (* (AREF M 6) (- #:S1)) (* (AREF M 10) #:C0))
                   'SINGLE-FLOAT))
   (SETF (AREF D 14) (COERCE (AREF M 14) 'SINGLE-FLOAT))
   (SETF (AREF D 3) (COERCE (AREF M 3) 'SINGLE-FLOAT))
   (SETF (AREF D 7)
           (COERCE (+ (* (AREF M 7) #:C0) (* (AREF M 11) #:S1)) 'SINGLE-FLOAT))
   (SETF (AREF D 11)
           (COERCE (+ (* (AREF M 7) (- #:S1)) (* (AREF M 11) #:C0))
                   'SINGLE-FLOAT))
   (SETF (AREF D 15) (COERCE (AREF M 15) 'SINGLE-FLOAT)))
  D)

;; expansion of the MATRIX-MULT macro for m4*:

(LET ((#:G878 (MAKE-ARRAY 16 :ELEMENT-TYPE 'SINGLE-FLOAT :INITIAL-ELEMENT 0.0)))
  (ASSERT (ARRAY-IN-BOUNDS-P A 15))
  (ASSERT (ARRAY-IN-BOUNDS-P B 15))
  (ASSERT (ARRAY-IN-BOUNDS-P #:G878 15))
  (LOCALLY
   (DECLARE (OPTIMIZE (SB-C::INSERT-ARRAY-BOUNDS-CHECKS 0)))
   (SETF (AREF #:G878 0)
           (COERCE
            (+ (* (AREF A 0) (AREF B 0)) (* (AREF A 4) (AREF B 1))
               (* (AREF A 8) (AREF B 2)) (* (AREF A 12) (AREF B 3)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 4)
           (COERCE
            (+ (* (AREF A 0) (AREF B 4)) (* (AREF A 4) (AREF B 5))
               (* (AREF A 8) (AREF B 6)) (* (AREF A 12) (AREF B 7)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 8)
           (COERCE
            (+ (* (AREF A 0) (AREF B 8)) (* (AREF A 4) (AREF B 9))
               (* (AREF A 8) (AREF B 10)) (* (AREF A 12) (AREF B 11)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 12)
           (COERCE
            (+ (* (AREF A 0) (AREF B 12)) (* (AREF A 4) (AREF B 13))
               (* (AREF A 8) (AREF B 14)) (* (AREF A 12) (AREF B 15)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 1)
           (COERCE
            (+ (* (AREF A 1) (AREF B 0)) (* (AREF A 5) (AREF B 1))
               (* (AREF A 9) (AREF B 2)) (* (AREF A 13) (AREF B 3)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 5)
           (COERCE
            (+ (* (AREF A 1) (AREF B 4)) (* (AREF A 5) (AREF B 5))
               (* (AREF A 9) (AREF B 6)) (* (AREF A 13) (AREF B 7)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 9)
           (COERCE
            (+ (* (AREF A 1) (AREF B 8)) (* (AREF A 5) (AREF B 9))
               (* (AREF A 9) (AREF B 10)) (* (AREF A 13) (AREF B 11)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 13)
           (COERCE
            (+ (* (AREF A 1) (AREF B 12)) (* (AREF A 5) (AREF B 13))
               (* (AREF A 9) (AREF B 14)) (* (AREF A 13) (AREF B 15)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 2)
           (COERCE
            (+ (* (AREF A 2) (AREF B 0)) (* (AREF A 6) (AREF B 1))
               (* (AREF A 10) (AREF B 2)) (* (AREF A 14) (AREF B 3)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 6)
           (COERCE
            (+ (* (AREF A 2) (AREF B 4)) (* (AREF A 6) (AREF B 5))
               (* (AREF A 10) (AREF B 6)) (* (AREF A 14) (AREF B 7)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 10)
           (COERCE
            (+ (* (AREF A 2) (AREF B 8)) (* (AREF A 6) (AREF B 9))
               (* (AREF A 10) (AREF B 10)) (* (AREF A 14) (AREF B 11)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 14)
           (COERCE
            (+ (* (AREF A 2) (AREF B 12)) (* (AREF A 6) (AREF B 13))
               (* (AREF A 10) (AREF B 14)) (* (AREF A 14) (AREF B 15)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 3)
           (COERCE
            (+ (* (AREF A 3) (AREF B 0)) (* (AREF A 7) (AREF B 1))
               (* (AREF A 11) (AREF B 2)) (* (AREF A 15) (AREF B 3)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 7)
           (COERCE
            (+ (* (AREF A 3) (AREF B 4)) (* (AREF A 7) (AREF B 5))
               (* (AREF A 11) (AREF B 6)) (* (AREF A 15) (AREF B 7)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 11)
           (COERCE
            (+ (* (AREF A 3) (AREF B 8)) (* (AREF A 7) (AREF B 9))
               (* (AREF A 11) (AREF B 10)) (* (AREF A 15) (AREF B 11)))
            'SINGLE-FLOAT))
   (SETF (AREF #:G878 15)
           (COERCE
            (+ (* (AREF A 3) (AREF B 12)) (* (AREF A 7) (AREF B 13))
               (* (AREF A 11) (AREF B 14)) (* (AREF A 15) (AREF B 15)))
            'SINGLE-FLOAT)))
  #:G878)

;;; expansion for m4*?

(LET ()
  (ASSERT (ARRAY-IN-BOUNDS-P A 15))
  (ASSERT (ARRAY-IN-BOUNDS-P B 15))
  (PROGN
   (ASSERT (ARRAY-IN-BOUNDS-P D OFFSET))
   (ASSERT (ARRAY-IN-BOUNDS-P D (+ 63 OFFSET))))
  (LOCALLY
   (DECLARE (INLINE NIBBLES:IEEE-SINGLE-REF/LE NIBBLES::IEEE-SINGLE-SET/LE)
            (OPTIMIZE (SB-C::INSERT-ARRAY-BOUNDS-CHECKS 0)))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D OFFSET)
           (COERCE
            (+ (* (AREF A 0) (AREF B 0)) (* (AREF A 4) (AREF B 1))
               (* (AREF A 8) (AREF B 2)) (* (AREF A 12) (AREF B 3)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 16 OFFSET))
           (COERCE
            (+ (* (AREF A 0) (AREF B 4)) (* (AREF A 4) (AREF B 5))
               (* (AREF A 8) (AREF B 6)) (* (AREF A 12) (AREF B 7)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 32 OFFSET))
           (COERCE
            (+ (* (AREF A 0) (AREF B 8)) (* (AREF A 4) (AREF B 9))
               (* (AREF A 8) (AREF B 10)) (* (AREF A 12) (AREF B 11)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 48 OFFSET))
           (COERCE
            (+ (* (AREF A 0) (AREF B 12)) (* (AREF A 4) (AREF B 13))
               (* (AREF A 8) (AREF B 14)) (* (AREF A 12) (AREF B 15)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 4 OFFSET))
           (COERCE
            (+ (* (AREF A 1) (AREF B 0)) (* (AREF A 5) (AREF B 1))
               (* (AREF A 9) (AREF B 2)) (* (AREF A 13) (AREF B 3)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 20 OFFSET))
           (COERCE
            (+ (* (AREF A 1) (AREF B 4)) (* (AREF A 5) (AREF B 5))
               (* (AREF A 9) (AREF B 6)) (* (AREF A 13) (AREF B 7)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 36 OFFSET))
           (COERCE
            (+ (* (AREF A 1) (AREF B 8)) (* (AREF A 5) (AREF B 9))
               (* (AREF A 9) (AREF B 10)) (* (AREF A 13) (AREF B 11)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 52 OFFSET))
           (COERCE
            (+ (* (AREF A 1) (AREF B 12)) (* (AREF A 5) (AREF B 13))
               (* (AREF A 9) (AREF B 14)) (* (AREF A 13) (AREF B 15)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 8 OFFSET))
           (COERCE
            (+ (* (AREF A 2) (AREF B 0)) (* (AREF A 6) (AREF B 1))
               (* (AREF A 10) (AREF B 2)) (* (AREF A 14) (AREF B 3)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 24 OFFSET))
           (COERCE
            (+ (* (AREF A 2) (AREF B 4)) (* (AREF A 6) (AREF B 5))
               (* (AREF A 10) (AREF B 6)) (* (AREF A 14) (AREF B 7)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 40 OFFSET))
           (COERCE
            (+ (* (AREF A 2) (AREF B 8)) (* (AREF A 6) (AREF B 9))
               (* (AREF A 10) (AREF B 10)) (* (AREF A 14) (AREF B 11)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 56 OFFSET))
           (COERCE
            (+ (* (AREF A 2) (AREF B 12)) (* (AREF A 6) (AREF B 13))
               (* (AREF A 10) (AREF B 14)) (* (AREF A 14) (AREF B 15)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 12 OFFSET))
           (COERCE
            (+ (* (AREF A 3) (AREF B 0)) (* (AREF A 7) (AREF B 1))
               (* (AREF A 11) (AREF B 2)) (* (AREF A 15) (AREF B 3)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 28 OFFSET))
           (COERCE
            (+ (* (AREF A 3) (AREF B 4)) (* (AREF A 7) (AREF B 5))
               (* (AREF A 11) (AREF B 6)) (* (AREF A 15) (AREF B 7)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 44 OFFSET))
           (COERCE
            (+ (* (AREF A 3) (AREF B 8)) (* (AREF A 7) (AREF B 9))
               (* (AREF A 11) (AREF B 10)) (* (AREF A 15) (AREF B 11)))
            'SINGLE-FLOAT))
   (SETF (NIBBLES:IEEE-SINGLE-REF/LE D (+ 60 OFFSET))
           (COERCE
            (+ (* (AREF A 3) (AREF B 12)) (* (AREF A 7) (AREF B 13))
               (* (AREF A 11) (AREF B 14)) (* (AREF A 15) (AREF B 15)))
            'SINGLE-FLOAT)))
  NIL)

```
