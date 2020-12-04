#++
(ql:quickload '(3b-mmath parachute))
(defpackage #:3b-mmath-test
  (:use #:cl #:parachute)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:ag #:3b-mmath/accessor-generator)
                    (#:m #:3b-mmath/matrix)
                    (#:mi #:3b-mmath/misc)
                    (#:l #:3b-mmath/lib)))

(in-package #:3b-mmath-test)

;; define some types

(defmacro defmtype (n type)
  `(a:define-constant ,n (mi:intern-matrix-type ,@type) :test 'equalp)
  )


(defun mv (r c &rest values)
  (let ((v (make-array (* r c) :element-type 'single-float)))
    (assert (= (length values) (* r c)))
    (loop for i below r
          do (loop for j below c
                   do (setf (aref v (+ i (* j r)))
                            (coerce
                             (or (elt values (+ j (* i c))) 0.0)
                             'single-float))))
    v))

(defmacro v= (f v)
  `(is equalp ,v ,f))

(defmtype m2 (2 2 :type 'single-float))
(defmtype m2x3 (2 3))
(defmtype m2x4 (2 4))
(defmtype m3x2 (3 2))
(defmtype m3 (3 3))
(defmtype m3x4 (3 4))
(defmtype m3x4r (3 4 :row-major t))
(defmtype m4 (4 4))
(defmtype m4x2 (4 2))
(defmtype m4x3 (4 3))
(defmtype m4x3r (4 3 :row-major t))
(defmtype m4n (4 4 :type nil))
(defmtype m4r (4 4 :row-major t))
(defmtype cv4 (4 1))
(defmtype m4d (4 4 :type 'double-float))
(defmtype v1 (1 1))
(defmtype v2 (2 1))
(defmtype v2d (2 1 :type 'double-float))
(defmtype v3 (3 1))
(defmtype v3r (1 3))
(defmtype v4 (4 1))
(defmtype v4r (1 4))
(defmtype v5 (5 1))
(defmtype v3r (1 3))

(defun 1+eps (n eps)
  (let ((x (float 1 eps)))
    (loop for i below n do (incf x eps))
    x))

(defun 1-eps (n eps)
  (let ((x (float 1 eps)))
    (loop for i below n do (decf x eps))
    x))


(define-test misc
  (true (mi:all-of-type 'fixnum 1 2 3 4))
  (false (mi:all-of-type 'fixnum 1 2.2 3 4))
  (true (mi:all-of-type 'symbol 'a 'b :c))


  (false (mi:accesses-overlap '(a 0 5) '(a 6 15)))
  (false (mi:accesses-overlap '(a 10 15) '(a 6 8)))
  (true (mi:accesses-overlap '(a 0 15) '(a 0 15)))
  (true (mi:accesses-overlap '(a 0 15) '(a 15 16)))
  (true (mi:accesses-overlap '(a 12 18) '(a 0 15)))
  (false (mi:accesses-overlap '(a 0 15) '(a 16 31)))

  (let ((a (make-array 2 :element-type '(unsigned-byte 8) :initial-element 0)))
    (declare (notinline mi:half-ref/le))
    (fail (mi:half-ref/le a 1))
    (is = 2.125s0 (setf (mi:half-ref/le a 0) 2.125s0))
    (is = 2.125s0 (mi:half-ref/le a 0))
    (is = -3.5s0 (setf (mi:half-ref/le a 0) -3.5s0))
    (is = -3.5s0 (mi:half-ref/le a 0)))

  (false (mi:ulp= 1.0 (1+eps 1 single-float-epsilon) :bits 0))
  (true (mi:ulp= 1.0 (1+eps 1 single-float-epsilon) :bits 1))
  (false (mi:ulp= 1.0 (1+eps 2 single-float-epsilon) :bits 1))
  (true (mi:ulp= 1.0 (1+eps 2 single-float-epsilon) :bits 2))
  (false (mi:ulp= 1.0 (1+eps 3 single-float-epsilon) :bits 2))
  (true (mi:ulp= 1.0 (1+eps 3 single-float-epsilon) :bits 3))

  (false (mi:ulp= 1.0 (1-eps 1 single-float-negative-epsilon) :bits 0))
  (true (mi:ulp= 1.0 (1-eps 1 single-float-negative-epsilon) :bits 1))
  (false (mi:ulp= 1.0 (1-eps 2 single-float-negative-epsilon) :bits 1))
  (true (mi:ulp= 1.0 (1-eps 2 single-float-negative-epsilon) :bits 2))
  (false (mi:ulp= 1.0 (1-eps 3 single-float-negative-epsilon) :bits 2))
  (true (mi:ulp= 1.0 (1-eps 3 single-float-negative-epsilon) :bits 3))


  (false (mi:ulp= 1d0 (1+eps 1 double-float-epsilon) :bits 0))
  (true (mi:ulp= 1d0 (1+eps 1 double-float-epsilon) :bits 1))
  (false (mi:ulp= 1d0 (1+eps 2 double-float-epsilon) :bits 1))
  (true (mi:ulp= 1d0 (1+eps 2 double-float-epsilon) :bits 2))
  (false (mi:ulp= 1d0 (1+eps 3 double-float-epsilon) :bits 2))
  (true (mi:ulp= 1d0 (1+eps 3 double-float-epsilon) :bits 3))

  (false (mi:ulp= 1d0 (1-eps 1 double-float-negative-epsilon) :bits 0))
  (true (mi:ulp= 1d0 (1-eps 1 double-float-negative-epsilon) :bits 1))
  (false (mi:ulp= 1d0 (1-eps 2 double-float-negative-epsilon) :bits 1))
  (true (mi:ulp= 1d0 (1-eps 2 double-float-negative-epsilon) :bits 2))
  (false (mi:ulp= 1d0 (1-eps 3 double-float-negative-epsilon) :bits 2))
  (true (mi:ulp= 1d0 (1-eps 3 double-float-negative-epsilon) :bits 3))

  ;; not sure if these should be ulp= or not?
  (true (mi:ulp= most-positive-single-float
                 sb-ext:single-float-positive-infinity))
  (true (mi:ulp= most-negative-single-float
                 sb-ext:single-float-negative-infinity))
  (true (mi:ulp= most-positive-double-float
                 sb-ext:double-float-positive-infinity))
  (true (mi:ulp= most-negative-double-float
                 sb-ext:double-float-negative-infinity)))

#++
(test 'misc)

(define-test matrix-type
  (is = 2 (mi:matrix-type-rows m2x3))
  (is = 3 (mi:matrix-type-columns m2x3))
  (is = 6 (mi:matrix-type-elements m2x3))
  (is eql 'single-float (mi:matrix-type-type m2x3))
  (is eql nil (mi:matrix-type-row-major m2x3))
  (is eql t (mi:matrix-type-row-major m3x4r))
  (is = 4 (mi:matrix-type-stride m2x3))
  (is eql 'double-float (mi:matrix-type-type m4d))
  (is = 8 (mi:matrix-type-stride m4d))
  (is = 128 (mi:matrix-type-octets m4d))


  ;; sbcl can't dump the struct from constant, so get it at runtime
  (let ((m2v (symbol-value 'm2)))
    (is eq m2v (mi:intern-matrix-type 2 2 :row-major nil))
    (is eq m2v (mi:intern-matrix-type 2 2 :type 'single-float))
    (v= (mi:matrix-type-permutation m2) #2A((0 2) (1 3)))
    (v= (mi:matrix-type-permutation m4x3r)
        #2a ((0 1 2) (3 4 5) (6 7 8) (9 10 11)))

    (is eq m2v (mi:matrix-type-designator m2))
    (is eq m2v (mi:matrix-type-designator 'm2))
    (is eq m2v (mi:matrix-type-designator '(2 2))))
)

#++(test 'matrix-type)

(define-test permute
  :depends-on (matrix-type))

(define-test (permute major)
  :depends-on (matrix-type)
  (v= (mi::%permute/major 2 2 nil) #2A((0 2) (1 3)))
  (v= (mi::%permute/major 2 2 t) #2A((0 1) (2 3)))
  (v= (mi::%permute/major 4 1 nil) #2a((0) (1) (2) (3)))
  (v= (mi::%permute/major 1 4 t) #2a((0 1 2 3)))
  (v= (mi::%permute/major 4 2 nil) #2A((0 4) (1 5) (2 6) (3 7)))
  (v= (mi::%permute/major 2 4 t) #2A((0 1 2 3) (4 5 6 7))))


(define-test (permute sub)
  :depends-on (matrix-type)
  ;; no negative offsets
  (fail (ag::permute/sub v1 -1 0 m3 nil))
  (fail (ag::permute/sub v1 1 -2 m3 nil))
  ;; submatrix can't be larger than target
  (fail (ag::permute/sub v4 0 0 m3 nil))
  (fail (ag::permute/sub v4 0 0 m3 t))
  (fail (ag::permute/sub m4 0 0 m3 nil))
  (fail (ag::permute/sub m4x3 0 0 m3x4 nil))
  (fail (ag::permute/sub m4x3 0 0 m4x3 t))
  ;; or extend outside it
  (fail (ag::permute/sub m3 2 2 m4 nil))
  (fail (ag::permute/sub m3 4 4 m4 nil))
  (fail (ag::permute/sub v1 4 4 m4 nil))
  ;; column submatrices of column-major matrices
  (v= (ag::permute/sub v4 0 0 m4 nil) #2a((0) (1) (2) (3)))
  (v= (ag::permute/sub v4 0 1 m4 nil) #2a((4) (5) (6) (7)))
  (v= (ag::permute/sub v4 0 2 m4 nil) #2a((8) (9) (10) (11)))
  (v= (ag::permute/sub v4 0 3 m4 nil) #2a((12) (13) (14) (15)))
  (v= (ag::permute/sub v3 0 0 m4 nil) #2a((0) (1) (2)))
  (v= (ag::permute/sub v3 0 1 m4 nil) #2a((4) (5) (6)))
  (v= (ag::permute/sub v3 0 2 m4 nil) #2a((8) (9) (10)))
  (v= (ag::permute/sub v3 0 3 m4 nil) #2a((12) (13) (14)))
  (v= (ag::permute/sub v3 1 1 m4 nil) #2a((5) (6) (7)))
  ;; column submatrices of column-major matrices as row-vectors
  (v= (ag::permute/sub v4r 0 0 m4 t) #2a((0 1 2 3)))
  (v= (ag::permute/sub v4r 0 1 m4 t) #2a((4 5 6 7)))
  (v= (ag::permute/sub v4r 0 2 m4 t) #2a((8 9 10 11)))
  (v= (ag::permute/sub v4r 0 3 m4 t) #2a((12 13 14 15)))
  (v= (ag::permute/sub v3r 0 0 m4 t) #2a((0 1 2)))
  (v= (ag::permute/sub v3r 0 1 m4 t) #2a((4 5 6)))
  (v= (ag::permute/sub v3r 0 2 m4 t) #2a((8 9 10)))
  (v= (ag::permute/sub v3r 0 3 m4 t) #2a((12 13 14)))
  (v= (ag::permute/sub v3r 1 1 m4 t) #2a((5 6 7)))

  ;; column submatrices of row-major matrices
  (v= (ag::permute/sub v4 0 0 m4r nil) #2a((0) (4) (8) (12)))
  (v= (ag::permute/sub v4 0 1 m4r nil) #2a((1) (5) (9) (13)))
  (v= (ag::permute/sub v4 0 2 m4r nil) #2a((2) (6) (10) (14)))
  (v= (ag::permute/sub v4 0 3 m4r nil) #2a((3) (7) (11) (15)))
  (v= (ag::permute/sub v3 0 0 m4r nil) #2a((0) (4) (8)))
  (v= (ag::permute/sub v3 0 1 m4r nil) #2a((1) (5) (9)))
  (v= (ag::permute/sub v3 0 2 m4r nil) #2a((2) (6) (10)))
  (v= (ag::permute/sub v3 0 3 m4r nil) #2a((3) (7) (11)))
  (v= (ag::permute/sub v3 1 3 m4r nil) #2a((7) (11) (15)))
  ;; column submatrices of row-major matrices as row-vectors
  (v= (ag::permute/sub v4r 0 0 m4r t) #2a((0 4 8 12)))
  (v= (ag::permute/sub v4r 0 1 m4r t) #2a((1 5 9 13)))
  (v= (ag::permute/sub v4r 0 2 m4r t) #2a((2 6 10 14)))
  (v= (ag::permute/sub v4r 0 3 m4r t) #2a((3 7 11 15)))
  (v= (ag::permute/sub v3r 0 0 m4r t) #2a((0 4 8)))
  (v= (ag::permute/sub v3r 0 1 m4r t) #2a((1 5 9)))
  (v= (ag::permute/sub v3r 0 2 m4r t) #2a((2 6 10)))
  (v= (ag::permute/sub v3r 0 3 m4r t) #2a((3 7 11)))
  (v= (ag::permute/sub v3r 1 3 m4r t) #2a((7 11 15)))

  ;; row submatrices of column-major matrix
  (v= (ag::permute/sub v4r 0 0 m4 nil) #2a((0 4 8 12)))
  (v= (ag::permute/sub v4r 1 0 m4 nil) #2a((1 5 9 13)))
  (v= (ag::permute/sub v4r 2 0 m4 nil) #2a((2 6 10 14)))
  (v= (ag::permute/sub v4r 3 0 m4 nil) #2a((3 7 11 15)))
  (v= (ag::permute/sub v3r 3 1 m4 nil) #2a((7 11 15)))
  ;; row submatrices of column-major matrix as column vectors
  (v= (ag::permute/sub v4 0 0 m4 t) #2a((0) (4) (8) (12)))
  (v= (ag::permute/sub v4 1 0 m4 t) #2a((1) (5) (9) (13)))
  (v= (ag::permute/sub v4 2 0 m4 t) #2a((2) (6) (10) (14)))
  (v= (ag::permute/sub v4 3 0 m4 t) #2a((3) (7) (11) (15)))
  (v= (ag::permute/sub v3 3 1 m4 t) #2a((7) (11) (15)))


  ;; row submatrices of row-major matrix
  (v= (ag::permute/sub v4r 0 0 m4r nil) #2a((0 1 2 3)))
  (v= (ag::permute/sub v4r 1 0 m4r nil) #2a((4 5 6 7)))
  (v= (ag::permute/sub v4r 2 0 m4r nil) #2a((8 9 10 11)))
  (v= (ag::permute/sub v4r 3 0 m4r nil) #2a((12 13 14 15)))
  (v= (ag::permute/sub v3r 3 1 m4r nil) #2a((13 14 15)))

  ;; row submatrices of row-major matrix as column-vectors
  (v= (ag::permute/sub v4 0 0 m4r t) #2a((0) (1) (2) (3)))
  (v= (ag::permute/sub v4 1 0 m4r t) #2a((4) (5) (6) (7)))
  (v= (ag::permute/sub v4 2 0 m4r t) #2a((8) (9) (10) (11)))
  (v= (ag::permute/sub v4 3 0 m4r t) #2a((12) (13) (14) (15)))
  (v= (ag::permute/sub v3 3 1 m4r t) #2a((13) (14) (15)))

  ;; square submatrices
  (v= (ag::permute/sub m2 0 0 m4 nil) #2a((0 4) (1 5)))
  (v= (ag::permute/sub m2 2 0 m4 nil) #2a((2 6) (3 7)))
  (v= (ag::permute/sub m2 0 2 m4 nil) #2a((8 12) (9 13)))
  (v= (ag::permute/sub m2 2 2 m4 nil) #2a((10 14) (11 15)))

  (v= (ag::permute/sub m2 0 0 m4 t) #2a((0 1) (4 5)))
  (v= (ag::permute/sub m2 2 0 m4 t) #2a((2 3) (6 7)))
  (v= (ag::permute/sub m2 0 2 m4 t) #2a((8 9) (12 13)))
  (v= (ag::permute/sub m2 2 2 m4 t) #2a((10 11) (14 15)))

  ;; rectangular
  (v= (ag::permute/sub m2x3 0 0 m4 nil) #2a((0 4 8)
                                            (1 5 9)))
  (v= (ag::permute/sub m2x3 2 0 m4 nil) #2a((2 6 10)
                                            (3 7 11)))
  (v= (ag::permute/sub m3x4 0 0 m4 nil) #2a((0 4 8 12)
                                            (1 5 9 13)
                                            (2 6 10 14)))
  (v= (ag::permute/sub m3x4 1 0 m4 nil) #2a((1 5 9 13)
                                            (2 6 10 14)
                                            (3 7 11 15)))

)
#++
(test 'sub)

(define-test (permute slice)
  :depends-on (matrix-type)
  ;; wrong number of rows/columns
  (fail (ag::permute/slice m2 #*1111 #*1111 m4 nil))
  (fail (ag::permute/slice m2 #*1001 #*1111 m4 nil))
  (fail (ag::permute/slice m2 #*0001 #*0011 m4 nil))
  (fail (ag::permute/slice m2 #b1111 #b1111 m4 nil))
  (fail (ag::permute/slice m2 #b0001 #b0011 m4 nil))

  ;; variants that could use /sub
  ;; column vector
  (v= (ag::permute/slice v4 #*1111 #*1000 m4 nil) #2a((0) (1) (2) (3)))
  (v= (ag::permute/slice v4 #*1111 #*0010 m4 nil) #2a((8) (9) (10) (11)))
  ;; column as row vector
  (v= (ag::permute/slice v4r #*1111 #*1000 m4 t) #2a ((0 1 2 3)))
  (v= (ag::permute/slice v4r #*1111 #*0010 m4 t) #2a ((8 9 10 11)))

  ;; row vector
  (v= (ag::permute/slice v4r #*1000 #*1111 m4 nil) #2a ((0 4 8 12)))
  (v= (ag::permute/slice v4r #*0010 #*1111 m4 nil) #2a ((2 6 10 14)))
  ;; row as column vector
  (v= (ag::permute/slice v4 #*1000 #*1111 m4 t) #2a((0) (4) (8) (12)))
  (v= (ag::permute/slice v4 #*0010 #*1111 m4 t) #2a((2) (6) (10) (14)))
  ;; row vector, row-major
  (v= (ag::permute/slice v4r #*1000 #*1111 m4r nil) #2a ((0 1 2 3)))
  (v= (ag::permute/slice v4r #*0010 #*1111 m4r nil) #2a ((8 9 10 11)))
  ;; row as column vector, row-major
  (v= (ag::permute/slice v4 #*1000 #*1111 m4r t) #2a((0) (1) (2) (3)))
  (v= (ag::permute/slice v4 #*0010 #*1111 m4r t) #2a((8) (9) (10) (11)))


  ;; 2x2
  (v= (ag::permute/slice m2 #*1100 #*1100 m4 nil) #2a((0 4)
                                                      (1 5)))
  (v= (ag::permute/slice m2 #*0011 #*0011 m4 nil) #2a((10 14)
                                                      (11 15)))
  (v= (ag::permute/slice m2 #*1100 #*1100 m4r nil) #2a((0 1)
                                                       (4 5)))
  (v= (ag::permute/slice m2 #*0011 #*0011 m4r nil) #2a((10 11)
                                                       (14 15)))
  (v= (ag::permute/slice m2 #*1100 #*1100 m4 t) #2a((0 1)
                                                    (4 5)))
  (v= (ag::permute/slice m2 #*0011 #*0011 m4 t) #2a((10 11)
                                                    (14 15)))
  (v= (ag::permute/slice m2 #*1100 #*1100 m4r t) #2a((0 4)
                                                     (1 5)))
  (v= (ag::permute/slice m2 #*0011 #*0011 m4r t) #2a((10 14)
                                                     (11 15)))

  ;; non-contiguous submatrices

  ;; rows/columns
  (v= (ag::permute/slice m4x2 #*1111 #*1010 m4 nil) #2a((0 8)
                                                        (1 9)
                                                        (2 10)
                                                        (3 11)))
  (v= (ag::permute/slice m2x4 #*1010 #*1111 m4 nil) #2a((0 4 8 12)
                                                        (2 6 10 14)))
  ;; (various combinations used by determinant)
  ;; remove top row and middle column etc
  (v= (ag::permute/slice m3 #*0111 #*1011 m4 nil) #2a((1 9 13)
                                                      (2 10 14)
                                                      (3 11 15)))
  (v= (ag::permute/slice m3 #*0111 #*1101 m4 nil) #2a((1 5 13)
                                                      (2 6 14 )
                                                      (3 7 15))
)
  (v= (ag::permute/slice m2 #*0011 #*1010 m4 nil) #2a((2 10)
                                                      (3 11)))
  (v= (ag::permute/slice m2 #*0011 #*1001 m4 nil) #2a((2 14)
                                                      (3 15)))
  (v= (ag::permute/slice m2 #*0011 #*0101 m4 nil) #2a((6 14)
                                                      (7 15)))

  ;; test with integer mask instead of bitvector
  (v= (ag::permute/slice m3 #b1110 #b1101 m4 nil) #2a((1 9 13)
                                                      (2 10 14)
                                                      (3 11 15)))
  (v= (ag::permute/slice m3 #b1110 #b1011 m4 nil) #2a((1 5 13)
                                                      (2 6 14)
                                                      (3 7 15)))
  (v= (ag::permute/slice m2 #b1100 #b101 m4 nil) #2a((2 10)
                                                     (3 11)))
  (v= (ag::permute/slice m2 #b1100 #b1001 m4 nil) #2a((2 14)
                                                      (3 15)))
  (v= (ag::permute/slice m2 #b1100 #b1010 m4 nil) #2a((6 14)
                                                      (7 15))))
#++
(test 'slice)

(define-test (permute diag)
  :depends-on (matrix-type)
  ;; requested (subset of) diagonal must be smaller than matrix
  ;; (possibly should require taking whole thing, or generalize the
  ;; implicit subset option?)
  (fail (ag::permute/diag v4 m3 nil))
  (fail (ag::permute/diag v5 m3 t))
  ;; normal diagonals (column-major)
  (v= (ag::permute/diag v4 m4 nil) #2a((0) (5) (10) (15)))
  (v= (ag::permute/diag v3 m3x4 nil) #2a((0) (4) (8)))
  (v= (ag::permute/diag v3 m4x3 nil) #2a((0) (5) (10)))

  (v= (ag::permute/diag v4 m4 t) #2a((12) (9) (6) (3)))
  (v= (ag::permute/diag v3 m3x4 t) #2a((9) (7) (5)))
  (v= (ag::permute/diag v3 m4x3 t) #2a((8) (5) (2)))
  ;; row-major versions
  (v= (ag::permute/diag v4 m4r t) #2a((3) (6) (9) (12)))
  (v= (ag::permute/diag v3 m3x4r nil) #2a((0) (5) (10)))
  (v= (ag::permute/diag v3 m4x3r nil) #2a((0) (4) (8)))

  (v= (ag::permute/diag v4 m4r t) #2a((3) (6) (9) (12)))
  (v= (ag::permute/diag v3 m3x4r t) #2a((3) (6) (9)))
  (v= (ag::permute/diag v3 m4x3r t) #2a((2) (4) (6)))
  ;; row-vector output
  (v= (ag::permute/diag v4r m4 nil) #2a((0 5 10 15)))
  (v= (ag::permute/diag v3r m3x4 nil) #2a((0 4 8)))
  (v= (ag::permute/diag v3r m4x3 nil) #2a((0 5 10)))

  (v= (ag::permute/diag v4r m4 t) #2a((12 9 6 3)))
  (v= (ag::permute/diag v3r m3x4 t) #2a((9 7 5)))
  (v= (ag::permute/diag v3r m4x3 t) #2a((8 5 2))))
#++
(test 'diag)

(define-test accessor)

(defun ac (ac &key binds)
  (let* ((f (ag:access ac))
         (r (mi:matrix-type-rows (ag:mtype ac)))
         (c (mi:matrix-type-columns (ag:mtype ac)))
         (a (make-array (list r c) :initial-element nil)))
    (loop for i below r
          do (loop for j below c
                   do (setf (aref a i j)
                            (eval `(let (,@binds) ,(funcall f i j))))))
    a))

(defmacro ac= (accessor a &key binds)
  `(is equalp ,a (ac ,accessor :binds ,binds)))
(defconstant +x+ 1.0)
(defconstant +y+ 2.0)
(defconstant +z+ 3.0)
(defvar *x*)
(define-test (accessor lit)
  (let* ((*x* 1.0)
         (a (finish (ag:literal m4
                                0 '+x+ '+y+ '+z+
                                4 5 6 7
                                8 9 10 11
                                12 13 14 15))))
      (of-type 'ag::accessor a)
      (fail (funcall (ag:access a) -1 0))
      (fail (funcall (ag:access a) 0 -1))
      (ac= a #2a ((0 1 2 3)
                  (4 5 6 7)
                  (8 9 10 11)
                  (12 13 14 15)))
      (is = 0 (funcall (ag:access a) 0 0))
      (is = 4 (funcall (ag:access a) 1 0))
      (is equal '(coerce +x+ 'single-float)
          (funcall (ag:access a) 0 1))
    (is = 11 (funcall (ag:access a) 2 3))
    (is = 15 (funcall (ag:access a) 3 3))
    (fail (funcall (ag:access a) 4 4))))

(define-test (accessor scalar)
  (let* ((a (finish (ag:scalar m2 12.3)))
         (b (finish (ag:scalar m2x3 'x))))
    (of-type 'ag::accessor a)
    (fail (funcall (ag:access a) -1 0))
    (fail (funcall (ag:access a) 0 -1))
    (ac= a #2a ((12.3 12.3)
                (12.3 12.3)))
    (ac= B #2a ((2.3 2.3 2.3)
                (2.3 2.3 2.3))
         :binds '((x 2.3)))
    (fail (funcall (ag:access a) 4 4))))

#++
(test 'scalar #+ :report 'interactive)


(defstruct s2x2
  (a 0.0 :type single-float)
  (b 0.0 :type single-float)
  (c 0.0 :type single-float)
  (d 0.0 :type single-float))

(define-test (accessor struct)
  (let* (#++(s (make-s2x2 :a 2.0 :b 3.0 :c 4.0 :d 5.0))
         (a (finish (ag:struct m2 's
                               's2x2-a 's2x2-b
                               's2x2-c 's2x2-d))))
    (of-type 'ag::accessor a)
    (fail (funcall (ag:access a) -1 0))
    (fail (funcall (ag:access a) 0 -1))
    (flet ((e (x)
             (eval (print `(let ((s (make-s2x2 :a 2.0 :b 3.0 :c 4.0 :d 5.0)))
                        ,x)))))
      (ac= a #2a ((2 3)
                  (4 5))
           :binds '((s (make-s2x2 :a 2.0 :b 3.0 :c 4.0 :d 5.0)))
           )

      (is = 2 (e (funcall (ag:access a) 0 0)))
      (is = 4 (e (funcall (ag:access a) 1 0))))
    (fail (funcall (ag:access a) 4 4))))


(define-test (accessor lrot)
  (flet ((e (m i j)
           (eval `(let (,@ (ag:binds m))
                    ,(funcall (ag:access m) i j)))))
   (let ((z2 (finish (ag:literal/rot m2 (/ pi 2) :z)))
         (x3 (finish (ag:literal/rot m3 (/ pi 2) :x)))
         (y3 (finish (ag:literal/rot m3 (/ pi 2) :y)))
         (z3 (finish (ag:literal/rot m3 (/ pi 2) :z)))
         (x4 (finish (ag:literal/rot m4 (/ pi 2) :x)))
         (y4 (finish (ag:literal/rot m4 (/ pi 2) :y)))
         (z4 (finish (ag:literal/rot m4 (/ pi 2) :z)))
         (-s (- (float (sin (/ pi 2)) 1.0)))
         (s (float (sin (/ pi 2)) 1.0))
         (c (float (cos (/ pi 2)) 1.0)))
     (fail (ag:literal/rot m2 1 :x))
     (fail (ag:literal/rot m2 1 :y))
     (fail (ag:literal/rot m2x3 1 :x))
     (fail (ag:literal/rot m3x2 1 :y))

     (is mi:ulp= c (e z2 0 0))
     (is mi:ulp= c (e z2 1 1))
     (is mi:ulp= -s (e z2 0 1))
     (is mi:ulp= s (e z2 1 0))

     (is = 1 (funcall (ag:access x3) 0 0))
     (is = 0 (funcall (ag:access x3) 1 0))
     (is = 0 (funcall (ag:access x3) 2 0))
     (is = 0 (funcall (ag:access x3) 0 1))
     (is = 0 (funcall (ag:access x3) 0 2))
     (is mi:ulp= c (e x3 1 1))
     (is mi:ulp= c (e x3 2 2))
     (is mi:ulp= -s (e x3 1 2))
     (is mi:ulp= s (e x3 2 1))

     (is = 0 (funcall (ag:access y3) 0 1))
     (is = 0 (funcall (ag:access y3) 1 0))
     (is = 1 (funcall (ag:access y3) 1 1))
     (is = 0 (funcall (ag:access y3) 2 1))
     (is = 0 (funcall (ag:access y3) 2 1))
     (is mi:ulp= c (e y3 0 0))
     (is mi:ulp= c (e y3 2 2))
     (is mi:ulp= s (e y3 0 2))
     (is mi:ulp= -s (e y3 2 0))

     (is = 0 (funcall (ag:access z3) 2 0))
     (is = 0 (funcall (ag:access z3) 0 2))
     (is = 0 (funcall (ag:access z3) 2 1))
     (is = 0 (funcall (ag:access z3) 1 2))
     (is = 1 (funcall (ag:access z3) 2 2))
     (is mi:ulp= c (e z3 0 0))
     (is mi:ulp= c (e z3 1 1))
     (is mi:ulp= -s (e z3 0 1))
     (is mi:ulp= s (e z3 1 0))

     (is = 1 (funcall (ag:access x4) 0 0))
     (is = 0 (funcall (ag:access x4) 1 0))
     (is = 0 (funcall (ag:access x4) 2 0))
     (is = 0 (funcall (ag:access x4) 0 1))
     (is = 0 (funcall (ag:access x4) 0 2))
     (is = 0 (funcall (ag:access x4) 0 3))
     (is = 0 (funcall (ag:access x4) 1 3))
     (is = 0 (funcall (ag:access x4) 2 3))
     (is = 1 (funcall (ag:access x4) 3 3))
     (is = 0 (funcall (ag:access x4) 3 0))
     (is = 0 (funcall (ag:access x4) 3 1))
     (is = 0 (funcall (ag:access x4) 3 2))
     (is = 1 (funcall (ag:access x4) 3 3))
     (is mi:ulp= c (e x4 1 1))
     (is mi:ulp= c (e x4 2 2))
     (is mi:ulp= -s (e x4 1 2))
     (is mi:ulp= s (e x4 2 1))

     (is = 0 (funcall (ag:access y4) 0 1))
     (is = 0 (funcall (ag:access y4) 1 0))
     (is = 1 (funcall (ag:access y4) 1 1))
     (is = 0 (funcall (ag:access y4) 2 1))
     (is = 0 (funcall (ag:access y4) 2 1))
     (is = 0 (funcall (ag:access y4) 0 3))
     (is = 0 (funcall (ag:access y4) 1 3))
     (is = 0 (funcall (ag:access y4) 2 3))
     (is = 1 (funcall (ag:access y4) 3 3))
     (is = 0 (funcall (ag:access y4) 3 0))
     (is = 0 (funcall (ag:access y4) 3 1))
     (is = 0 (funcall (ag:access y4) 3 2))
     (is = 1 (funcall (ag:access y4) 3 3))
     (is mi:ulp= c (e y4 0 0))
     (is mi:ulp= c (e y4 2 2))
     (is mi:ulp= s (e y4 0 2))
     (is mi:ulp= -s (e y4 2 0))

     (is = 0 (funcall (ag:access z4) 2 0))
     (is = 0 (funcall (ag:access z4) 0 2))
     (is = 0 (funcall (ag:access z4) 2 1))
     (is = 0 (funcall (ag:access z4) 1 2))
     (is = 1 (funcall (ag:access z4) 2 2))
     (is = 0 (funcall (ag:access z4) 0 3))
     (is = 0 (funcall (ag:access z4) 1 3))
     (is = 0 (funcall (ag:access z4) 2 3))
     (is = 1 (funcall (ag:access z4) 3 3))
     (is = 0 (funcall (ag:access z4) 3 0))
     (is = 0 (funcall (ag:access z4) 3 1))
     (is = 0 (funcall (ag:access z4) 3 2))
     (is = 1 (funcall (ag:access z4) 3 3))
     (is mi:ulp= c (e z4 0 0))
     (is mi:ulp= c (e z4 1 1))
     (is mi:ulp= -s (e z4 0 1))
     (is mi:ulp= s (e z4 1 0))


     ))
)
#++
(test 'lrot #+ :report 'interactive)
(user-homedir-pathname)

(define-test (accessor vec)
  (let* ((n 16)
         (v (make-array n :element-type 'single-float)))
    (loop for i below n
          do (setf (aref v i) (float i)))
    (let ((a (finish (ag:vec m4 v)))
          (ar (finish (ag:vec m4r v)))
          (b (finish (ag:vec m2 v)))
          (c (finish (ag:vec m2 v :ofs 12)))
          (d (finish (ag:vec m2 v :base 2)))
          (e (finish (ag:vec m2x4 v :ofs 1)))
          (f (finish (ag:vec m4x2 v :ofs 4))))
      (of-type ag::accessor a)
      (of-type ag::accessor b)
      (of-type ag::accessor c)
      (fail (funcall (ag:access a) -1 0))
      (fail (funcall (ag:access a) 0 -1))
      (ac= a #2a ((0 4 8 12)
                  (1 5 9 13)
                  (2 6 10 14)
                  (3 7 11 15)))
      (ac= ar #2a ((0 1 2 3)
                   (4 5 6 7)
                   (8 9 10 11)
                   (12 13 14 15)))
      (ac= b #2a ((0 2)
                  (1 3)))
      (ac= c #2a ((12 14)
                  (13 15)))
      (ac= d #2a ((8 10)
                  (9 11)))
      (ac= e #2a ((1 3 5 7)
                  (2 4 6 8)))
      (ac= f #2a ((4 8)
                  (5 9)
                  (6 10)
                  (7 11)))
      )))

#++
(test 'vec)

(define-test (accessor bvec)
  (let* ((n 16)
         (v (make-array (* 4 n) :element-type '(unsigned-byte 8))))
    (loop for i below n
          do (setf (nibbles:ieee-single-ref/le v (* i 4)) (float i)))
    (let ((a (finish (ag:bvec m4 v)))
          (ar (finish (ag:bvec m4r v)))
          (b (finish (ag:bvec m2 v)))
          (c (finish (ag:bvec m2 v :ofs (* 12 4))))
          (d (finish (ag:bvec m2 v :base 2)))
          (e (finish (ag:bvec m2x4 v :ofs (* 1 4))))
          (f (finish (ag:bvec m4x2 v :ofs (* 4 4)))))
      (of-type ag::accessor a)
      (of-type ag::accessor b)
      (of-type ag::accessor c)
      (fail (funcall (ag:access a) -1 0))
      (fail (funcall (ag:access a) 0 -1))
      (ac= a #2a ((0 4 8 12)
                  (1 5 9 13)
                  (2 6 10 14)
                  (3 7 11 15)))
      (ac= ar #2a ((0 1 2 3)
                   (4 5 6 7)
                   (8 9 10 11)
                   (12 13 14 15)))
      (ac= b #2a ((0 2)
                  (1 3)))
      (ac= c #2a ((12 14)
                  (13 15)))
      (ac= d #2a ((8 10)
                  (9 11)))
      (ac= e #2a ((1 3 5 7)
                  (2 4 6 8)))
      (ac= f #2a ((4 8)
                  (5 9)
                  (6 10)
                  (7 11))))))

;; todo: factor out body of vec tests from vector allocation and filling
(define-test (accessor ffivec)
  (let* ((n 16))
    (cffi:with-foreign-object (p :float n)
      (loop for i below n
            do (setf (cffi:mem-aref p :float i) (float i)))
      (let ((a (finish (ag:ffivec m4 p)))
            (ar (finish (ag:ffivec m4r p)))
            (b (finish (ag:ffivec m2 p)))
            (c (finish (ag:ffivec m2 p :ofs (* 12 4))))
            (d (finish (ag:ffivec m2 p :base 2)))
            (e (finish (ag:ffivec m2x4 p :ofs (* 1 4))))
            (f (finish (ag:ffivec m4x2 p :ofs (* 4 4)))))
        (of-type ag::accessor a)
        (of-type ag::accessor b)
        (of-type ag::accessor c)
        (fail (funcall (ag:access a) -1 0))
        (fail (funcall (ag:access a) 0 -1))
        (fail (funcall (ag:access d) -1 0))
        (fail (funcall (ag:access d) 0 -1))
        (ac= a #2a ((0 4 8 12)
                    (1 5 9 13)
                    (2 6 10 14)
                    (3 7 11 15)))
        (ac= ar #2a ((0 1 2 3)
                     (4 5 6 7)
                     (8 9 10 11)
                     (12 13 14 15)))
        (ac= b #2a ((0 2)
                    (1 3)))
        (ac= c #2a ((12 14)
                    (13 15)))
        (ac= d #2a ((8 10)
                    (9 11)))
        (ac= e #2a ((1 3 5 7)
                    (2 4 6 8)))
        (ac= f #2a ((4 8)
                    (5 9)
                    (6 10)
                    (7 11)))
        ;; bounds/stride/etc checking
        (fail (eval
               (funcall
                (ag:check
                 (finish (ag:ffivec m2 p :base 4 :buffer-size '(* n 4)))))))
        (fail (ag:ffivec m2 p :base 4 :buffer-size (* n 4)))
        (fail (ag:ffivec m4 p :stride 6))
        (fail (ag:ffivec m4 p :element-stride 2))
        (fail (ag:ffivec m4 p :buffer-size 12))))))

(define-test (accessor submatrix)
  (let* ((a (mv 4 4
                0 1 2 3
                4 5 6 7
                8 9 10 11
                12 13 14 15))
         ;; columns, as column vectors
         (c0 (finish (ag:submatrix 4 1 0 0 (ag:vec m4 a))))
         (c1 (finish (ag:submatrix 4 1 0 1 (ag:vec m4 a))))
         (c2 (finish (ag:submatrix 4 1 0 2 (ag:vec m4 a))))
         (c3 (finish (ag:submatrix 4 1 0 3 (ag:vec m4 a))))
         ;; row as row vector
         (r3 (finish (ag:submatrix 1 4 3 0 (ag:vec m4 a))))
         ;; 2x2s
         (m2@00 (finish (ag:submatrix 2 2 0 0 (ag:vec m4 a))))
         (m2@11 (finish (ag:submatrix 2 2 1 1 (ag:vec m4 a))))
         (m2@22 (finish (ag:submatrix 2 2 2 2 (ag:vec m4 a)))))
    (of-type 'ag::accessor c1)
    (ac= c0 #2a ((0) (4) (8) (12)))
    (ac= c1 #2a ((1) (5) (9) (13)))
    (ac= c2 #2a ((2) (6) (10) (14)))
    (ac= c3 #2a ((3) (7) (11) (15)))

    (ac= r3 #2a((12 13 14 15)))

    (fail (ag:submatrix 4 1 -1 -1 (ag:vec m4 a)))
    (fail (ag:submatrix 4 1 -1 0 (ag:vec m4 a)))
    (fail (ag:submatrix 4 1 1 1 (ag:vec m4 a)))
    (fail (ag:submatrix 4 1 0 4 (ag:vec m4 a)))

    (ac= m2@00 #2a((0 1)
                   (4 5)))
    (ac= m2@11 #2a((5 6)
                   (9 10)))
    (ac= m2@22 #2a((10 11)
                   (14 15)))

    (fail (ag:submatrix 2 2 -1 -1 (ag:vec m4 a)))
    (fail (ag:submatrix 2 2 -1 0 (ag:vec m4 a)))
    (fail (ag:submatrix 2 2 3 3 (ag:vec m4 a)))
    (fail (ag:submatrix 2 2 0 4 (ag:vec m4 a)))
    (fail (ag:submatrix 2 2 4 0 (ag:vec m4 a)))))

(define-test (accessor submatrix*)
  (let* ((a (mv 4 4
                0 1 2 3
                4 5 6 7
                8 9 10 11
                12 13 14 15))
         ;; columns, as column vectors
         (c0 (finish (ag:submatrix* #*1111 #*1000 (ag:vec m4 a))))
         (c0a (finish (ag:submatrix* -1 #*1000 (ag:vec m4 a))))
         ;; row as row vector
         (r3 (finish (ag:submatrix* #*0001 #*1111 (ag:vec m4 a))))
         (r3a (finish (ag:submatrix* #*0001 -1 (ag:vec m4 a))))
         ;; 2x2s
         (m2@x (finish (ag:submatrix* #*0110 #*0110 (ag:vec m4 a))))
         (m2@y (finish (ag:submatrix* #*1010 #*1010 (ag:vec m4 a))))
         (m2@z (finish (ag:submatrix* #*1100 #*1100 (ag:vec m4 a))))
         ;; stuff from determinant
         (b (mv 3 3
                0 1 2
                3 4 5
                6 7 8))
         (lb (finish (ag:literal '(3 3)
                                 0 1 2
                                 3 4 5
                                 6 7 8)))
         (d0 (finish (ag:submatrix* #*011 #*011 (ag:vec m3 b))))
         (d1 (finish (ag:submatrix* #*011 #*101 (ag:vec m3 b))))
         (d2 (finish (ag:submatrix* #*011 #*110 (ag:vec m3 b))))
         (d0l (finish (ag:submatrix* #*011 #*011 lb)))
         (d1l (finish (ag:submatrix* #*011 #*101 lb)))
         (d2l (finish (ag:submatrix* #*011 #*110 lb)))
         )
    (of-type 'ag::accessor c0)
    (ac= c0 #2a ((0) (4) (8) (12)))
    (ac= r3 #2a((12 13 14 15)))
    (ac= c0a #2a ((0) (4) (8) (12)))
    (ac= r3a #2a((12 13 14 15)))

    (ac= m2@x #2a((5 6)
                   (9 10)))
    (ac= m2@y #2a((0 2)
                   (8 10)))
    (ac= m2@z #2a((0 1)
                   (4 5)))

    (ac= d0 #2a ((4 5)
                 (7 8)))
    (ac= d1 #2a ((3 5)
                 (6 8)))
    (ac= d2 #2a ((3 4)
                 (6 7)))

    (ac= d0l #2a ((4 5)
                 (7 8)))
    (ac= d1l #2a ((3 5)
                 (6 8)))
    (ac= d2l #2a ((3 4)
                 (6 7)))

    (fail (ag:submatrix* #*11111 #*1111 (ag:vec m4 a)))
    (fail (ag:submatrix* #*1111 #*11111 (ag:vec m4 a)))
    (fail (ag:submatrix* #*0000 #*0000 (ag:vec m4 a)))
    (fail (ag:submatrix* #*00011 #*00011 (ag:vec m4 a)))))

#++
(test 'submatrix*)
(let ((b (mv 3 3
             0 1 2
             3 4 5
             6 7 8))
      (lb (ag:literal '(3 3)
                       0 1 2
                       3 4 5
                       6 7 8)))
  (ag:submatrix* #*011 #*101 (ag:vec m3 b))
  (ag:submatrix* #*011 #*101 lb))

(define-test (accessor vec/diagonal)
  (let* ((a (mv 4 4
                0 1 2 3
                4 5 6 7
                8 9 10 11
                12 13 14 15))
         (b (mv 3 4
                0 1 2 3
                4 5 6 7
                8 9 10 11))
         (c (mv 4 3
                0 1 2
                3 4 5
                6 7 8
                9 10 11))
         ;; diagonals, as column vector
         (ca (finish (ag:diagonal (ag:vec m4 a))))
         (cb (finish (ag:diagonal (ag:vec m3x4 b))))
         (cc (finish (ag:diagonal (ag:vec m4x3 c))))

         ;; antidiagonals, as column vector
         (aca (finish (ag:antidiagonal (ag:vec m4 a))))
         (acb (finish (ag:antidiagonal (ag:vec m3x4 b))))
         (acc (finish (ag:antidiagonal (ag:vec m4x3 c)))))
    (of-type 'ag::accessor ca)
    (ac= ca #2a ((0) (5) (10) (15)))
    (ac= cb #2a ((0) (5) (10)))
    (ac= cc #2a ((0) (4) (8)))

    (ac= aca #2a ((3) (6) (9) (12)))
    (ac= acb #2a ((3) (6) (9)))
    (ac= acc #2a ((2) (4) (6)))))

(define-test (accessor vec/transpose)
  (let* ((a (mv 4 4
                0 1 2 3
                4 5 6 7
                8 9 10 11
                12 13 14 15))
         (b (mv 3 4
                0 1 2 3
                4 5 6 7
                8 9 10 11))
         (c (mv 4 3
                0 1 2
                3 4 5
                6 7 8
                9 10 11))
         (v (mv 4 1 0 1 2 3))
         (r (mv 1 4 0 1 2 3))
         (ta (finish (ag:transpose (ag:vec m4 a))))
         (tb (finish (ag:transpose (ag:vec m3x4 b))))
         (tc (finish (ag:transpose (ag:vec m4x3 c))))
         (tr (finish (ag:transpose (ag:vec v4r r))))
         (tv (finish (ag:transpose (ag:vec v3 v)))))
    (of-type 'ag::accessor ta)
    (ac= ta #2a ((0 4 8 12)
                 (1 5 9 13)
                 (2 6 10 14)
                 (3 7 11 15)))
    (ac= tb #2a ((0 4 8)
                 (1 5 9)
                 (2 6 10)
                 (3 7 11)))
    (ac= tc #2a ((0 3 6 9)
                 (1 4 7 10)
                 (2 5 8 11)))

    (ac= tr #2a ((0) (1) (2) (3)))
    (ac= tv #2a ((0 1 2)))))


(define-test lib)

(define-test (lib determinant)
  (is = 18.0 (eval (l:determinant (ag:literal (3 3)
                                              -2 2 -3
                                              -1 1 3
                                              2 0 -1))))
  (is = 18.0 (eval `(let ((a -2) (b 2) (c -3)
                          (d -1) (e 1) (f 3)
                          (g 2) (h 0) (i 3))
                      (l:determinant (ag:literal (3 3)
                                                 a b c
                                                 d e f
                                                 g h i)))))
  (is equalp `(- (+ (* A (- (+ (* F (- (* K P) (* L O)))
                               (* H (- (* J O) (* K N))))
                            (* G (- (* J P) (* L N)))))
                    (* C (- (+ (* E (- (* J P) (* L N)))
                               (* H (- (* I N) (* J M))))
                            (* F (- (* I P) (* L M))))))
                 (* B (- (+ (* E (- (* K P) (* L O)))
                            (* H (- (* I O) (* K M))))
                         (* G (- (* I P) (* L M)))))
                 (* D (- (+ (* E (- (* J O) (* K N)))
                            (* G (- (* I N) (* J M))))
                         (* F (- (* I O) (* K M))))))
      (macroexpand '(l:determinant (ag:literal (4 4)
                                    a b c d
                                    e f g h
                                    i j k l
                                    m n o p)
                     :opt :remove-coerce))))
#++
(test 'determinant #+:report 'interactive)
#++
(let* ((n 16)
       (v (make-array n :element-type 'single-float)))
  (loop for i below n
        do (setf (aref v i) (float i)))
  (let ((a (finish (ag:vec m4 v))))
    (eval (funcall (ag:access a) 0 0))
))
#++
(test '(permute diag))
#++
(test 'permute)
#++
(test '3b-mmath-test)

