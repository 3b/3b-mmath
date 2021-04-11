#++(ql:quickload '(3b-mmath/test))
(defpackage #:3b-mmath/test
  (:use :parachute :cl)
  (:local-nicknames (:m :3b-mmath)))
(in-package 3b-mmath/test)



(defun accessor-equal (a b)
  (and (equalp (m::dimensions a) (m::dimensions b))
       (let ((ok t))
         (m::map-dimensions (m::dimensions a)
                            (lambda (i)
                              (setf ok (and ok
                                            (equalp (m::read-element a i)
                                                    (m::read-element b i))))))
         ok)))

(define-test storage
  ;; size is a positive fixnum
  (fail (make-instance 'm::storage :size 'a))
  (fail (make-instance 'm::storage :size 12.3))
  (fail (make-instance 'm::storage :size -1))
  (fail (make-instance 'm::storage :size 0))
  (finish (make-instance 'm::storage :size 1))
  (finish (make-instance 'm::storage :size 16))
  (fail (make-instance 'm::storage :size (expt 2 66)))
  ;; element-type is a type designator (or symbol (cons symbol))
  (finish (make-instance 'm::storage :size 16
                                     :element-type 'single-float))
  (finish (make-instance 'm::storage :size 16
                                     :element-type '(single-float 0.0)))
  (fail (make-instance 'm::storage :size 16 :element-type 1.23))

  ;; offset is a non-negative fixnum
  (fail (make-instance 'm::storage-cl-vector :size 16 :offset -1))
  (finish (make-instance 'm::storage-cl-vector :size 16 :offset 0))
  (fail (make-instance 'm::storage-cl-vector :size 16 :offset 12.3))
  (finish (make-instance 'm::storage-cl-vector :size 16 :offset 32))
  (fail (make-instance 'm::storage-cl-vector :size 16 :offset 'b))
  (fail (make-instance 'm::storage-cl-vector :size 16 :offset (expt 2 66)))
  (let ((s (finish (make-instance 'm::storage-cl-vector
                                  :storage 'a
                                  :size 16 :offset 32
                                  :element-type 'single-float)))
        (s2 (finish (make-instance 'm::storage-cl-vector
                                   :storage 'b
                                   :size 8 :offset 48
                                   :element-type 'double-float))))

    ;; READ/WRITE code requires a literal integer for index, within [0,size)
    (fail (m::read-element s -1))
    (finish (m::read-element s 0))
    (finish (m::read-element s2 7))
    (fail (m::read-element s2 8))
    (finish (m::read-element s 15))
    (fail (m::read-element s 16))
    (fail (m::read-element s 'a))
    (fail (m::read-element s 12.34))

    ;; bounds check generates code to determine if all elements are accessible
    (is equal '(assert (array-in-bounds-p a 47)) (m::bounds-check s))
    (is equal '(assert (array-in-bounds-p b 55)) (m::bounds-check s2))

    ;; read-element generates code to read specified element
    (is equal '(aref a 32) (m::read-element s 0))
    (is equal '(aref b 49) (m::read-element s2 1))
    (is equal '(aref a 34) (m::read-element s 2))

    ;; write-element generates code to assign a value tos pecified element

    ;; always coerce literal #s
    (is equal '(setf (aref a 35) 12.0) (m::write-element 12 s 3))

    (is equal '(setf (aref a 35) 12.0)
        (m::write-element 12 s 3 :type 'single-float))
    ;; coerce variables by default
    (is equal '(setf (aref a 36) (coerce x 'single-float))
        (m::write-element 'x s 4))
    ;; or when type is specified and doesn't match
    (is equal '(setf (aref a 36) (coerce x 'single-float))
        (m::write-element 'x s 4 :type 'double-float))
    ;; but not when type matches
    (is equal '(setf (aref a 36) x)
        (m::write-element 'x s 4 :type 'single-float))
    ;; skip-bounds-check generates code without bounds checking on sbcl
    (skip-on ((not :sbcl)) "only implemented for sbcl so far"
      (is equal
          '(setf (locally (declare
                           (optimize (sb-c::insert-array-bounds-checks 0)))
                   (aref a 36))
            (coerce x 'single-float))
          (m::write-element 'x s 4 :skip-bounds-check t)))

    ;; we can return the underlying cl-vector
    (is eql 'a (m::storage s))
    (is eql 'b (m::storage s2))



    (let ((a '(coerce (loop for i from 0.0 below 16.0 collect i)
               '(simple-array single-float (16))))
          (s (finish
              (make-instance 'm::storage-cl-vector
                             :storage 'a :size 16
                             :element-type 'single-float))))
      ;; bounds checks work
      (finish (eval `(let ((a ,a))
                       ,(m::bounds-check s))))
      (fail (eval `(let ((a (make-array 15)))
                     ,(m::bounds-check s))))
      ;; readers/writers work
      (is eql 1.0 (eval `(let ((a ,a))
                           ,(m::read-element s 1))))
      (is eql 12.0 (eval `(let ((a ,a))
                            ,(m::read-element s 12))))
      (is eql 33.0 (eval `(let ((a ,a))
                            ,(m::write-element 33 s 12))))
      (is equal '(12.0 33.0 33.0) (eval `(let ((a ,a))
                                           (list
                                            ,(m::read-element s 12)
                                            ,(m::write-element 33 s 12)
                                            ,(m::read-element s 12))))))))

#++
(test 'storage)

(define-test util
  (labels ((md (n)
             (let ((r nil))
               (m::map-dimensions n (lambda (a) (push a r)))
               (nreverse r))))
    (is equalp '(()) (md '()))
    (is equalp '((0)) (md '(0)))
    (is equalp '((0)) (md '(1)))
    (is equalp '((0 0 0)) (md '(0 0 0)))
    (is equalp '((0 0 0)) (md '(1 1 1)))
    (is equalp '((0 0) (1 0) (0 1) (1 1)) (md '(2 2)))
    (is equalp '((0 0 0) (1 0 0) (0 0 1) (1 0 1)) (md '(2 0 2)))
    (is equalp '((0 0) (1 0) (2 0) (3 0)
                 (0 1) (1 1) (2 1) (3 1)
                 (0 2) (1 2) (2 2) (3 2))
        (md '(4 3))))
  ;; special-case op-types
  (is eql 'integer (m::op-type 'mod '(short-float single-float)))
  (is eql 'integer (m::op-type 'floor '(double-float single-float)))
  (is eql 'integer (m::op-type 'round '(unsigned-byte single-float)))
  ;; float contagion
  (is eql 'short-float (m::op-type '+ '(short-float)))
  (is eql 'short-float (m::op-type '+ '(short-float integer)))
  (is eql 'single-float (m::op-type '+ '(short-float single-float)))
  (is eql 'single-float (m::op-type '+ '(unsigned-byte short-float single-float)))
  (is eql 'double-float (m::op-type '+ '(unsigned-byte double-float short-float single-float)))
  (is eql 'long-float (m::op-type '+ '(long-float unsigned-byte double-float short-float single-float)))
  ;; numbers with no float, guess number (can't just keep type, since
  ;; many ops will change type, for example / from integer to
  ;; rational, or SIN etc from non-float to float, or SQRT to complex
  (is eql 'number (m::op-type '+ '(bit)))
  (is eql 'number (m::op-type '+ '(bit integer)))
  (is eql 'number (m::op-type '+ '(real complex)))
  ;; any non-numeric types -> T
  (is eql 't (m::op-type 'append '(list list list)))
  (is eql 't (m::op-type 'char '(string unsigned-byte)))
  (is eql 't (m::op-type 'and '(boolean boolean))))
#++
(test 'util)

(define-test (storage literal)
  (let ((s (finish (m::make-storage-literal 'single-float
                                            0 1 2 3/4
                                            4 5 6 7
                                            8 9 'a 'b
                                            'c 'd 'e #1='(aref f 0)))))
    ;; size is # of elements supplied
    (is = 1 (m::size (m::make-storage-literal 'single-float
                                              :a)))
    (is = 16 (m::size s))
    ;; numbers are coerced to specified type
    (is eql 0.0 (m::read-element s 0))
    (is eql 1.0 (m::read-element s 1))
    (is eql 0.75 (m::read-element s 3))
    ;; other forms are left as-is
    (is eql 'b (m::read-element s 11))
    (is eq #1# (m::read-element s 15))))

#++
(test 'literal)

(define-test matrix
  )

(define-test (matrix literal)
  ;; fails if dimensions don't match
  (fail (m::make-literal-matrix t '(3 3) 1 2 3))
  (fail (m::make-literal-matrix t '(2 2) 1 2 3 4 5 6))
  ;; works with correct dimensions
  (let ((m (finish (m::make-literal-matrix 'double-float
                                           '(4 4)
                                           0 1 2 3/4
                                           4 5 6 7/8
                                           8 9 'a 'b
                                           'c 'd 'e 'f))))
    ;; numbers coerced to specified type
    (is eql 0d0 (m::read-element m '(0 0)))
    (is eql 1d0 (m::read-element m '(0 1)))
    (is eql 0.75d0 (m::read-element m '(0 3)))
    (is eql 4d0 (m::read-element m '(1 0)))
    (is eql 'c (m::read-element m '(3 0)))))
#++
(test 'literal)

(define-test (matrix major)
  ;; row-major-matrix and column-major-matrix
  (let* ((s (finish (m::make-storage-literal 'double-float
                                             0 1/2 2
                                             3 4 5
                                             6 7 8
                                             9 'a 'b)))
         (s2 (finish (make-instance 'm::storage-cl-vector
                                    :storage 'a
                                    :size 16 :offset 32
                                    :element-type 'single-float)))
         (r (m::row-major-matrix s 4 3))
         (c (m::column-major-matrix s 4 3)))
    ;; c-m matrix is
    ;; 0 4 8
    ;; 1/2 5 9
    ;; 2 6 a
    ;; 3 7 b

    ;; can't make matrices of wrong size
    (fail (m::row-major-matrix s 4 4))
    (fail (m::row-major-matrix s 2 2))
    (fail (m::column-major-matrix s 4 4))
    (fail (m::column-major-matrix s 2 2))
    (fail (m::column-major-matrix s 2 3 4))

    (fail (m::row-major-matrix s2 3 4))
    (fail (m::row-major-matrix s2 2 2))
    (fail (m::column-major-matrix s2 4 3))
    (fail (m::column-major-matrix s2 2 2))
    (fail (m::column-major-matrix s2 2 2 2))

    ;; can make matrices of valid dimensions
    (finish (m::row-major-matrix s 12))
    (finish (m::row-major-matrix s 12 1))
    (finish (m::row-major-matrix s 3 2 2))
    (finish (m::row-major-matrix s 1 2 3 2))
    (finish (m::column-major-matrix s 12 1))
    (finish (m::column-major-matrix s 12))
    (finish (m::column-major-matrix s 3 2 2))
    (finish (m::column-major-matrix s 1 2 3 2))

    (finish (m::row-major-matrix s2 1 16))
    (finish (m::row-major-matrix s2 4 4))
    (finish (m::row-major-matrix s2 2 2 2 2))
    (finish (m::column-major-matrix s2 1 16))
    (finish (m::column-major-matrix s2 4 1 4))
    (finish (m::column-major-matrix s2 2 1 2 1 2 1 2))

    ;; matrix is specified size
    (is equal '(4 3) (m::dimensions r))
    (is equal '(4 3) (m::dimensions c))

    ;; numbers coerced to specified type
    (is eql 0d0 (m::read-element r '(0 0)))
    (is eql 0.5d0 (m::read-element r '(0 1)))
    (is eql 2d0 (m::read-element r '(0 2)))
    (is eql 3d0 (m::read-element r '(1 0)))

    (is eql 0d0 (m::read-element c '(0 0)))
    (is eql 0.5d0 (m::read-element c '(1 0)))
    (is eql 2d0 (m::read-element c '(2 0)))
    (is eql 3d0 (m::read-element c '(3 0)))
    (is eql 5d0 (m::read-element c '(1 1)))

    ;; others unmodified
    (is eql 'a (m::read-element r '(3 1)))
    (is eql 'a (m::read-element c '(2 2)))))
#++
(test 'major)

(define-test scalar
  (finish (m::make-scalar 'single-float 'a))
  (true (m::is-scalar (m::make-scalar 'single-float 'a)))
  (true (m::is-scalar (m::make-literal-matrix 'single-float nil 1)))
  (is eql 'a (m::scalar-value (m::make-scalar 'single-float 'a)))
  (is eql 'b (m::scalar-value (m::make-literal-matrix 'single-float nil 'b)))
  (is eql 1.0 (m::scalar-value (m::make-scalar 'single-float 1)))
  (is eql 2d0 (m::scalar-value (m::make-scalar 'double-float 2)))
  (is eql 3.0 (m::scalar-value (m::make-literal-matrix 'single-float nil 3))))
#++
(test 'scalar)


(define-test broadcast
  (let ((a2 (finish (m::make-literal-matrix 'single-float
                                            '(3 4)
                                            :a :b :c :d
                                            :e :f :g :h
                                            :i :j :k :l)))
        (a3 (finish (m::make-literal-matrix 'single-float
                                            '(2 2 2)
                                            :a :b
                                            :c :d
                                            :e :f
                                            :g :h))))
    ;; no duplicate symbols allowed
    (fail (m::reshape a2 :from m m :to n m))
    (fail (m::reshape a2 :from m n :to n n m))
    ;; only symbols allowed in from
    (fail (m::reshape a2 :from m 0 :to n m))
    ;; from must match shape of a
    (fail (m::reshape a2 :from m :to m))
    (fail (m::reshape a2 :from m n o :to o n m))
    (fail (m::reshape a3 :from m :to m))
    (fail (m::reshape a3 :from m n :to n m))
    (fail (m::reshape a3 :from m n o p :to p o n m))
    ;; only symbols or 0 allowed in to
    (fail (macroexpand '(m::reshape a2 :from m n :to n 1 m)))
    (fail (macroexpand '(m::reshape a2 :from m n :to n "x" m)))
    ;; all symbols in from must appear in to unless they are broadcast
    ;; dimensions
    (fail (m::reshape a2 :from m n :to m))
    (fail (m::reshape a2 :from m n :to n))
    (fail (m::reshape a3 :from m n o :to m))
    (fail (m::reshape a3 :from m n o :to n o))
    (fail (m::reshape a3 :from m n o :to 0 o))
    ;; valid reshapes finish
    (finish (m::reshape a2 :from m n :to m n)) ;; identity
    (finish (m::reshape a2 :from m n :to n m))
    (finish (m::reshape a2 :from m n :to 0 m n))
    (finish (m::reshape a2 :from m n :to m 0 n))
    (finish (m::reshape a2 :from m n :to m n 0))
    (finish (m::reshape a3 :from m n o :to m n o))
    (finish (m::reshape a3 :from m n o :to o n m))
    (finish (m::reshape a3 :from m n o :to n o m))
    (finish (m::reshape a3 :from m n o :to n o m 0))
    (finish (m::reshape a3 :from m n o :to n o 0 m))
    ;; broadcast reshape is broadcast
    (of-type 'm::broadcast (m::reshape a3 :from m n o :to n o 0 m))
    ;; non-broadcast reshape isn't brooadcast type
    (of-type '(and m::permutation (not m::broadcast))
             (m::reshape a3 :from m n o :to n o m))
    ;; broadcast dimensions are marked as such
    (let ((b (finish (m::reshape a3 :from m n o :to n 0 o 0 m 0))))
      (false (m::is-broadcast-dimension b 0))
      (true (m::is-broadcast-dimension b 1))
      (false (m::is-broadcast-dimension b 2))
      (true (m::is-broadcast-dimension b 3))
      (false (m::is-broadcast-dimension b 4))
      (true (m::is-broadcast-dimension b 5)))
    ;; can remove broadcast dimensions
    (let ((b (finish (m::reshape a3 :from m n o :to n 0 o 0 m 0))))
      (fail (m::reshape b :from a b c d e f :to b c d e f))
      (finish (m::reshape b :from a b c d e f :to a c d e f))
      (fail (m::reshape b :from a b c d e f :to a b d e f))
      (finish (m::reshape b :from a b c d e f :to a b c e f))
      (fail (m::reshape b :from a b c d e f :to a b c d f))
      (finish (m::reshape b :from a b c d e f :to a b c d e))
      (finish (m::reshape b :from a b c d e f :to a c e)))
    ;; can remove dimension if it has exactly 1 element? not sure
    ;; about that, for now use slice instead?
    #++
    (let ((a (finish (m::make-literal-matrix 'single-float '(1) :a)))
          (b (finish (m::make-literal-matrix 'single-float '(1 2) :a :b)))
          (c (finish (m::make-literal-matrix 'single-float '(2 1) :a :b)))
          (d (finish (m::make-literal-matrix 'single-float '(1 2 3)
                                             :a :b :c
                                             :d :e :f)))
          (e (finish (m::make-literal-matrix 'single-float '(2 1 3)
                                             :a :b :c
                                             :d :e :f))))
      (finish (m::reshape a :from x :to))
      (finish (m::reshape b :from x y :to y))
      (finish (m::reshape c :from x y :to x))
      (finish (m::reshape d :from x y z :to y z))
      (finish (m::reshape e :from x y z :to x z)))

    ;; can broadcast a scalar (not sure if these are useful or not?)
    (let ((b (finish (m::reshape (m::make-scalar 'single-float 12)
                                 :from :to 0))))
      (is equal '(0) (m::dimensions b))
      ;; and can reshape it back to scalar
      (true (m::is-scalar (m::Reshape b :from x :to))))
    (let ((b (finish (m::reshape a2 :from m n :to n 0 m))))
      ;; has correct dimensions
      (is equal '(4 0 3) (m::dimensions b))
      ;; can access any element of broadcast dimension and get same
      ;; value, and get correct values
      (is eql :e (m::read-element b '(0 0 1)))
      (is eql :e (m::read-element b '(0 3 1)))
      (is eql :e (m::read-element b '(0 9 1)))
      (is eql :a (m::read-element b '(0 1 0)))
      (is eql :b (m::read-element b '(1 3 0)))
      (is eql :c (m::read-element b '(2 3 0)))
      (is eql :d (m::read-element b '(3 3 0)))
      (is eql :i (m::read-element b '(0 2 2)))
      (is eql :l (m::read-element b '(3 2 2))))))
#++
(test 'broadcast)

(defun md (n)
  (let ((r (make-array (m::dimensions n))))
    (m::map-dimensions (m::dimensions n)
                       (lambda (a)
                         (setf (apply #'aref r a)
                               (m::read-element n a))))
    r))

(define-test slice
  ;; fails for out of bound dimensions or indices
  (let ((a (finish (m::make-literal-matrix 'single-float
                                           '(1 2 3)
                                           :a :b :c
                                           :d :e :f))))
    (fail (m::slice a -1 0))
    (fail (m::slice a 3 0))
    (fail (m::slice a 0 -1))
    (fail (m::slice a 0 2))
    (fail (m::slice a 1 -2))
    (fail (m::slice a 1 3))
    (fail (m::slice a 2 -3))
    (fail (m::slice a 2 4)))

  ;; removing a 1-element dimension, so just reducing dimension
  ;; without removing any elements
  (let ((a (finish (m::make-literal-matrix 'single-float '(1) :a)))
        (b (finish (m::make-literal-matrix 'single-float '(1 2) :a :b)))
        (c (finish (m::make-literal-matrix 'single-float '(2 1) :a :b)))
        (d (finish (m::make-literal-matrix 'single-float '(1 2 3)
                                           :a :b :c
                                           :d :e :f)))
        (e (finish (m::make-literal-matrix 'single-float '(2 1 3)
                                           :a :b :c
                                           :d :e :f))))
    (is equalp #0a:a (md (m::slice a 0 0)))
    (is equalp #(:a :b) (md (m::slice b 0 0)))
    (is equalp #(:a :b) (md (m::slice c 1 0)))
    (is equalp #2a ((:a :b :c)
                    (:d :e :f))
        (md (m::slice d 0 0)))
    (is equalp #2a ((:a :b :c)
                    (:d :e :f))
        (md (m::slice e 1 0))))
  ;; slice at any index of broadcast dimension is equivalent to
  ;; removing that dimension
  (let* ((a (finish (m::make-literal-matrix 'single-float
                                            '(3 4)
                                            :a :b :c :d
                                            :e :f :g :h
                                            :i :j :k :l)))
         (b (finish (m::reshape a :from x y :to 0 x y)))
         (c (finish (m::reshape a :from x y :to x 0 y)))
         (d (finish (m::reshape a :from x y :to x y 0))))
    (true (accessor-equal a (m::slice b 0 0)))
    (true (accessor-equal a (m::slice b 0 1)))
    (true (accessor-equal a (m::slice b 0 2)))
    (true (accessor-equal a (m::slice c 1 3)))
    (true (accessor-equal a (m::slice d 2 4))))

  ;; otherwise returns specified slice of matrix
  (let* ((a (finish (m::make-literal-matrix 'single-float
                                            '(3 4)
                                            :a :b :c :d
                                            :e :f :g :h
                                            :i :j :k :l)))
         (b (finish (m::make-literal-matrix 'single-float
                                            '(2 2 2)
                                            :a :b
                                            :c :d

                                            :e :f
                                            :g :h))))
    ;; rows
    (is equalp #(:a :b :c :d) (md (m::slice a 0 0)))
    (is equalp #(:e :f :g :h) (md (m::slice a 0 1)))
    (is equalp #(:i :j :k :l) (md (m::slice a 0 2)))
    ;; columns
    (is equalp #(:a :e :i) (md (m::slice a 1 0)))
    (is equalp #(:b :f :j) (md (m::slice a 1 1)))
    (is equalp #(:c :g :k) (md (m::slice a 1 2)))
    (is equalp #(:d :h :l) (md (m::slice a 1 3)))
    ;; layers
    (is equalp #2a((:a :b) (:c :d)) (md (m::slice b 0 0)))
    (is equalp #2a((:e :f) (:g :h)) (md (m::slice b 0 1)))
    (is equalp #2a((:a :b) (:e :f)) (md (m::slice b 1 0)))
    (is equalp #2a((:c :d) (:g :h)) (md (m::slice b 1 1)))
    (is equalp #2a((:a :c) (:e :g)) (md (m::slice b 2 0)))
    (is equalp #2a((:b :d) (:f :h)) (md (m::slice b 2 1)))))
#++
(test 'slice)





(define-test per-element
  (let* ((a (finish (m::make-literal-matrix 'single-float
                                            '(3 2)
                                            0 1
                                            2 3
                                            4 5)))
         (b (finish (m::make-literal-matrix 'double-float
                                            '(3 2)
                                            10 11
                                            12 13
                                            14 15)))
         (c (finish (m::make-literal-matrix 'short-float
                                            '(3 2)
                                            110 111
                                            112 113
                                            114 115)))
         (d (finish (m::make-literal-matrix 'short-float
                                            '(2 1)
                                            :a :b)))
         (e (finish (m::make-literal-matrix 'short-float
                                            '(2 3)
                                            110 111 112
                                            113 114 115)))
         ;; per-element with matching shapes works
         (add0 (finish (m::per-element '+)))
         (negate (finish (m::per-element '- c)))
         (add (finish (m::per-element '+ a b)))
         (addt (finish (m::per-element '+ a (m::reshape e :from m n :to n m))))
         (add3 (finish (m::per-element '+ a b c)))
         (mod (finish (m::per-element 'mod b a)))
         ;; or with broadcasts
         (addb (finish (m::per-element '+
                                       (m::reshape a :from m n :to n m 0)
                                       (m::reshape d :from n k :to n 0 k)))))
    ;; mismatched shapes don't work
    (fail (m::per-element '+ a d))
    (fail (m::per-element '+ d c))
    ;; broadcasts with overlapping broadcast dimensions doesn't work yet
    (fail (m::per-element '+
                          (m::reshape a :from m n :to n m 0)
                          (m::reshape c :from m n :to m n 0)))

    ;; with no args, returns a scalar
    (true (m::is-scalar add0))
    ;; with value (op)
    (is equalp '(+) (m::scalar-value add0))
    ;; with args, returns same shape as inputs
    (is equalp '(3 2) (m::dimensions negate))
    (is equalp '(3 2) (m::dimensions add))
    (is equalp '(3 2) (m::dimensions addt))
    (is equalp '(3 2) (m::dimensions add3))
    (is equalp '(3 2) (m::dimensions mod))
    (is equalp '(2 3 1) (m::dimensions addb))
    ;; float element types are correct
    (is eql 'short-float (m::element-type negate))
    (is eql 'double-float (m::element-type add))
    (is eql 'single-float (m::element-type addt))
    (is eql 'double-float (m::element-type add3))
    (is eql 'single-float (m::element-type addb))
    ;; and types for known ops
    (is eql 'integer (m::element-type mod))
    ;; and elements apply op to exected elements
    (is equal '(- 110.0) (m::read-element negate '(0 0)))
    (is equal '(- 111.0) (m::read-element negate '(0 1)))
    (is equal '(- 112.0) (m::read-element negate '(1 0)))
    (is equal '(+ 0.0 10d0) (m::read-element add '(0 0)))
    (is equal '(+ 3.0 13d0) (m::read-element add '(1 1)))
    (is equal '(+ 2.0 12d0) (m::read-element add '(1 0)))
    (is equal '(+ 0.0 110s0) (m::read-element addt '(0 0)))
    (is equal '(+ 3.0 114s0) (m::read-element addt '(1 1)))
    (is equal '(+ 2.0 111s0) (m::read-element addt '(1 0)))
    (is equal '(+ 0.0 10d0 110s0) (m::read-element add3 '(0 0)))
    (is equal '(+ 3.0 13d0 113s0) (m::read-element add3 '(1 1)))
    (is equal '(+ 2.0 12d0 112s0) (m::read-element add3 '(1 0)))
    (is equal '(+ 0.0 :a) (m::read-element addb '(0 0 0)))
    (is equal '(+ 5.0 :b) (m::read-element addb '(1 2 0)))))
#++
(test 'per-element)

(define-test reduce
  ;; error to reduce a scalar
  (fail (m::reduce-dimension '+ (m::make-scalar 'single-float 1)))
  ;; if first dimension is broadcast, just remove it
  (let* ((a (finish (m::make-literal-matrix 'single-float
                                            '(2 2) :a :b :c :d)))
         (b (finish (m::reshape a :from m n :to 0 m n)))
         (c (finish (m::reshape* (m::make-scalar 'single-float 123)
                                 :from nil :to '(0))))
         (d (finish (m::reduce-dimension '+ c))))
    (true (accessor-equal a (m::reduce-dimension '+ b)))
    (true (m::is-scalar d))
    (is eql 123.0 (m::scalar-value d)))
  ;; if first dimension is 1, just remove it
  (let* ((a (finish (m::make-literal-matrix 'single-float
                                            '(1 2 2) :a :b :c :d)))
         (b (finish (m::make-literal-matrix 'single-float
                                            '(1) 123)))
         (c (finish (m::reduce-dimension '+ b))))
    (is equalp #2a ((:a :b) (:c :d)) (md (m::reduce-dimension '+ a)))
    (true (m::is-scalar c))
    (is eql 123.0 (m::scalar-value c)))

)
#++
(test 'reduce)

(define-test misc
  ;; matrix multiplication
  (flet ((mm (a b)
           (m::reduce-dimension
            '+ (m::per-element '*
                               (m::reshape a :from m n :to n m 0)
                               (m::reshape b :from n k :to n 0 k))))
         (e (c)
           (let ((r (make-array (m::dimensions c))))
             (m::map-dimensions (m::dimensions c)
                                (lambda (i)
                                  (setf (apply #'aref r i)
                                        (eval (m::read-element c i)))))
             r)))
   (let ((a (finish (m::make-literal-matrix t '(2 2) :a :b :c :d)))
         (b (finish (m::make-literal-matrix t '(2 1) :e :g)))
         (c (finish (m::make-literal-matrix t '(2 2) 'e 'f 'g 'h))))

     (is equalp
         #2A(((+ (* :A :E) (* :B :G)))
             ((+ (* :C :E) (* :D :G))))
         (md (mm a b)))
     (is equalp
         #2A(((+ (* :A E) (* :B G)) (+ (* :A F) (* :B H)))
             ((+ (* :C E) (* :D G)) (+ (* :C F) (* :D H))))
         (md (mm a c))))
    (is equalp #2a ((3 2340)
                    (0 1000))
        (e (mm (m::make-literal-matrix t '(2 3)
                                       2 3 4
                                       1 0 0)
               (m::make-literal-matrix t '(3 2)
                                       0 1000
                                       1 100
                                       0 10))))
    (is equalp #2a ((21 24 27)
                    (47 54 61))
        (e (mm (m::make-literal-matrix t '(2 2)
                                       1 2
                                       3 4)
               (m::make-literal-matrix t '(2 3)
                                       5 6 7
                                       8 9 10))))
    (fail
        (e (mm (m::make-literal-matrix t '(2 3)
                                       1 2 3
                                       4 5 6)
               (m::make-literal-matrix t '(2 3)
                                       5 6 7
                                       8 9 10))))))
#++
(test 'misc)


(defun f (x) (coerce x 'single-float))
(defun m4 (a b c d
           e f g h
           i j k l
           m n o p)
  (make-array 16 :element-type 'single-float
                 :initial-contents (mapcar 'f (list a e i m
                                                    b f j n
                                                    c g k o
                                                    d h l p))))

(defun compiles (l)
  (multiple-value-bind (r w f)
      (compile nil l)
    (when f (error "compilation failed"))
    (when w (error "compilation warned"))
    r))

(define-test usage
  (finish (eval `(m::defmfun matrix-mult (a b)
                   (reduce '+
                           (* (m::reshape a :from m n :to n m 0)
                              (m::reshape b :from n k :to n 0 k))))))
  (let ((m4r* (finish
               (compiles
                `(lambda (a b)
                   (declare (type (simple-array single-float (16)) a b))
                   (m::mm ()
                     (let ((a (m::wrap (m::cl-vector (4 4) :row-major t) a))
                           (b (m::wrap (m::cl-vector (4 4) :row-major t) b)))
                       (matrix-mult a b)))))))
        (m4c* (finish
               (compiles
                `(lambda (a b)
                   (declare (type (simple-array single-float (16)) a b))
                   (m::mm ()
                     (let ((a (m::wrap (m::cl-vector (4 4)) a))
                           (b (m::wrap (m::cl-vector (4 4)) b)))
                       (matrix-mult a b)))))))
        (m4c*f (finish
                (compiles
                 `(lambda (a f)
                    (declare (type (simple-array single-float (16)) a)
                             (type single-float f))
                    (m::mm ()
                      (let ((a (m::wrap (m::cl-vector (4 4)) a)))
                        (* a f)))))))
        (m4ctx (finish
                (compiles
                 `(lambda (a x y z)
                    (declare (type (simple-array single-float (16)) a)
                             (type single-float x y z))
                    (m::mm ()
                      (let ((a (m::wrap (m::cl-vector (4 4)) a)))
                        (matrix-mult a (m::make-literal-matrix 'single-float
                                                               '(4 4)
                                                               1 0 0 'x
                                                               0 1 0 'y
                                                               0 0 1 'z
                                                               0 0 0 1)))))))))
    (is equalp #(650.0 740.0 830.0 920.0 762.0 868.0 974.0 1080.0
                 861.0 982.0 1103.0 1224.0 986.0 1124.0 1262.0 1400.0)
        (funcall m4r*
                 (m4 1 2 3 4
                     5 6 7 8
                     9 10 11 12
                     13 14 15 16)
                 (m4 21 22 23 24
                     25 26 27 28
                     29 30 31 31
                     33 34 35 36)))
    (is equalp #(290.0 722.0 1154.0 1586.0 300.0 748.0 1196.0 1644.0
                 310.0 774.0 1238.0 1702.0 317.0 793.0 1269.0 1745.0)
        (funcall m4c*
                 (m4 1 2 3 4
                     5 6 7 8
                     9 10 11 12
                     13 14 15 16)
                 (m4 21 22 23 24
                     25 26 27 28
                     29 30 31 31
                     33 34 35 36)))

    (is equalp #(2.0 10.0 18.0 26.0
                 4.0 12.0 20.0 28.0
                 6.0 14.0 22.0 30.0
                 8.0 16.0 24.0 32.0)
        (funcall m4c*f
                 (m4 1 2 3 4
                     5 6 7 8
                     9 10 11 12
                     13 14 15 16)
                 2.0))
    (is equalp #(1.0 5.0 9.0 0.0 2.0 6.0 10.0 0.0
                 3.0 7.0 11.0 0.0 27.5 73.5 119.5 1.0)
        (funcall m4ctx (m4 1 2 3 4
                           5 6 7 8
                           9 10 11 12
                           0 0 0 1)
                 2.25 3.5 4.75))))
#++
(test 'usage)

#++
(test '3b-mmath/test)
