#++(ql:quickload '(3b-mmath))
(defpackage #:3b-mmath/examples
  (:use :cl :3b-mmath)
  (:local-nicknames (:m :3b-mmath))
  (:export
   #:matrix-mult
   #:eval-quadratic-bezier
   #:eval-cubic-bezier
   #:cross
   #:sign-at-curve
   #:dist))
(in-package #:3b-mmath/examples)

;; examples with shadowed symbols to simplify coding

(defmfun matrix-mult (a b)
  (reduce '+
          (* (reshape a :from m n :to n m 0)
             (reshape b :from n k :to n 0 k))))

(defpackage #:3b-mmath/examples/concrete
  (:use :cl :3b-mmath :3b-mmath/examples)
  (:local-nicknames (:l :3b-mmath/examples)
                    (:a :alexandria.2))
  (:export))
(in-package #:3b-mmath/examples/concrete)

(deftype m (i j &optional (type 'single-float))
  `(simple-array ,type (,(* i j))))

(deftype v (i &optional (type 'single-float))
  `(simple-array ,type (,i)))

(deftype m4 () '(m 4 4))
;; define a matching wrapper type alias, which will read a 4x4 matrix
;; from offset 0 of a variable, and attempt to detect element type
;; from a SIMPLE-ARRAY type declaration on that variable
(defwrap m4 () '(cl-vector (4 4) :type :auto))

;; consing single-float 4x4 matrix multiply
(defun m4* (a b)
  (declare (type m4 a b))
  ;; MM is the main entry point to concrete compiler
  (mm ()
    ;; for any non-scalar values we want to use, we need to wrap them
    ;; with WRAP. If wrapper doesn't specify an element type (or uses
    ;; :auto as in this case), it will be detected from any type
    ;; declarations in scope at this point.
    (let ((a (wrap m4 a))
          (b (wrap m4 b)))
     ;; returns accessor for a 4x4 single-float matrix, which is
     ;; implicitly written into a column-major (simple-array
     ;; single-float (16)) when returned from MM
     (l:matrix-mult a b))))

;; usually we will want a macro to define wrappers for us since we
;; will be doing the same wrapping in most cases within a particular
;; set of concrete functions
(defmacro with-m4s ((&rest vars) &body body)
  (print `(let (,@ (loop for v in vars collect `(,v (wrap m4 ,v))))
      ,@body)))

;; non-consing version, assumes no aliasing of arguments
#+todo
(defun m4*! (dest a b)
  (declare (type m4 dest a b))
  (mm (:runtime-overlap :ignore)
    ;; wrap variables using a macro
    (with-m4s (dest a b)
      ;; -> returns the accessor that is filled, and cl-vector accessor
      ;; is returned from MM as the underlying storage by default
      (-> dest (l:matrix-mult a b)))))

(defun m4*f (a f)
  (declare (type m4 a)
           (type single-float f))
  (mm ()
    ;; here we don't define a wrapper for F, so it will be implicitly
    ;; wrapped as a scalar (which is treated as matching any
    ;; size/shape of matrix for per-element ops, with all elements
    ;; containing same value). Like :auto wrappers, it will attempt to
    ;; detect type from declarations. If a scalar has an array/vector
    ;; type declaration, it will probably error (not sure yet) on the
    ;; assumption it should have been explicitly wrapped as some sort
    ;; of matrix.
    (with-m4s (a)
      ;; Since this is in vector context, * is compiled as per-element
      ;; multiply.
      (* a f))))

(defun translate (m x y z)
  (declare (type m4 m))
  (mm ()
    (with-m4s (m)
      (matrix-mult m (make-literal-matrix 'single-float
                                          '(4 4)
                                          1 0 0 'x
                                          0 1 0 'y
                                          0 0 1 'z
                                          0 0 0 1)))))


;; hypothetical non-consing pooled version
#++
(defcfun m4* (pool dest a b)
  (declare (type m4 dest pool)
           (type fixnum a b))
  (mm ()
    ;; if we don't have a defwrap matching type decl, or need to
    ;; specify more than just variable name for a particular wrapper,
    ;; we can use WRAP. (POOL isn't evaluated here, so doesn't get an
    ;; implicit wrapper)
    (macrolet ((pool (x)
                 ;; not sure yet if cl-vector wrapper will work with
                 ;; runtime offsets or not, so might need different
                 ;; type
                 `(wrap m4 pool :runtime-offset ,x)))
     (let ((a (pool a))
           (b (pool b))
           (dest (pool dest)))
       (-> dest (l:matrix-mult a b)))))
  ;; MM probably returned the underlying POOL vector (not completely sure yet?)
  ;; so return something more specific
  (values dest pool))

