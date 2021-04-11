#++(ql:quickload '(3b-mmath))
(in-package #:3b-mmath/examples)

;; examples with shadowed symbols to simplify coding

(defmfun eval-quadratic-bezier (b1 b2 b3 at)
  ;; defmfun assumes vector context, so these will expand to vector
  ;; code (with 'vector's possibly being wrapped scalars, as is
  ;; expected for AT) possibly should add some declarations for that?
  (let* ((at² (* at at))
         (mt (- 1 at))
         (mt² (* mt mt)))
    (+
     (* b1 mt²)
     (* 2 b2 mt at )
     (* b3 at²))))

(defmfun eval-cubic-bezier (b1 b2 b3 b4 at)
  (let* ((at² (* at at))
         (at³ (* at² at))
         (mt (- 1 at))
         (mt² (* mt mt))
         (mt³ (* mt² mt)))
    (+
     (* b1 mt³)
     (* 3 b2 mt² at)
     (* 3 b3 mt at²)
     (* b4 at³))))


(defun square (a)
  (expt a 2))

(defmfun dot (a b)
  (reduce + (* a b)))


;; write a meta-function manually, since we are making custom
;; permutations and such
(add-meta-function 'cx)
(defun cx (a)
  ;; todo: require input to be a 3-element column vector
  #++ (assert-shape a 3 1)

  ;; returns (written with 1-based indices)
  ;; 0 -a3 a2
  ;; a3 0 -a1
  ;; -a2 a1 0
  (per-element '*
               (make-literal-matrix (m::element-type a)
                                    '(3 3)
                                    0 -1 1
                                    1 0 -1
                                    -1 1 0)
               ;; manually create a 3x3 permutation of 1d accessor A with (at
               ;; least) 3 elements
               (make-instance 'm::permutation
                              :base a
                              :permutation
                              #2a(((0 0) (2 0) (1 0))
                                  ((2 0) (1 0) (0 0))
                                  ((1 0) (0 0) (2 0))))))

(defmfun cross (a b)
  ;; a,b both 3 element column vectors = 3x1 matrices
  (matrix-mult (cx a) b))

(defmfun || (v)
  (sqrt (reduce '+ (square v))))


;; allow multiple options for how NORMALIZE should treat 0-length
;; input, and select with a wrapper function/macro
(defmfun normalize/on-0-ignore (v)
  (/ v (|| v)))

#++
(defmfun normalize/on-0-error (v)
   (let ((l (|| v)))
     (if (= 0 l)
         (as-scalar (error "tried to normalize vector with length 0"))
         (/ v l))))

#++
(defmfun normalize/on-0-zero (v)
  (let ((l (|| v)))
    (if (= 0 l)
        ;; todo: add a "reshape to match V"
        (reshape 0 :from :to ?)
        (/ v l))))

;; not sure about this normalize wrapper, might be better to just let
;; users pick the specific behavior they want when making a concrete
;; version, and add options there if they want it generic?
#++
(declaim (inline normalize))
#++
(defun normalize (v &key (on-zero :error))
  ;; ECASE is marked to compile as scalar automatically since it
  ;; doesn't know about accessors, otherwise would need AS-SCALAR
  ;; around it
  (ecase on-zero
    ;; NORMALIZE/* were compiled with MDEFUN, so can be called
    ;; directly from another MDEFUN function in either vector or
    ;; scalar mode. If we wanted inline vector code using normal
    ;; functions like +,-,etc, would need to switch back with MPROGN
    (:ignore (normalize/on-0-ignore v))
    (:zero (normalize/on-0-zero v))
    (:error (normalize/on-0-error v))))


(defmfun dist (a b)
  (sqrt (reduce '+ (square (- a b)))))


(defmfun sign-at-curve (p0 p1 p2 at n d1)
  (let* ((p2-2p1+p0 (+ (- p2 (* p1 2)) p0))
         (c2d1 (* 2 d1))
         (db (+ (* p2-2p1+p0
                   (* 2 at))
                c2d1)))
    (if (plusp (|| (cross db n))) 1 -1)))

;; concrete code using above
(in-package #:3b-mmath/examples/concrete)

(deftype m (i j &optional (type 'single-float))
  `(simple-array ,type (,(* i j))))

(deftype v (i &optional (type 'single-float))
  `(simple-array ,type (,i)))

(deftype m4 () '(m 4 4))

(deftype v3d () '(v 3 double-float))
(deftype v4d () '(v 4 double-float))
(defwrap v3d () '(cl-vector (3) :type :auto))
(defwrap v4d () '(cl-vector (3) :type :auto))
(defun v3d (x y z)
  (make-array 3 :element-type 'double-float
                :initial-contents (list (float x 1d0)
                                        (float y 1d0)
                                        (float z 1d0))))

(defmacro with-v3d ((&rest vars) &body body)
  `(let (,@ (loop for v in vars collect `(,v (wrap v3d ,v))))
     ,@body))

(defmacro with-v4d ((&rest vars) &body body)
  `(let (,@ (loop for v in vars collect `(,v (wrap v4d ,v))))
     ,@body))

;; concrete versions with various types of accessors
(defun eval-quadratic-bezier/v (b1 b2 b3 at)
  (mm ()
    ;; expose input matrix directly with specified type. since AT
    ;; isn't included in list of wrappers, it is implicitly wrapped as
    ;; as a scalar
    (with-v3d (b1 b2 b3)
      (eval-quadratic-bezier b1 b2 b3 at))))

(defun eqb/ref (a b c at)
  (coerce
   (loop for i below 3
         collect (a:lerp at
                         (a:lerp at (aref a i) (aref b i))
                         (a:lerp at (aref b i) (aref c i))))
   'v3d))
(let ((a (v3d 1 2 0))
      (b (v3d 3 4 0))
      (c (v3d 4 5 0)))
 (loop for i from 0d0 upto 1d0 by 0.0625
       do (format t "~8f = ~s~%         ? ~s~%" i
                  (eval-quadratic-bezier/v a b c i)
                  (eqb/ref a b c i))))
(defun eval-quadratic-bezier/pool (dest pool o1 o2 o3 at)
  ;; allocate a vector for return value, with no name. MM will
  ;; implicitly copy contents of accessor returned by last form into
  ;; it to return
  (mm (:ret (v3d)
       ;; expose input matrices sharing storage as multiple
       ;; variables (here POOL is storage, o1,o2,o3 are offsets, p1
       ;; p2 p3 are accessor variable names
       :submats (pool v3d (b1 o1) (b2 o2) (b3 o3)))
      (eval-quadratic-bezier b1 b2 b3)))

(defun eval-quadratic-bezier/struct (dest s at)
  ;; allocate a vector for return value, and name it. The vector
  ;; should be manually filled with mreplace, and will be returned
  ;; from MM. Mostly intended for cases where return value will be
  ;; built in pieces (like columns of a matrix).
  (mm (:ret (v3d dest)
       ;; expose input matrices stored in struct slots S is variable
       ;; containing struct instance, S-P1,S-P2,S-P3 are slots
       ;; containing matrix data, B1,B2,B3 are accessor variable
       ;; names
       :structmats (S v3d (b1 s-p1) (b2 s-p2) (b3 s-p3))
       ;; following might be shorter with long struct names, but
       ;; might not always get packagee right? in general case would
       ;; also have to specify package name
       :structmats ((s :conc 's-) v3d (b1 p1) (b2 p2) (b3 p3)))
    (mreplace dest (eval-quadratic-bezier b1 b2 b3))))

;; helper for defining pairs of variable names with symbol designating
;; use of that symbol for both names
(defun ensure-name-pair (n)
  (if (consp n)
      n
      (list n n)))

;; define a new wrapper keyword, where a struct contains indices into
;; a pool of vectors
(define-mm-wrapper pool-struct (pool struct type &rest bindings)
  ;; binding forms:
  ;;   symbol expose slot as variable of same name
  ;;   (aname sname) expose slot SNAME as variable ANAME
  (loop for b in bindings
        for (a s) = (ensure-name-pair b)
        collect (list a (make-pool-accessor type pool `(,sname ,struct)))))

(defun eval-quadratic-bezier/poolstruct (dest s at)
  (mm (:dest v3d
       ;; using previously defined wrapper
       'pool-struct (pool s v3d (b1 s-p1) (b2 s-p2) (b3 s-p3)))
      (eval-quadratic-bezier b1 b2 b3)))


;; expected more idiomatic use with a wrapper for specific types
(defmacro with-spv3s ((pool &rest structs+vars) &body body)
  ;; instead of putting bindings directly in MM args, can also use WITH-MATS
  ;; inside body with same args
  `(with-mats ((loop for (s . vars) in structs+vars
                     collect `(pool-struct
                               ,pool
                               ,s
                               v3d
                               (,(first vars) s-p1)
                               (,(second vars) s-p2)
                               (,(third vars) s-p3))))
     ,@body))

;; used like
(defun eval-quadratic-bezier/poolstruct (dest s at)
  (mm ()
    (with-spv3s (pool (s b1 b2 b3))
      ;; -> is alias for mreplace
      (-> dest (eval-quadratic-bezier b1 b2 b3))))
  ;; if RET wasn't specified and last form of MM was vector code,
  ;; return value from MM is unspecified, so return something useful
  ;; from function
  dest)

;; concrete 4d double-float, with wrapper macro for type
(defmacro with-v4s ((&rest names) &body body)
  `(with-mats ((v4d
                ,@ (loop for n in names
                         for (a v) = (ensure-name-pair n)
                         collect (list a v))))
     ,@body))

(defun eval-cubic-bezier/v4 (dest b1 b2 b3 b4 at)
  (mm ()
    (with-v4s (dest b1 b2 b3 b4)
      (-> dest (eval-cubic-bezier b1 b2 b3 b4 at))))
  ;; if RET wasn't specified and last form of MM was vector code,
  ;; return value from MM is unspecified, so return something useful
  ;; from function
  dest)


(defun dist/v (a b)
  (mm (:mats (v3d a b))
    ;; to-scalar extracts a single value from a scalar or 1x1x..x1
    ;; matrix and allows mm to return it
    (to-scalar (dist a b))))

(defun cross/v (a b)
  (mm (:mats (v3d a b))
    ;; return/return-from directly in MM is shortcut for defining a
    ;; :ret argument matching type of value, and copying into it?
    (return (cross a b))))

(defun ||/v (a)
  (mm (:mats (v3d a))
    ;; to-scalar extracts a single value from a scalar or 1x1x..x1
    ;; matrix and allows mm to return it
    (to-scalar (|| a))))

(defun v- (a b)
  (mm (:mats (v3d a b))
      ;; return-as directly in MM is shortcut for defining a :ret
      ;; argument and copying into it? (inside defmfun, it tells caller
      ;; to generate a tmp var and write to that, not sure scope of tmp
      ;; var?)
    (return-as v3d (- a b))))

(defun sign-at-curve (p0 p1 p2 at n c2d1)
  (mm (:mats (v3d p0 p1 p2 n))
    (let ((p2-2p1+p0 (+ (- p2 (* p1 2)) p0))
          (db (+ (* p2-2p1+p0
                    (* 2 at))
                 c2d1)))
      (if (plusp (||/v (cross/v db n))) 1 -1))))



(defun dist-point/quadratic-bezier
    (p p0 p1 p2 &optional (out (make-array 3 :element-type 'double-float
                                             :initial-element 0d0)))
  (mm (:mats (v3d p p0 p1 p2)
        ;; accessors for DX allocated tmp vars
       :tmps (v3d d0 d1 d2)
       :tmps ((scalar :type double-float) a b c d))
      (setf d0 (- p p0)
            d1 (- p1 p0)
            d2 (- p2 (- (* p1 2) p0))
            a (dot d2 d2)
            ;; in vector context, so 3 is wrapped as a scalar, and * is
            ;; treated as multiplying scalar by 1-element matrix
            b (* 3 (dot d1 d2))
            c (- (* 3 (dot d1 d1))
                 (dot d2 d0))
            d (- (dot d1 d0)))
      ;; run SOLVE-CUBIC in scalar mode, so A,B,C,D are unwrapped to
      ;; double-floats, and OUT wasn't wrapped, so is used directly
      ;; (actually it was implicitly wrapped as a scalar, then
      ;; unwrapped to pass to solve-cubic)
      (let ((roots (as-scalar (solve-cubic a b c d out)))
            (dd most-positive-double-float))
        (declare (type (simple-array double-float (*)) roots)
                 (double-float dd))
        (loop for r across roots
              do (let ((b (eval-quadratic-bezier p0 p1 p2 r))
                       (d (dist p b)))
                   (when (< d (abs dd))
                     ;; WHEN converts to scalar mode, so switch back to vector
                     (mprogn
                      (let* ((s (sign-at-curve c r
                                               (- b p)
                                               (* 2 d1))))
                        ;; don't mess with the SETF form
                        (as-scalar
                         (setf dd (* s d))))))))

        dd)))
