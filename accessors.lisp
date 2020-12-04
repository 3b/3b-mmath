#++ (ql:quickload '(3b-mmath))
(in-package #:3b-mmath/accessor-generator)

;; generators for accessors for various data formats (in a separate
;; package so the can have short names when used with
;; package-local-nicknames without conflicting with similar names in
;; other parts or targets)

;; should return a function of 1 argument that returns code for a
;; place representing the value at the specified index

;; note that these aren't expected to do once-only, since that should
;; be in user if needed, and keeping original form makes it easier for
;; compiler macros etc to detect constants

(defclass accessor ()
  ;; possibly should add an 'lvalue' test so we can error when dest is
  ;; a LITERAL with constant arguments? (should be valid to write to
  ;; one if all arguments are variables though, so possibly should
  ;; rename it?)
  ((linear-access :reader linear-access :initarg :linear-access)
   (access :reader access :initarg :access)
   (mtype :reader mtype :initarg :type)
   (range :reader range :initarg :range :initform nil)
   (check :reader check :initarg :check :initform nil)
   (declares :reader declares :initarg :declare :initform nil)
   (binds :reader binds :initarg :binds :initform nil)
   (ret :reader ret :initarg :ret :initform nil)
   (permute :accessor %permute :reader permute :initarg :permute)))

(defun make-accessor (access mtype &key range check declare binds ret permute)
  (let ((a (make-instance 'accessor
                          :linear-access access
                          :type mtype
                          :range range :check check :declare declare
                          :binds binds :ret ret
                          :permute (or permute
                                       (matrix-type-permutation mtype)))))
    (setf (slot-value a 'access)
          (lambda (i-row j-col)
            (funcall access (aref (permute a) i-row j-col))))
    a))

(defun make-permuted-accessor (accessor mtype permute)
  "make an accessor for a permutation of type MTYPE of the elements
accessible by ACCESSOR"
  (let ((a (make-instance 'accessor
                          :access nil
                          :linear-access (linear-access accessor)
                          :type mtype
                          :range (range accessor)
                          :check (check accessor) :declare (declares accessor)
                          :binds (binds accessor) :ret (ret accessor)
                          :permute (or permute
                                       (matrix-type-permutation mtype)))))
    (setf (slot-value a 'access)
          (lambda (i-row j-col)
            (funcall (linear-access a)
                     (aref (permute a) i-row j-col))))
    a))

(defun accessor-designator (ac)
  (etypecase ac
    (accessor
     ac)
    (cons
     (apply #'funcall ac))))

(defun scalar (matrix-type element)
  "Return accessor for a matrix of type MATRIX-TYPE, containing only ELEMENT.

If ELEMENT is a variable, it can be written to, but order of multiple
writes isn't guaranteed."
  (setf matrix-type
        (matrix-type-designator matrix-type))
  (let ((element
          (let ((type (matrix-type-type matrix-type))
                (i element))
            (cond
              ((and type (numberp i))
               (coerce i type))
              (type
               `(coerce ,i ',type))
              (t i)))))
    (make-accessor
     (lambda (x)
       (declare (ignore x))
       element)
     matrix-type)))

(defun scalar-out (matrix-type)
  "Return accessor for a matrix of type MATRIX-TYPE, containing a
  writable temp var, and set that temp var as the return value.

Reading contents without writing them is undefined (but probably will
return 0 of some type), so mostly intended for output of things like
dot product.
"
  (setf matrix-type
        (matrix-type-designator matrix-type))
  (let ((element (gensym "TMP")))
    (make-accessor
     (lambda (x)
       (declare (ignore x))
       element)
     matrix-type
     :binds `((,element ,(coerce 0 (matrix-type-type matrix-type))))
     :ret element)))

(defun force-row-major (accessor)
  ;; for literals, we always store data row-major, so overwrite
  ;; permutation from type
  (let ((m (mtype accessor)))
    (setf (slot-value accessor 'permute)
          (3b-mmath/misc::%permute/major (matrix-type-rows m)
                                         (matrix-type-columns m)
                                         t)))
  accessor)

(defun literal (matrix-type &rest elements)
  (setf matrix-type (matrix-type-designator matrix-type))
  (let ((elements
          (loop with type = (matrix-type-type matrix-type)
                for i in elements
                collect
                (cond
                  ((and type (numberp i))
                   (coerce i type))
                  (type
                   `(coerce ,i ',type))
                  (t i)))))
    (force-row-major (make-accessor
                      (lambda (x)
                        (elt elements x))
                      matrix-type))))


(defun literal/rot (matrix-type radians axis)
  "Return an accessor for a 2d (with axis = :Z) or 3d rotation of
RADIANS around AXIS"
  ;; todo: accept (sin cos) in place of radians, for cases where they
  ;; are already known or will be reused
  (setf matrix-type (matrix-type-designator matrix-type))
  (when (or (= (matrix-type-rows matrix-type) 2)
            (= (matrix-type-columns matrix-type) 2))
    (format t "~s,~s = ~s~%"
            (matrix-type-rows matrix-type) (matrix-type-columns matrix-type)
            axis)
    (assert (eql axis :z) nil "can't make rotation around ~s into ~s,~s matrix"
            axis
            (matrix-type-rows matrix-type) (matrix-type-columns matrix-type)))
  (let* ((c (gensym "C"))
         (s (gensym "S"))
         (type (matrix-type-type matrix-type))
         (elements
           (ecase axis
             ;; todo: accept axis = (x y z) for angle-axis?
             (:x `(1 0 0 0
                     0 ,c (- ,s) 0
                     0 ,s ,c 0
                     0 0 0 1))
             (:y `(,c 0 ,s 0
                      0 1 0 0
                      (- ,s) 0 ,c 0
                      0 0 0 1))
             (:z `(,c (- ,s) 0 0
                      ,s ,c 0 0
                      0 0 1 0
                      0 0 0 1)))))
    ;; not sure how 4d or larger rotation would be defined, or how to
    ;; distinguish 3d homogeneneous from 4d
    (assert (<= (matrix-type-rows matrix-type) 4))
    (assert (<= (matrix-type-columns matrix-type) 4))
    (make-accessor
     (lambda (x)
       (elt elements x))
     matrix-type
     :binds `((,c (coerce (cos ,radians) ',type))
              (,s (coerce (sin ,radians) ',type)))
     :permute (permute/sub matrix-type
                           0 0
                           (3b-mmath/misc::%permute/major 4 4 t)
                           nil))))


#++
(funcall (literal (intern-matrix-type 4 2)
                  0 1
                  2 3
                  4 5
                  6 7) 3 1)

#++
(let ((foo (make-array 3)))
  ;; vec-length is a macro using the accessor protocol
  (vec-length (vec foo)))

(defun struct (matrix-type struct &rest slots)
  (setf matrix-type (matrix-type-designator matrix-type))
  (let ((elements
          (loop with type = (matrix-type-type matrix-type)
                for s in slots
                for i = (if (symbolp s)
                            `(,s ,struct)
                            s)
                collect
                (cond
                  ((and type (numberp i))
                   (coerce i type))
                  (type
                   `(coerce ,i ',type))
                  (t i)))))
    (force-row-major
     (make-accessor
      (lambda (x)
        (elt elements x))
      matrix-type
      :ret struct))))

#++
(funcall (struct (intern-matrix-type 4 1) (make-foo 1 2 3)
                 'foo-x
                 'foo-y
                 'foo-z
                 1) 2 0)



(defun vec (type v &key (ofs 0) (base 0)
                     (stride nil))
  "Operate on matrix stored in CL vector V, optionally starting from
index (+ OFS (* BASE STRIDE)). Matrix is assumed to be tightly packed
in V, and stride defaults to number of elements in STYPE.

Returns an ACCESSOR object containing info needed to access the matrix"

  (setf type (matrix-type-designator type))
  (unless stride
    (setf stride (matrix-type-elements type)))
  (let* ((size (matrix-type-elements type))
         (default-stride (matrix-type-elements type))
         (start (+ ofs (* base (or stride default-stride))))
         (end (+ start size -1)))
    (make-accessor
     (lambda (x)
       (assert (<= 0 x (1- size)))
       (if (and (eql base 0)
                (eql ofs 0))
           ;; generate simpler code for common case
           `(aref ,v ,x)
           ;; generate code for general case
           `(aref ,v ,(opt `(+ ,x ,start)))))
     type
     :range (list v start end)
     :check (if (and (eql base 0) (eql ofs 0))
                `(assert (array-in-bounds-p ,v ,end))
                `(progn
                   (assert (array-in-bounds-p ,v ,start))
                   (assert (array-in-bounds-p ,v ,end))))
     :ret v)))

(defun alloc (matrix-type)
  "allocate a vector of specified type and return it"
  (let* ((n (gensym))
         (a (vec matrix-type n))
         (m (mtype a)))
    (setf (slot-value a 'binds)
          `((,n (make-array ,(matrix-type-elements m)
                            :element-type ',(matrix-type-type m)
                            :initial-element
                            ,(coerce 0 (matrix-type-type m))))))
    a))



(defun bvec (type v &key
                      (base 0)
                      (ofs 0) (endian :native)
                      (stride nil)
                      (element-stride nil))
  ;; add some type aliases like :ub32 :sb16 :single etc?
  "Operate on elements of TYPE extracted from a CL octet vector V,
   optionally starting from byte offset (+ OFS (* BASE STRIDE)),
   with each element ELEMENT-STRIDE bytes apart.

   STRIDE is intended to account for size/alignment of an aggregate
   containing multiple sets of data. Defaults to size of TYPE.

   BASE is intended to index which aggregate contains the data

   OFS is byte offset of the vector within the aggregate

   ELEMENT-STRIDE defaults to the size of the elements, and specifies
   spacing between elements within the vector

   TYPE should be (signed-byte x), (unsigned-byte x), x=8,16,32,64
   or :half-float, single-float, double-float

   ENDIAN should be :NATIVE, :BIG, or :LITTLE (at compile time, so not
   stored in a runtime variable)


Returns an ACCESSOR object containing info needed to access the matrix"
  (setf type (matrix-type-designator type))
  (unless stride
    (setf stride (matrix-type-octets type)))
  (let* ((accessor (nibbles-accessor (matrix-type-type type) endian))
         (default-stride (matrix-type-octets type))
         (default-element-stride (matrix-type-stride type))
         (size (matrix-type-octets type))
         (start (opt `(+ ,ofs (* ,base ,(or default-stride stride)))))
         (end (opt `(+ ,start ,size -1))))
    (make-accessor
     (lambda (x)
       (assert (<= 0 x (1- (matrix-type-elements type))))
       (if (and (eql base 0)
                (eql ofs 0)
                (or (not element-stride)
                    (eql element-stride default-element-stride)))
           ;; generate simpler code for simple case
           ;; element-stride known to be a constant number
           `(,accessor ,v ,(* x (or element-stride default-element-stride)))
           ;; generate code for general case
           (let* ((element-stride (or element-stride default-element-stride)))
             `(,accessor ,v ,(opt `(+ (* ,x ,element-stride)
                                      ,start))))))
     type
     :range (list v start end)
     :check (if (eql 0 start)
                `(assert (array-in-bounds-p ,v ,end))
                `(progn
                   (assert (array-in-bounds-p ,v ,start))
                   (assert (array-in-bounds-p ,v ,end))))
     :declare `((inline ,accessor
                        ,(nibbles-setter
                          (matrix-type-type type) endian))))))

;; todo: move FFI stuff to separate files+package
(defun ffivec (type p &key (base 0) (stride nil) (ofs 0)
                        element-stride
                        buffer-size)
  ;; add some type aliases like :ub32 :sb16 :single etc?
  "Operate on elements of TYPE extracted from a CFFI pointer P.
   optionally starting from byte offset (+ OFS (* BASE STRIDE)), with
   each element ELEMENT-STRIDE bytes apart. If BUFFER-SIZE is
   specified, it will be checked before any access.

   STRIDE is intended to account for size/alignment of an aggregate
   containing multiple sets of data.

   BASE is intended to index which aggregate contains the data

   OFS is byte offset of the vector within the aggregate

   ELEMENT-STRIDE defaults to the size of the elements, and specifies
   spacing between elements within the vector

   TYPE should be (signed-byte x), (unsigned-byte x), x=8,16,32,64 or
   :half-float, single-float, double-float

  ;; todo: add ENDIAN support?
"

  (setf type (matrix-type-designator type))
  (let* ((ffi-type (ffi-type (matrix-type-type type)))
         (stride (or stride (matrix-type-octets type)))
         (element-stride (or element-stride (matrix-type-stride type)))
         (size (matrix-type-octets type))
         (start (opt `(+ ,ofs (* ,base ,stride))))
         (end (opt `(+ ,start ,size -1))))
    (assert ffi-type)
    (assert (>= stride (matrix-type-octets type)))
    (assert (>= element-stride (cffi:foreign-type-size ffi-type)))
    (when (and buffer-size (numberp buffer-size))
      (when (numberp end)
        (assert (< end buffer-size)))
      (assert (<= stride buffer-size)))
    (make-accessor
     (lambda (x)
       (assert (<= 0 x (1- (matrix-type-elements type))))
       (if (and (eql base 0)
                (eql ofs 0)
                (or (eql element-stride (matrix-type-stride type))))
           ;; generate simpler code for simple case
           ;; element-stride known to be a constant number
           `(cffi:mem-ref ,p ',ffi-type ,(* x element-stride))
           ;; generate code for general case
           `(cffi:mem-ref ,p ',ffi-type ,(opt `(+ (* ,x ,element-stride)
                                                  ,start)))))
     type
     :range (list p start end)
     :check (when buffer-size
              (if (eql 0 start)
                  `(assert (< ,end ,buffer-size))
                  `(progn
                     (assert (< ,end ,buffer-size))
                     (assert (< ,end ,buffer-size))))))))
