#++ (ql:quickload '(alexandria ieee-floats float-features nibbles cffi))
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
  ((access :reader access :initarg :access)
   (mtype :reader mtype :initarg :type)
   (range :reader range :initarg :range :initform nil)
   (check :reader check :initarg :check :initform nil)
   (declares :reader declares :initarg :declare :initform nil)
   (binds :reader binds :initarg :binds :initform nil)
   (ret :reader ret :initarg :ret :initform nil)))

(defun make-accessor (access mtype &key range check declare binds ret)
  (make-instance 'accessor :access access :type mtype
                           :range range :check check :declare declare
                           :binds binds :ret ret))

(defun scalar (matrix-type element)
  (setf matrix-type
        (matrix-type-designator matrix-type))
  (let ((element
          (let ((type (matrix-type-type matrix-type))
                (i element))
            (cond
              ((and type (numberp i))
               (coerce i type))
              (type
               `(coerce ,i ,type))
              (t i)))))
    (make-accessor
     (lambda (i-row j-col)
       (declare (ignore i-row j-col))
       element)
     matrix-type)))

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
                   `(coerce ,i ,type))
                  (t i)))))
    (make-accessor
     (lambda (i-row j-col)
       (elt elements (index matrix-type i-row j-col :row-major t)))
     matrix-type)))


(defun literal/rot (matrix-type radians axis)
  ;; todo: accept (sin cos) in place of radians, for cases where they
  ;; are already known or will be reused
  (setf matrix-type (matrix-type-designator matrix-type))
  (let* ((c (gensym "C"))
         (s (gensym "S"))
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
    (make-accessor
     (lambda (i-row j-col)
       (elt elements (index matrix-type i-row j-col :row-major t)))
     matrix-type
     :binds `((,c (cos ,radians))
              (,s (sin ,radians))))))


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
                   `(coerce ,i ,type))
                  (t i)))))
    (make-accessor
     (lambda (i-row j-col)
       (elt elements (index matrix-type i-row j-col :row-major t)))
     matrix-type
     :ret struct)))

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
in V.

Returns an ACCESSOR object containing info needed to access the matrix"
  (setf type (matrix-type-designator type))
  (let* ((size (* (matrix-type-elements type)))
         (default-stride (matrix-type-elements type))
         (start (+ ofs (* base (or stride default-stride))))
         (end (+ start size -1)))
    (make-accessor
     (lambda (i-row j-col)
       (let ((x (index type i-row j-col)))
         (if (and (eql base 0)
                  (eql stride nil)
                  (eql ofs 0))
             ;; generate simpler code for common case
             `(aref ,v ,x)
             ;; generate code for general case
             `(aref ,v ,(opt `(+ ,x ,start))))))
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
   containing multiple sets of data.

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
  (let* ((accessor (nibbles-accessor (matrix-type-type type) endian))
         (default-stride (matrix-type-octets type))
         (default-element-stride (matrix-type-stride type))
         (size (matrix-type-octets type))
         (start (opt (print `(+ ,ofs (* ,base ,(or default-stride stride))))))
         (end (opt `(+ ,start ,size -1))))
    (make-accessor
     (lambda (i-row j-col)
       (let ((x (index type i-row j-col)))
         (if (and (eql base 0)
                  (eql ofs 0)
                  (or (not stride)
                      (eql stride default-stride))
                  (or (not element-stride)
                      (eql element-stride default-element-stride)))
             ;; generate simpler code for simple case
             ;; element-stride known to be a constant number
             `(,accessor ,v ,(* x element-stride))
             ;; generate code for general case
             (let* ((element-stride (or element-stride default-element-stride)))
               `(,accessor ,v ,(opt (print `(+ (* ,x ,element-stride)
                                               ,start))))))))
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

(defun ffivec (p type &key (base 0) (stride 1) (ofs 0)
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
  (error "todo ~s"
         (list 'ffivec p type
               :base base :stride stride :ofs ofs
               :element-stride element-stride :buffer-size buffer-size)))
