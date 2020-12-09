(in-package #:3b-mmath/lines)

;;; macros etc for various geometric operations like distance and intersection


(defclass composite-accessor ()
  ())

(defmethod validate-composite-accessor (a)
  ;; error if not valid, otherwise return t?
  t)
(defmethod initialize-instance :after ((a composite-accessor) &key)
  (validate-composite-accessor a))

;; types:

;; point (2+ element column vector)
(defclass point-accessor (composite-accessor)
  ((p0 :reader p0 :initarg :p0)))

(defun validate-vector-accessor (p)
  (assert (typep p 'ag:accessor))
  (assert (= (matrix-type-columns (ag:mtype p)) 1))
  (assert (>= (matrix-type-rows (ag:mtype p)) 2)))

(defmethod validate-composite-accessor ((a point-accessor))
  (setf (slot-value a 'p0) (ag:accessor-designator (p0 a)))
  (let ((p0 (p0 a)))
    ;; POINT-ACCESSOR must have a 2+ element column vector in p0
    (validate-vector-accessor p0)
    t))

;; ray (point + normalized vector)
(defclass ray-accessor (point-accessor)
  ((dir :reader dir :initarg :dir)))

(defmethod validate-composite-accessor ((a ray-accessor))
  (call-next-method)
  (setf (slot-value a 'dir) (ag:accessor-designator (dir a)))
  (let ((p0 (p0 a))
        (dir (dir a)))
    ;; RAY-ACCESSOR must have a 2+ element column vector in p0 and
    ;; dir, with same # of elements

    (validate-vector-accessor dir)
    (assert (= (matrix-type-rows (ag:mtype dir))
               (matrix-type-rows (ag:mtype p0))))
    t))


;; segment (point x 2)

(defclass segment-accessor (point-accessor)
  ((p1 :reader p1 :initarg :p1)))

(defmethod validate-composite-accessor ((a segment-accessor))
  (call-next-method)
  (setf (slot-value a 'p1) (ag:accessor-designator (p1 a)))
  (let ((p0 (p0 a))
        (p1 (p1 a)))
    ;; RAY-ACCESSOR must have a 2+ element column vector in p0 and
    ;; p1, with same # of elements

    (validate-vector-accessor p1)
    (assert (= (matrix-type-rows (ag:mtype p1))
               (matrix-type-rows (ag:mtype p0))))
    t))

;; quadratic bezier (point x 3)
(defclass bezier2-accessor (point-accessor)
  ((pc :reader pc :initarg :pc)
   (p1 :reader p1 :initarg :p1)))

(defmethod validate-composite-accessor ((a bezier2-accessor))
  (let ((p0 (p0 a))
        (p1 (p1 a))
        (pc (pc a)))
    ;; RAY-ACCESSOR must have a 2+ element column vector in p0 and
    ;; p1, with same # of elements
    (call-next-method)

    (validate-vector-accessor p1)
    (validate-vector-accessor pc)
    (assert (= (matrix-type-rows (ag:mtype p1))
               (matrix-type-rows (ag:mtype p0))
               (matrix-type-rows (ag:mtype pc))))
    t))



;; cubic bezier (point x 4)

(defclass bezier3-accessor (point-accessor)
  ((pc1 :reader pc1 :initarg :pc1)
   (pc2 :reader pc2 :initarg :pc2)
   (p1 :reader p1 :initarg :p1)))

(defmethod validate-composite-accessor ((a bezier3-accessor))
  (let ((p0 (p0 a))
        (p1 (p1 a))
        (pc1 (pc1 a))
        (pc2 (pc2 a)))
    ;; RAY-ACCESSOR must have a 2+ element column vector in p0 and
    ;; p1, with same # of elements
    (call-next-method)

    (validate-vector-accessor p1)
    (validate-vector-accessor pc1)
    (validate-vector-accessor pc2)
    (assert (= (matrix-type-rows (ag:mtype p1))
               (matrix-type-rows (ag:mtype p0))
               (matrix-type-rows (ag:mtype pc1))
               (matrix-type-rows (ag:mtype pc2))))
    t))


;; general bezier (point x N)

;; circular arcs?

;; parabola?


;;;; code generators for functions using above types

(defmacro eval-at/segment (dest segment at)
  (let* ((ad (ag:accessor-designator dest))
         (as (ag:accessor-designator segment))
         (dest (if (typep as 'point-accessor)
                   (p0 ad)
                   ad))
         (dtype (if (typep as 'point-accessor)
                    (ag:mtype (p0 ad))
                    (ag:mtype ad)))
         (elcount (matrix-type-rows dtype))
         (eltype (matrix-type-type dtype)))
    (assert (typep as 'segment-accessor))
    (a:with-gensyms (tmp)
      `(let ((,tmp (make-array ,elcount
                               :element-type ',eltype
                               :initial-element ,(coerce 0 eltype))))
         ;; only dx allocate if we are storing into specialized array,
         ;; since otherwise the values need to be heap-allocated (this
         ;; test is a bit permissive since user could pass an
         ;; unspecialized vector anyway, but hopefully compiler will
         ;; catch it in that case)
         ,@(when (and (not (eql eltype t))
                      (eql eltype
                           (upgraded-array-element-type eltype)))
             `((declare (dynamic-extent ,tmp))))
         (m:per-element - (ag:vec ,dtype ,tmp) (p1 ,as) (p0 ,as))
         #++
         (l:normalize (ag:vec ,dtype ,tmp) (ag:vec ,dtype ,tmp) :on-zero :skip)
         (m:per-element* ,#'(lambda (a b)
                              `(+ ,a (* ,b ,at)))
                         ,dest
                         (p0 ,as)
                         (ag:vec ,dtype ,tmp))))))
