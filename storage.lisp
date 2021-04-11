(in-package #:3b-mmath)


;;; abstraction for matrix storage, wraps variable(s) describing a
;;; storage location, and generates code for various operations like
;;; reading/writing elements, runtime checks to verify variables are
;;; correct type and large enough, etc


(defclass storage ()
  (;; # of valid indices we will accept (0 to size-1)
   (size :reader size :initarg :size)
   ;; if set, optionally generate COERCE forms as needed
   (element-type :reader element-type :initarg :element-type
                 :initform nil)))

(defmethod initialize-instance :after ((s storage) &key)
  (assert (typep (element-type s) '(or symbol (cons symbol))))
  (assert (typep (size s) `(integer (0) ,most-positive-fixnum))))

(defmethod write-element (n s index &rest r &key type
                          &allow-other-keys)
  ;;; by default, just generate a SETF form
  (let ((value (cond
                 ((and (numberp n) (element-type s))
                  (coerce n (element-type s)))
                 ((and (element-type s)
                       (not (eql (element-type s) t))
                       (or (not type)
                           (not (eql type (element-type s)))))
                  `(coerce ,n ',(element-type s)))
                 (t n)))
        (keys r))
    (remf keys :type)
   `(setf ,(apply #'read-element s index keys) ,value)))





;;; storage backed by (part of) a cl vector. Accesses SIZE elements
;;; starting at OFFSET

(defclass storage-cl-vector (storage)
  ;; accessor for SIZE elements stored in a CL VECTOR in STORAGE,
  ;; starting from OFFSET.
  ((storage :reader storage :initarg :storage)
   ;; compile-time constant value added to element index to get index
   ;; in storage vector
   (offset :reader offset :initarg :offset :initform 0)))

(defmethod initialize-instance :after ((s storage-cl-vector) &key)
  (assert (typep (offset s) `(integer 0 ,most-positive-fixnum))))

;;; generate code to verify all accesses we will generate are in
;;; bounds for provided vector
(defmethod bounds-check ((s storage-cl-vector))
  `(assert (array-in-bounds-p ,(storage s) ,(+ (offset s) (1- (size s))))))

;;; generate code to read an element
(defmethod read-element ((s storage-cl-vector) index &key skip-bounds-check)
  (assert (typep index `(integer 0 (,(size s)))))
  ;; if SKIP-BOUNDS-CHECK is set, we try to generate code that assumes
  ;; we know index refers to a valid element of VARIABLE, for example
  ;; by running code from BOUNDS-CHECK (and agreeing not to call
  ;; ADJUST-ARRAY on it)
  (if skip-bounds-check
      `(locally (declare #+sbcl (optimize
                         (sb-c::insert-array-bounds-checks 0)))
         (aref ,(storage s) ,(+ (offset s) index)))
      `(aref ,(storage s) ,(+ (offset s) index))))

;;; if possible, generate code to return the storage as a normal CL value
;; (won't be possible for example if storage is just a bunch of
;; variables/literals, or if it is
(defmethod return-storage ((s storage-cl-vector))
  (storage s))




;;; storage backed by explicitly provided forms per element. Elements
;;; can be literal numbers, variable names, or any code that can
;;; return a value. Doesn't attempt to determine if any element is
;;; actually writable, so attempting to write to it will only work if
;;; the written elements are valid places.
;;;

(defclass storage-literal (storage)
  ;; vector containing the elements
  ((elements :reader elements :initarg :elements)))

(defun coerce-numbers (x type)
  (loop for i from 0
        for e across x
        do (setf (aref x i)
                 (if (numberp e)
                     (coerce e type)
                     e)))
  x)

(defun make-storage-literal* (element-type elements)
  (make-instance 'storage-literal :size (length elements)
                                  :elements (coerce-numbers
                                             (coerce elements 'vector)
                                             element-type)
                                  :element-type element-type))

(defun make-storage-literal (element-type &rest elements)
  (make-storage-literal* element-type elements))

(defmethod read-element ((s storage-literal) index &key)
  (aref (elements s) index))
