#++ (ql:quickload '3b-mmath)
(in-package #:3b-mmath/misc)

(deftype unsigned-fixnum () '(and fixnum unsigned-byte))

;; description of a matrix, including row/column vectors

(defstruct matrix-type
  (rows 4 :type (and fixnum (integer 1)))
  (columns 4 :type (and fixnum (integer 1)))
  (type 'single-float :type symbol)
  (row-major nil :type (or t nil))
  (stride 4 :type (and fixnum (integer 1)))
  (permutation (%permute/major 4 4 nil) :type (simple-array unsigned-fixnum 2)))

(deftype matrix-type-designator () '(or matrix-type symbol cons))
;; possibly some of these should be stored directly in struct?
(defun matrix-type-elements (matrix-type)
  (* (matrix-type-rows matrix-type)
     (matrix-type-columns matrix-type)))

(defun matrix-type-octets (matrix-type)
  (* (matrix-type-elements matrix-type)
     (matrix-type-stride matrix-type)))


(defparameter *matrix-types* (make-hash-table :test 'equalp))

(defun intern-matrix-type (rows columns
                           &key (type 'single-float) (row-major nil)
                             (stride (if type (size-for-type type) 1)))
  (a:ensure-gethash (list rows columns type row-major stride)
                    *matrix-types*
                    (make-matrix-type :rows rows :columns columns
                                      :type type :row-major row-major
                                      :stride stride
                                      :permutation (%permute/major
                                                    rows columns row-major))))

(defun intern-matrix-type* (rows columns base-type &key (row-major nil rmp))
  (intern-matrix-type rows columns
                      :type (matrix-type-type base-type)
                      :row-major (if rmp
                                     row-major
                                     (matrix-type-row-major base-type))))


