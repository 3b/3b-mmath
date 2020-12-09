(defpackage #:3b-mmath/misc
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2))
  (:export
   #:matrix-type
   #:*matrix-types*
   #:intern-matrix-type
   #:half-ref/le
   #:half-ref/be
   #:index
   #:matrix-type-rows
   #:matrix-type-columns
   #:matrix-type-type
   #:matrix-type-row-major
   #:matrix-type-elements
   #:nibbles-accessor
   #:size-for-type
   #:canonical-endian
   #:matrix-type-stride
   #:matrix-type-octets
   #:all-of-type
   #:accesses-overlap
   #:matrix-type-designator
   #:nibbles-setter
   #:matrix-type-permutation
   #:ffi-type
   #:ulp=
   #:intern-matrix-type*
   #:matrix-type-dimensions))

(defpackage #:3b-mmath/opt
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2))
  (:export
   #:opt
   #:*opt*
   #:itype
   #:*itype*))

(defpackage #:3b-mmath/accessor-generator
  (:use :cl #:3b-mmath/misc #:3b-mmath/opt)
  (:local-nicknames (#:a #:alexandria-2))
  (:export
   #:literal
   #:vec
   #:bvec
   #:ffivec
   #:accesses-overlap
   #:access
   #:mtype
   #:range
   #:check
   #:declares
   #:alloc
   #:ret
   #:allocs
   #:binds
   #:struct
   #:literal/rot
   #:scalar
   #:accessor-designator
   #:submatrix
   #:row
   #:column
   #:antidiagonal
   #:diagonal
   #:submatrix*
   #:transpose
   #:remove-row+column
   #:accessor
   #:make-checked-accessor))

(defpackage #:3b-mmath/util
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:ag #:3b-mmath/accessor-generator)
                    (#:mi #:3b-mmath/misc))
  (:export
   #:checked
   #:row-matrix-accessor-p
   #:same-size-matrix-accessor-p
   #:column-matrix-accessor-p
   #:with-checked-accessors
   #:with-sources
   #:with-dest
   #:with-accessor-designators
   #:with-column-vectors
   #:assert-same-shape
   #:with-operands
   #:assert-square-matrices
   #:with-overlap-checks))

(defpackage #:3b-mmath/matrix
  (:use :cl #:3b-mmath/misc #:3b-mmath/opt)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:ag #:3b-mmath/accessor-generator)
                    (#:mu #:3b-mmath/util))
  (:export
   #:matrix-mult
   #:per-element
   #:cross
   #:filter
   #:per-element*))

(defpackage #:3b-mmath/lib
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:ag #:3b-mmath/accessor-generator)
                    (#:m #:3b-mmath/matrix)
                    (#:o #:3b-mmath/opt)
                    (#:mi #:3b-mmath/misc)
                    (#:mu #:3b-mmath/util))
  (:export
   #:determinant
   #:copy
   #:normalize))


(defpackage #:3b-mmath/lines
  (:use :cl #:3b-mmath/misc #:3b-mmath/opt)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:ag #:3b-mmath/accessor-generator)
                    (#:m #:3b-mmath/matrix)
                    (#:l #:3b-mmath/lib)
                    (#:mu #:3b-mmath/util))
  (:export
   #:interpolate-points))
