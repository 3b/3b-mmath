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
   #:intern-matrix-type*))

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
   #:remove-row+column))

(defpackage #:3b-mmath/matrix
  (:use :cl #:3b-mmath/misc #:3b-mmath/opt)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:ag #:3b-mmath/accessor-generator))
  (:export
   #:matrix-mult
   #:per-element
   #:cross))


(defpackage #:3b-mmath/lib
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:ag #:3b-mmath/accessor-generator)
                    (#:m #:3b-mmath/matrix)
                    (#:o #:3b-mmath/opt)
                    (#:mi #:3b-mmath/misc))
  (:export
   #:determinant))
