(defpackage #:3b-mmath
  (:use :cl)
  (:local-nicknames (#:a #:alexandria-2)
                    (#:w #:3b-walker))
  (:export
   #:per-element
   #:->
   #:reshape
   #:replace
   #:defmfun
   #:defcfun
   #:mm
   #:defwrap
   #:wrap
   #:cl-vector
   #:scalar
   #:reshape*
   #:make-literal-matrix
   #:add-meta-function))
