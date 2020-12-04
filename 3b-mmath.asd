(defsystem 3b-mmath
  :description "graphics math (small vectors/matrices etc) meta-library"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria #++ ieee-floats float-features nibbles cffi)
  :serial t
  :components ((:file "package")
               (:file "misc")
               (:file "opt")
               (:file "accessors")
               (:file "permute")
               (:file "matrix")
               (:file "lib")))
