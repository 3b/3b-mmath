(defsystem 3b-mmath
  :description "Experimental meta-math-library for computer graphics"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria 3b-walker introspect-environment)
  :in-order-to ((asdf:test-op (asdf:test-op 3b-mmath/test)))
  :serial t
  :components ((:file "package")
               (:file "storage")
               (:file "permutations")
               (:file "broadcast")
               (:file "scalar")
               (:file "compiler-common")
               (:file "ops")
               (:file "transforms")
               (:file "dsl")
               (:file "mcompiler")
               (:file "ccompiler")))


(defsystem 3b-mmath/test
  :depends-on (3b-mmath nibbles parachute)
  :serial t
  :perform
  (asdf:test-op (op c) (uiop:symbol-call :parachute :test :3b-mmath/test))
  :components ((:file "test")))
