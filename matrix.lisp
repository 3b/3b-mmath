#++ (ql:quickload '(alexandria ieee-floats float-features nibbles cffi))
(in-package #:3b-mmath/matrix)

(defmacro matrix-mult (dest m1 m2 &key ((:opt *opt*) t))
  "Generate code to multiply matrices specified by accessors M1 and
  M2, storing result in matrix defined by accessor DEST. If DEST is
  eql to M1 or M2, use temporaries as needed).

If OPT is set, optimize away multiplications by 0.0 and 1.0 even if
that would lose a NaN."
  (mu:with-accessor-designators (dest m1 m2)
    (mu::with-overlap-checks (in-place dest m1 m2)
      (let* ((ad (ag:access dest))
             (a1 (ag:access m1))
             (a2 (ag:access m2))
             (typed (ag:mtype dest))
             (type1 (ag:mtype m1))
             (type2 (ag:mtype m2))
             (*itype* (itype (matrix-type-type type1)
                             (matrix-type-type type2)))
             (dtype (matrix-type-type typed))
             (r1 (matrix-type-rows type1))
             (c1 (matrix-type-columns type1))
             (r2 (matrix-type-rows type2))
             (c2 (matrix-type-columns type2))
             (rd (matrix-type-rows typed))
             (cd (matrix-type-columns typed)))
        (assert (all-of-type 'function ad a1 a2))
        (format t " ~sx~s * ~sx~s -> ~sx~s~%"
                c1 r1 c2 r2 cd rd)
        ;; matrices can be multiplied
        (assert (= c1 r2))
        ;; destination is correct size
        (assert (= rd r1))
        (assert (= cd c2))
        (ecase in-place
          ;; check that dest is same shape and position as
          ;; source(s) itoverlaps.

          ;; todo: possibly allow overlap with same # of elements
          ;; and different shape? need to make sure we store
          ;; temporaries properly in that case

          ;; for now assuming overlap without being exact position
          ;; is an error, but possibly could support that too if
          ;; needed? (need to be even more careful about
          ;; temporaries in that case)
          (1 (error "todo in-place"))
          (2 (error "todo in-place"))
          (3 (error "todo in-place"))
          ((nil)))
        `(mu:with-operands (,dest ,m1 ,m2)
           ;; todo: when in-place, generate row or column major order
           ;; depending on which matrix is dest, to reduce # of tem
           ;; vars needed (should only need enough for 1 row or column,
           ;; depending on which is dest?)

           ;; when in-place, possibly also generate each row/group in a
           ;; separate LET, so the temps are never written and are
           ;; clearly scoped? need to see if that affects compile-time
           ;; or run-time performance
           ,@(loop
               for i below rd
               append (loop
                        for j below cd
                        collect
                        `(setf
                          ,(opt (funcall ad i j))
                          ,(opt `(coerce
                                  (+
                                   ,@(loop for k below c1
                                           collect
                                           `(* ,(funcall a1 i k)
                                               ,(funcall a2 k j))))
                                  ,dtype))))))))))


(defmacro per-element* (thunk dest m1 m2 &key ((:opt *opt*) t))
  " generate code to apply operation described by THUNK to
  corresponding elements of matrices specified by accessors M1 and M2,
  storing result in matrix defined by accessor DEST.

THUNK should be a function (designator) of 2 arguments, returning code
to apply some operation to elements specified by code in the 2
arguments (first argument is from M1, 2nd from M2)

If OPT is set, optimize away multiplications by 0.0 and 1.0 even if
that would lose a NaN."
  (unless (typep thunk 'function)
    (setf thunk (coerce thunk 'function)))
  (mu:with-accessor-designators (dest m1 m2)
    (let* ((ad (ag:access dest))
           (a1 (ag:access m1))
           (a2 (ag:access m2))
           (ranged (ag:range dest))
           (typed (ag:mtype dest))
           (type1 (ag:mtype m1))
           (type2 (ag:mtype m2))
           (o1 (accesses-overlap ranged (ag:range m1)))
           (o2 (accesses-overlap ranged (ag:range m2)))
           (*itype* (itype (matrix-type-type type1)
                           (matrix-type-type type2)))
           (dtype (matrix-type-type typed))
           (in-place (cond
                       ((and o1 o2) 3)
                       (o1 1)
                       (o2 2)
                       (t nil)))
           (rd (matrix-type-rows typed))
           (cd (matrix-type-columns typed)))
      (declare (ignorable in-place))
      (assert (all-of-type 'function ad a1 a2))
      (mu:assert-same-shape dest m1 m2)
      `(mu:with-operands (,dest ,m1 ,m2)
         ;; todo: generate in row or column-major order depending on
         ;; layout of matrices for better cache use?

         ,@(loop
             for i below rd
             append (loop
                      for j below cd
                      collect
                      `(setf
                        ,(opt (funcall ad i j))
                        ,(opt `(coerce
                                ,(funcall thunk
                                          (funcall a1 i j)
                                          (funcall a2 i j))
                                ,dtype)))))))))

(defmacro per-element (op dest m1 m2 &key ((:opt *opt*) t))
  " generate code to apply OP to corresponding elements of matrices
  specified by accessors M1 and M2, storing result in matrix defined
  by accessor DEST.

If OPT is set, optimize away multiplications by 0.0 and 1.0 even if
that would lose a NaN."
  `(per-element* ,#'(lambda (a b) `(,op ,a ,b))
                 ,dest ,m1 ,m2))

(defmacro filter (op dest m1 &key ((:opt *opt*) t))
  "generate code to apply OP to elements of matrix specified by
  accessors M1, storing result in matrix defined by accessor DEST.

If OP is IDENTITY or NIL, copy directly from M1 to DEST

If OPT is set, optimize away multiplications by 0.0 and 1.0 even if
that would lose a NaN."
  (mu:with-accessor-designators (dest m1)
    (mu:with-overlap-checks (in-place dest m1)
      ;;todo: handle or error on partial overlap
      (declare (ignorable in-place))
      (let* ((ad (ag:access dest))
             (a1 (ag:access m1))
             (typed (ag:mtype dest))
             (type1 (ag:mtype m1))
             (*itype* (matrix-type-type type1))
             (dtype (matrix-type-type typed))
             (rd (matrix-type-rows typed))
             (cd (matrix-type-columns typed)))
        (assert (all-of-type 'function ad a1))
        (mu:assert-same-shape dest m1)
        `(mu:with-operands (,dest ,m1)
           ;; todo: generate in row or column-major order depending on
           ;; layout of matrices for better cache use?
           ,@(loop
               for i below rd
               append (loop
                        for j below cd
                        collect
                        `(setf
                          ,(opt (funcall ad i j))
                          ,@ (list
                              (if (or (not op) (eql op 'identity))
                                  (opt `(coerce
                                         ,(funcall a1 i j)
                                         ,dtype))
                                  (opt `(coerce
                                         (,op
                                             ,(funcall a1 i j))
                                         ,dtype))))))))))))


(defmacro cross (dest m1 m2 &key ((:opt *opt*) t))
  " generate code to apply OP to corresponding elements of 3x1
  matrices specified by accessors M1 and M2, storing result in 3x1
  matrix defined by accessor DEST.

If OPT is set, optimize away multiplications by 0.0 and 1.0 even if
that would lose a NaN."
  (mu:with-accessor-designators (dest m1 m2)
    (mu:with-column-vectors (dest m1 m2)
      (mu:assert-same-shape dest m1 m2)
      (mu::with-overlap-checks (in-place dest m1 m2)
        (let* ((ad (ag:access dest))
               (a1 (ag:access m1))
               (a2 (ag:access m2))
               (*itype* (itype (matrix-type-type (ag:mtype m1))
                               (matrix-type-type (ag:mtype m2))))
               (dtype (matrix-type-type (ag:mtype dest))))
          (assert (all-of-type 'function ad a1 a2))
          ;; matrices all 3x1 (already made sure it is a column-vector
          (assert (= 3 (matrix-type-rows (ag:mtype dest))))
          (when in-place
            (error "todo in-place"))
          (flet ((x (d a b)
                   `(setf
                     ,(opt (funcall ad d 0))
                     ,(opt `(coerce
                             (- (* ,(funcall a1 a 0) ,(funcall a2 b 0))
                                (* ,(funcall a1 b 0) ,(funcall a2 a 0)))
                             ,dtype)))))
            `(mu:with-operands (,dest ,m1 ,m2)
               ,(x 0 1 2)
               ,(x 1 2 0)
               ,(x 2 0 1))))))))
