#++ (ql:quickload '(alexandria ieee-floats float-features nibbles cffi))
(in-package #:3b-mmath/matrix)

(defmacro matrix-mult (dest m1 m2 &key ((opt *opt*) t))
  "Generate code to multiply matrices specified by accessors M1 and
  M2, storing result in matrix defined by accessor DEST. If DEST is
  eql to M1 or M2, use temporaries as needed).

If OPT is set, optimize away multiplications by 0.0 and 1.0 even if
that would lose a NaN."
  (let* ((ad (ag:accessor-designator dest))
         (a1 (ag:accessor-designator m1))
         (a2 (ag:accessor-designator m2))
         (dest (ag:access ad))
         (m1 (ag:access a1))
         (m2 (ag:access a2))
         (ranged (ag:range ad))
         (typed (ag:mtype ad))
         (type1 (ag:mtype a1))
         (type2 (ag:mtype a2))
         (o1 (accesses-overlap ranged (ag:range a1)))
         (o2 (accesses-overlap ranged (ag:range a2)))
         (*itype* (itype (matrix-type-type type1)
                         (matrix-type-type type2)))
         (dtype (matrix-type-type typed))
         (in-place (cond
                     ((and o1 o2) 3)
                     (o1 1)
                     (o2 2)
                     (t nil)))
         (r1 (matrix-type-rows type1))
         (c1 (matrix-type-columns type1))
         (r2 (matrix-type-rows type2))
         (c2 (matrix-type-columns type2))
         (rd (matrix-type-rows typed))
         (cd (matrix-type-columns typed)))
    (assert (all-of-type 'function dest m1 m2))
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
    (print
     `(let (,@(ag:binds ad)
            ,@(ag:binds a1)
            ,@(ag:binds a2))
        ,(ag:check a1)
        ,(ag:check a2)
        ,(ag:check ad)
        ;; todo: when in-place, generate row or column major order
        ;; depending on which matrix is dest, to reduce # of tem
        ;; vars needed (should only need enough for 1 row or column,
        ;; depending on which is dest?)

        ;; when in-place, possibly also generate each row/group in a
        ;; separate LET, so the temps are never written and are
        ;; clearly scoped? need to see if that affects compile-time
        ;; or run-time performance
        (locally (declare ,@(ag:declares ad)
                          ,@(ag:declares a1)
                          ,@(ag:declares a2)
                          #+sbcl
                          (optimize
                           (sb-c::insert-array-bounds-checks 0)))
          ,@(loop
              for i below rd
              append (loop
                       for j below cd
                       collect
                       `(setf
                         ,(opt (funcall dest i j))
                         ,(opt `(coerce
                                 (+
                                  ,@(loop for k below c1
                                          collect
                                          `(* ,(funcall m1 i k)
                                              ,(funcall m2 k j))))
                                 ,dtype))))))
        ,(ag:ret ad)))))

(defmacro per-element (op dest m1 m2 &key ((opt *opt*) t))
  " generate code to apply OP to corresponding elements of matrices
  specified by accessors M1 and M2, storing result in matrix defined
  by accessor DEST.

If OPT is set, optimize away multiplications by 0.0 and 1.0 even if
that would lose a NaN."
  (flet ((aq (x)
           (apply #'funcall x)))
    (let* ((ad (aq dest))
           (a1 (aq m1))
           (a2 (aq m2))
           (dest (ag:access ad))
           (m1 (ag:access a1))
           (m2 (ag:access a2))
           (ranged (ag:range ad))
           (typed (ag:mtype ad))
           (type1 (ag:mtype a1))
           (type2 (ag:mtype a2))
           (o1 (accesses-overlap ranged (ag:range a1)))
           (o2 (accesses-overlap ranged (ag:range a2)))
           (*itype* (itype (matrix-type-type type1)
                           (matrix-type-type type2)))
           (dtype (matrix-type-type typed))
           (in-place (cond
                       ((and o1 o2) 3)
                       (o1 1)
                       (o2 2)
                       (t nil)))
           (r1 (matrix-type-rows type1))
           (c1 (matrix-type-columns type1))
           (r2 (matrix-type-rows type2))
           (c2 (matrix-type-columns type2))
           (rd (matrix-type-rows typed))
           (cd (matrix-type-columns typed)))
      (declare (ignorable in-place))
      (assert (all-of-type 'function dest m1 m2))
      ;; matrices all same size
      (assert (= r1 r2 rd))
      (assert (= c1 c2 cd))

      `(let (,@(ag:binds ad)
             ,@(ag:binds a1)
             ,@(ag:binds a2))
         ,(ag:check a1)
         ,(ag:check a2)
         ,(ag:check ad)
         ;; todo: generate in row or column-major order depending on
         ;; layout of matrices for better cache use?

         (locally (declare ,@(ag:declares ad)
                           ,@(ag:declares a1)
                           ,@(ag:declares a2)
                           #+sbcl
                           (optimize
                            (sb-c::insert-array-bounds-checks 0)))
           ,@(loop
               for i below rd
               append (loop
                        for j below cd
                        collect
                        `(setf
                          ,(opt (funcall dest i j))
                          ,(opt `(coerce
                                  (,op
                                      ,(funcall m1 i j)
                                      ,(funcall m2 i j))
                                  ,dtype))))))
         ,(ag:ret ad)))))


(defmacro cross (dest m1 m2 &key ((opt *opt*) t))
  " generate code to apply OP to corresponding elements of 3x1
  matrices specified by accessors M1 and M2, storing result in 3x1
  matrix defined by accessor DEST.

If OPT is set, optimize away multiplications by 0.0 and 1.0 even if
that would lose a NaN."
  (let* ((ad (ag:accessor-designator dest))
         (a1 (ag:accessor-designator m1))
         (a2 (ag:accessor-designator m2))
         (dest (ag:access ad))
         (m1 (ag:access a1))
         (m2 (ag:access a2))
         (ranged (ag:range ad))
         (typed (ag:mtype ad))
         (type1 (ag:mtype a1))
         (type2 (ag:mtype a2))
         (o1 (accesses-overlap ranged (ag:range a1)))
         (o2 (accesses-overlap ranged (ag:range a2)))
         (*itype* (itype (matrix-type-type type1)
                         (matrix-type-type type2)))
         (dtype (matrix-type-type typed))
         (in-place (cond
                     ((and o1 o2) 3)
                     (o1 1)
                     (o2 2)
                     (t nil)))
         (r1 (matrix-type-rows type1))
         (c1 (matrix-type-columns type1))
         (r2 (matrix-type-rows type2))
         (c2 (matrix-type-columns type2))
         (rd (matrix-type-rows typed))
         (cd (matrix-type-columns typed)))
    (assert (all-of-type 'function dest m1 m2))
    ;; matrices all 3x1 ;; todo: support 1x3 and mixed?
    (assert (= r1 r2 rd 3))
    (assert (= c1 c2 cd 1))
    (when in-place
      (error "todo in-place"))
    (flet ((x (d a b)
             `(setf
               ,(opt (funcall dest d 0))
               ,(opt `(coerce
                       (- (* ,(funcall m1 a 0) ,(funcall m2 b 0))
                          (* ,(funcall m1 b 0) ,(funcall m2 a 0)))
                       ,dtype)))))
      `(let (,@(ag:binds ad)
             ,@(ag:binds a1)
             ,@(ag:binds a2))
         ,(ag:check a1)
         ,(ag:check a2)
         ,(ag:check ad)
         (locally (declare ,@(ag:declares ad)
                           ,@(ag:declares a1)
                           ,@(ag:declares a2)
                           #+sbcl
                           (optimize
                            (sb-c::insert-array-bounds-checks 0)))
           ,(x 0 1 2)
           ,(x 1 2 0)
           ,(x 2 0 1)
           ,(ag:ret ad))))))
