(in-package #:3b-mmath/lib)

;;; library of code generators for more specific operations

(defmacro copy (dest src)
  `(m:filter nil ,dest ,src))

(defmacro normalize (dest src &key ((:opt o:*opt*) t) (on-zero :error))
  (mu:with-accessor-designators (dest src)
    (mu:with-column-vectors (dest src)
      (mu:assert-same-shape dest src)
      (let ((md (ag:mtype dest))
            (ms (ag:mtype src)))
        (a:with-gensyms (tmp block)
          `(mu:with-operands (,dest ,src)
             (block ,block
               (let((,tmp
                      (+ ,@(loop
                             for i below (mi:matrix-type-rows ms)
                             collect (funcall (ag:access src) i 0)))))
                 (declare (type ,(mi:matrix-type-type md) ,tmp))

                 (when (= 0 ,tmp)
                   ,(ecase on-zero
                      (:error
                       `(error "tried to normalize vector with length 0"))
                      (:skip `(return-from ,block nil))))
                 ,@(loop for i below (mi:matrix-type-rows ms)
                         collect `(setf ,(funcall (ag:access src) i 0)
                                        (/ ,(funcall (ag:access src) i 0)
                                           ,tmp)))))))))))

(defmacro determinant (accessor &key ((:opt o:*opt*) t))
  (mu:with-accessor-designators (accessor)
    (mu:assert-square-matrices accessor)
    (labels ((@ (a i j)
                 (funcall (ag:access a) i j))
             (r (a)
               (let ((c (mi:matrix-type-columns (ag:mtype a))))
                 (if (= c 2)
                     (o:opt `(- (* ,(@ a 0 0) ,(@ a 1 1))
                                (* ,(@ a 0 1) ,(@ a 1 0))))
                     (loop for x below c
                           for f = (o:opt `(* ,(@ a 0 x)
                                              ,(r (ag:remove-row+column
                                                   0 x a))))
                           when (evenp x)
                             collect f into plus
                           else
                             collect f into minus
                           finally
                              (return
                                (o:opt
                                 `(- (+ ,@plus)
                                     ,@minus))))))))
      (r accessor))))

(defmacro interpolate-points (d a b f)
  (mu:with-accessor-designators (a b d)
    (mu:with-column-vectors (a b d)
      (mu:assert-same-shape a b d)
      `(mu:with-operands (,d ,a ,b)
         (m:per-element* (lambda (a b)
                           `(+ ,a (* ,',f (- ,b ,a))))
                         ,d ,a ,b)))))


