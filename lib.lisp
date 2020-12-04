(in-package #:3b-mmath/lib)

;;; library of code generators for more specific operations

(defmacro determinant (accessor &key ((:opt o:*opt*) t))
  (setf accessor (ag:accessor-designator accessor))
  ;; must be square matrix
  (assert (= (mi:matrix-type-rows (ag:mtype accessor))
             (mi:matrix-type-columns (ag:mtype accessor))))
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
    (r accessor)))

