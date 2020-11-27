(in-package #:3b-mmath/opt)

;; possibly should add another flag for specifying if N*0 = 0 even
;; with NaNs?  or just use this to mean that and do other
;; optimizations always?
(defparameter *opt* t)
(defparameter *itype* 'single-float)

(defun itype (t1 t2)
  (cond
    ((subtypep t1 t2) t2)
    ((subtypep t2 t1) t1)
    ((and (subtypep t1 'float)
          (not (subtypep t2 'float)))
     t1)
    ((and (subtypep t2 'float)
          (not (subtypep t1 'float)))
     t1)
    (t t)))

(defmethod %opt (opt &rest args)
  (list* opt (mapcar 'opt args)))

(defmethod %opt ((opt (eql '*)) &rest args)
  (let ((a (mapcar 'opt args)))
    (if *opt*
        (let ((a (remove-if (lambda (x) (and (numberp x)
                                             (= x 1)))
                            a)))
          (cond
            ((every 'numberp a)
             (apply '* a))
            ((some (lambda (x) (and (numberp x)
                                    (zerop x)))
                   a)
             (coerce 0 *itype*))
            ((eql a nil)
             (coerce 1 *itype*))
            ((eql (length a) 1)
             (car a))
            (t (list* opt a))))
        (list* opt a))))

(defmethod %opt ((opt (eql 'coerce)) &rest args)
  (destructuring-bind (v type) args
    (setf v (opt v))
    (setf type
          (if (typep type '(cons (eql quote)))
              (second type)
              type))
    (cond
      ((typep v type)
       v)
      ((numberp v)
       (coerce v type))
      (t
       `(coerce ,v ',type)))))

(defmethod %opt ((opt (eql '+)) &rest args)
  (let ((a (mapcar 'opt args)))
    (if *opt*
        (let ((a (remove-if (lambda (x) (and (numberp x)
                                             (= x 0)))
                            a)))
          (cond
            ((every 'numberp a)
             (apply '+ a))
            ((some 'numberp a)
             (let ((n (remove-if-not 'numberp a)))
               (if (= (length n) 1)
                   (list* opt a)
                   (list* opt (apply '+ n)  (remove-if 'numberp a)))))
            ((eql a nil)
             (coerce 0 *itype*))
            ((eql (length a) 1)
             (car a))
            (t (list* opt a))))
        (list* opt a))))

(defmethod %opt ((opt (eql '-)) &rest args)
  (let ((a (mapcar 'opt args)))(list* opt a))
  #++(let ((a (mapcar 'opt args)))
       (if *opt*
           (let ((a (remove-if (lambda (x) (and (numberp x)
                                                (= x 0)))
                               a)))
             (cond
               ((every 'numberp a)
                (apply '+ a))
               ((some 'numberp a)
                (let ((n (remove-if-not 'numberp a)))
                  (if (= (length n) 1)
                      (list* opt a)
                      (list* opt (apply '+ n)  (remove-if 'numberp a)))))
               ((eql a nil)
                (coerce 0 *itype*))
               ((eql (length a) 1)
                (car a))
               (t (list* opt a))))
           (list* opt a))))

(defmethod opt (op)
  op)

(defmethod opt ((op cons))
  (apply '%opt op))
