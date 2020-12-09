(in-package #:3b-mmath/util)

;; library of macros and functions used to build code-gen macros

(define-symbol-macro _checked_ nil)

(defmacro sm-value (var &environment env)
  (macroexpand var env))

#++
(defun checked (var)
  (and (sm-value '_checked_)
       (gethash var (sm-value '_checked_))))

#++(defmacro set-checked (v)
  `(setf (gethash ,v (sm-value _checked_)) t))

(defun row-matrix-accessor-p (accessor)
  (= 1 (mi:matrix-type-rows (ag:mtype accessor))))

(defun same-size-matrix-accessor-p (a b)
  (and (= (mi:matrix-type-rows (ag:mtype a))
          (mi:matrix-type-rows (ag:mtype b)))
       (= (mi:matrix-type-columns (ag:mtype a))
          (mi:matrix-type-columns (ag:mtype b)))))

(defun column-matrix-accessor-p (accessor)
  (= 1 (mi:matrix-type-columns (ag:mtype accessor))))

(defmacro with-sources ((&rest sources) &body body)
  `(let (,@(loop for s in sources append (ag:binds s)))
     ,@(loop for s in sources collect (ag:check s))
     (locally (declare ,@(loop for s in sources append (ag:declares s))
                       #+sbcl
                       (optimize
                        (sb-c::insert-array-bounds-checks 0))))
     ,@body))
(defmacro with-dest ((dest) &body body &environment env)
  (let ((checked (macroexpand '_checked_ env)))
    (if (find dest checked)
        `(progn :wdx ,@body)
        (progn
          `(let (,@(ag:binds dest))
             :wd
             ,(ag:check dest)
             (locally (declare ,@(ag:declares dest)
                               #+sbcl
                               (optimize
                                (sb-c::insert-array-bounds-checks 0)))
               (symbol-macrolet ((_checked_ ,(cons dest checked)))
                 #++(setf (gethash ,dest (sm-value _checked_)) t)
                 #++(set-checked ,dest)
                 ,@body)
               ,@(unless (find dest checked)
                   (list (ag:ret dest)))))))))

(defmacro with-operands ((dest &rest sources) &body body)
  `(with-dest (,dest)
     (with-sources (,@sources)
       ,@body)))

(defmacro with-accessor-designators ((&rest a) &body body)
  `(progn
     ,@(loop for i in a collect `(setf ,i (ag:accessor-designator ,i)))
     ,@body))
(defmacro with-column-vectors ((m1 &rest m) &body body)
  (setf m (list* m1 m))
  `(progn
     ;; each vector must be row or column matrrix
     ,@(loop for i in m
             collect `(assert (or (row-matrix-accessor-p ,i)
                                  (column-matrix-accessor-p ,i))))
     ;; access as column matrix to simplify body
     ,@(loop for i in m
             collect `(when (row-matrix-accessor-p ,i)
                        (setf ,i (ag:transpose ,i))))
     ,@body))

(defun assert-same-shape (&rest accessors)
  (or (not accessors)
      (let* ((mts (mapcar 'ag:mtype accessors))
             (r (mi:matrix-type-rows (car mts)))
             (c (mi:matrix-type-columns (car mts))))
        (loop for i in (cdr mts)
              unless (and (= r (mi:matrix-type-rows i))
                          (= c (mi:matrix-type-columns i)))
                do (error "matrices must have same dimensions~%~s~%~s"
                          (mapcar 'mi:matrix-type-dimensions mts)
                          accessors)))))

(defun assert-square-matrices (&rest accessors)
  (let* ((mts (mapcar 'ag:mtype accessors)))
    (loop for i in mts
          do (unless (= (mi:matrix-type-rows i)
                        (mi:matrix-type-columns i))
               (error "matrices must be square~%~s~%~s"
                      (mapcar 'mi:matrix-type-dimensions mts)
                      accessors)))))

(defmacro with-overlap-checks ((var dest &rest sources) &body body)
  (multiple-value-bind (body decl) (a:parse-body body)
    `(let ((,var nil))
       ,@decl
       ,@(loop for s in sources
               for i from 0
               collect `(when (ag:accesses-overlap (ag:range ,dest)
                                                   (ag:range ,s))
                          (setf ,var (logior (or ,var 0)
                                             ,(ash 1 i)))))
       ,@body)))
