(in-package :day02)

(defun read-intput (inpath)
  (with-open-file (s inpath)
    (loop
       for line = (read-line s nil nil)
       while line collect line)))

(defun setup-funcs-sq ()
  (let ((funcs (make-hash-table :test #'equal)))
    (macrolet ((mk-func (dirchar cond dir)
                 `(setf (gethash ,dirchar funcs)
                        #'(lambda (curr)
                            (if ,cond
                                (+ curr ,dir)
                                curr)))))
      (mk-func #\U (< (imagpart curr) 1) #c(0 1))
      (mk-func #\D (> (imagpart curr) -1) #c(0 -1))
      (mk-func #\L (> (realpart curr) -1) #c(-1 0))
      (mk-func #\R (< (realpart curr) 1) #c(1 0)))
    funcs))

(defun go-one (pos descr funcs)
  (loop for d across descr do
       (setf pos (funcall (gethash d funcs) pos)))
  pos)

(defun go-all (descrs funcs &key (start #c(0 0)))
  (let ((pos start))
    (loop for descr in descrs
       do
         (setf pos (go-one pos descr funcs))
       collect
         pos)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mod-pairs (alist acc)
  "ALIST *must* have even length"
  (if (null alist)
      (nreverse acc)
      (progn
        (let ((f (car alist))
              (s (cadr alist)))
          (mod-pairs (cddr alist)
                 (cons s (cons `(gethash ,f dict) acc))))))))

(defmacro define-translation (name-n-pairs)
  `(defun ,(intern (format nil "TRANSLATE-~a"
                           (symbol-name (car name-n-pairs))))
       (cpxs)
     (let ((dict (make-hash-table :test #'equal)))
       (setf ,@(mod-pairs (cdr name-n-pairs) nil))
       (coerce (loop for cpx in cpxs collect
                    (gethash cpx dict))
               'string))))

(define-translation
    (square
     #c(-1 1) #\1
     #c(0 1) #\2
     #c(1 1) #\3
     #c(-1 0) #\4
     #c(0 0) #\5
     #c(1 0) #\6
     #c(-1 -1) #\7
     #c(0 -1) #\8
     #c(1 -1) #\9))

(defun sol1 ()
  (-<> (read-intput "./inputs/day02")
       (go-all <> (setup-funcs-sq))
       (translate-square <>)))

(defun abs-sum (cpx)
  (+ (abs (realpart cpx))
       (abs (imagpart cpx))))

(defun in-circle-p (cpx &optional (rad 2))
  (<= (abs-sum cpx) rad))

(defun setup-funcs-rhomb ()
  (let ((funcs (make-hash-table :test #'equal)))
    (macrolet ((mk-func (dirchar dir)
                 `(setf (gethash ,dirchar funcs)
                        #'(lambda (curr)
                            (if (in-circle-p (+ curr ,dir))
                                (+ curr ,dir)
                                curr)))))
      (mk-func #\U #c(0 1))
      (mk-func #\D #c(0 -1))
      (mk-func #\L #c(-1 0))
      (mk-func #\R #c(1 0)))
    funcs))

(define-translation
    (rhomb
     #c(0 2) #\1
     #c(-1 1) #\2
     #c(0 1) #\3
     #c(1 1) #\4
     #c(-2 0) #\5
     #c(-1 0) #\6
     #c(0 0) #\7
     #c(1 0) #\8
     #c(2 0) #\9
     #c(-1 -1) #\A
     #c(0 -1) #\B
     #c(1 -1) #\C
     #c(0 -2) #\D))

(defun sol2 ()
  (-<> (read-intput "./inputs/day02")
       (go-all <> (setup-funcs-rhomb) :start #c(-2 0))
       (translate-rhomb <>)))
