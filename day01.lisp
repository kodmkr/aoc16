(in-package :day01)

(defclass context ()
  ((curr-dir :initform #c(0 1))
   (curr :initform #c(0 0))))

(defmethod go-left ((ctx context))
  (with-slots (curr-dir) ctx
    (setf curr-dir (* curr-dir #c(0 1)))
    ctx))

(defmethod go-right ((ctx context))
  (with-slots (curr-dir) ctx
    (setf curr-dir (* curr-dir #c(0 -1)))
    ctx))

(defmethod go-dir ((ctx context) amount)
  (with-slots (curr-dir curr) ctx
    (setf curr (+ curr (* amount curr-dir)))
    ctx))

(defun abs-sum (cpx)
  (+ (abs (realpart cpx))
       (abs (imagpart cpx))))

(defmethod dist-from-origin ((ctx context))
  (with-slots (curr) ctx
    (abs-sum curr)))

(defun read-input (inpath)
  (with-open-file (s inpath)
    (read-line s nil nil)))

(defun journey (path)
  (let ((ctx (make-instance 'context)))
    (do-register-groups
        ((#'(lambda (s) (coerce s 'character)) dir)
         (#'parse-integer amount))
        ("(R|L)(\\d+),?" path (dist-from-origin ctx))
      (assert (and dir amount))
      (if (char= dir #\R)
          (go-right ctx)
          (go-left ctx))
      (go-dir ctx amount))))

;; maybe keeping the last four segments and
;; computing an intersection is more elegant
;; than this...
(defun journey-precise (path)
  (let ((ctx (make-instance 'context))
        (seen (make-hash-table :test #'equal)))
      (do-register-groups
        ((#'(lambda (s) (coerce s 'character)) dir)
         (#'parse-integer amount))
        ("(R|L)(\\d+),?" path)
      (assert (and dir amount))
      (if (char= dir #\R)
          (go-right ctx)
          (go-left ctx))
      (with-slots (curr-dir curr) ctx
        (loop
           for step from 1 upto amount do
             (macrolet ((expan (amt)
                          `(+ curr (* ,amt curr-dir))))
               (multiple-value-bind (v p?) (gethash (expan step) seen)
                 (declare (ignore v))
                 (if p?
                     (return-from journey-precise (expan step))
                     (setf (gethash (expan step) seen) t))))))
      (go-dir ctx amount))))

(defun sol1 ()
  (-<> (read-input "./inputs/day01")
       (journey <>)))

(defun sol2 ()
  (-<> (read-input "./inputs/day01")
       (journey-precise <>)
       (abs-sum <>)))
