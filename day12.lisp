(in-package :day12)

(defun preprocess-line (line)
  (when line
    (destructuring-bind (instr . args) line
      (cons instr (loop
                     for arg in args
                     for a = (parse-integer arg :junk-allowed t)
                     if a collect a
                     else collect (char arg 0))))))

(defun read-input (inpath)
  (with-open-file (s inpath)
    (let* ((prog-list (loop
                         for line = (preprocess-line (split "\\s" (read-line s nil)))
                         while line
                         collect line))
           (pl-len (length prog-list)))
      (make-array pl-len :initial-contents prog-list))))

(defparameter +registers+ '(#\a #\b #\c #\d))

(defstruct machine
  (pc 0 :type integer)
  (program nil :type (array *))
  (registers nil :type hash-table))

(defun initial-machine (program &optional init-vals)
  (let ((m (make-machine
            :program program
            :registers (let ((ht (make-hash-table)))
                         (loop
                            for r in +registers+
                            do (setf (gethash r ht)
                                     (if init-vals
                                         (cdr (assoc r init-vals))
                                         0)))
                         ht))))
    m))

(defparameter *actions* (make-hash-table :test 'equal))

(defun val (value-or-register machine)
  (if (integerp value-or-register)
      value-or-register
      (gethash value-or-register (machine-registers machine))))

;; This macro does not seem to help much in terms of
;; avoiding repetituous code.
;; This is mainly because `jnz` breaks rank...
(defmacro define-instruction (name args &body body)
  `(setf (gethash ,name *actions*)
         (lambda (,@args)
           (declare (special m))
           ,@body)))

(define-instruction "cpy" (src dst)
  (setf (gethash dst (machine-registers m))
        (val src m))
  (incf (machine-pc m)))

(define-instruction "inc" (sd)
  (setf (gethash sd (machine-registers m))
        (incf (gethash sd (machine-registers m))))
  (incf (machine-pc m)))

(define-instruction "dec" (sd)
  (setf (gethash sd (machine-registers m))
        (decf (gethash sd (machine-registers m))))
  (incf (machine-pc m)))

(define-instruction "jnz" (flag offset)
  (if (zerop (val flag m))
      (incf (machine-pc m))
      (incf (machine-pc m) (val offset m))))

;; NOTE: declaring M special is pure laziness and due to
;; the not thought-through implementation of DEFINE-INSTRUCTION
(defun run-machine (m)
  (declare (special m))
  (let ((plen (length (machine-program m))))
    (loop
       while (< (machine-pc m) plen)
       do (destructuring-bind (instr . args)
              (aref (machine-program m) (machine-pc m))
            (apply (gethash instr *actions*) args)))
    (machine-registers m)))

(defun sol1 ()
  (-<> (read-input "./inputs/day12")
       (initial-machine <>)
       (run-machine <>)
       (gethash #\a <>)))

(defun sol2 ()
  (-<> (read-input "./inputs/day12")
       (initial-machine <> (pairlis +registers+ '(0 0 1 0)))
       (run-machine <>)
       (gethash #\a <>)))
