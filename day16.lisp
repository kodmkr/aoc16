(in-package :day16)

(defparameter +input+ "11101000110010100")

(defparameter +target-length-1+ 272)

(defparameter +target-length-2+ 35651584)

;; TODO: try streams which write to several places at once
;; TODO: calculate final size in advance
(defun dragon (a)
  (let* ((b (nreverse (copy-seq a)))
         (len (array-dimension a 0))
         (new (make-array (1+ (* 2 len)) :element-type 'character))
         (idx 0))
    (loop for i below len do
         (setf (aref new idx) (aref a i))
         (incf idx))
    (setf (aref new idx) #\0)
    (incf idx)
    (loop for i from 0 below len do
         (setf (aref new idx) (if (char= (aref b i) #\1) #\0 #\1))
         (incf idx))
    new))

(defun one-checksum (bitstr)
  (let* ((len (array-dimension bitstr 0))
         (checksum (make-array (floor len 2) :element-type 'character)))
    (loop
       for j from 0
       for i = 0 then (+ i 2) while (< i (1- len)) do
         (let ((window (make-array 2
                                   :element-type 'character
                                   :displaced-to bitstr
                                   :displaced-index-offset i)))
           (setf (aref checksum j)
                 (if (char= (aref window 0) (aref window 1))
                     #\1
                     #\0))))
    checksum))

(defun fill-target-length-array (target-length init)
  (let ((target (make-array target-length :element-type 'character))
        (dragon init)
        ;; `times' comes from a formula for the length after 'n' iterations
        ;; l_n = 2^n * (s + 1) + 1 , where `l_n' is the length after `n' iterations
        ;;                           and `s' is the length if the inital string
        (times (ceiling (/ (log (/ (1+ target-length) (1+ (length init)))) (log 2)))))
    (loop repeat times  do
         (setf dragon (dragon dragon)))
    (loop for i below target-length do
         (setf (aref target i) (aref dragon i)))
    target))
    
(defun checksum (bitstr)
  (let ((cs (one-checksum bitstr)))
    (loop while (evenp (length cs)) do
         (setf cs (one-checksum cs)))
    cs))

(defun sol1 ()
  (-<> (fill-target-length-array +target-length-1+ +input+)
       (checksum <>)))

;; ~30s on CCL and ~7s on SBCL
(defun sol2 ()
  (-<> (fill-target-length-array +target-length-2+ +input+)
       (checksum <>)))
