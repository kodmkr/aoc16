(in-package :day18)

(defun read-input (pathstr)
  (with-open-file (s pathstr)
    (read-line s)))

(defparameter +test-1+ "..^^.")

(defun unsafe-p (line pos)
  (char= (char line pos) #\^))

(defun safe-p (line pos)
  (char= (char line pos) #\.))

(defun check-unsafe (line pos)
  (let ((llen (length line)))
    (cond ((zerop pos)
           (or (and (unsafe-p line pos)
                    (unsafe-p line (1+ pos)))
               (and (safe-p line pos)
                    (unsafe-p line (1+ pos)))))
          ((= pos (1- llen))
           (or (and (unsafe-p line pos)
                    (unsafe-p line (1- pos)))
               (and (safe-p line pos)
                    (unsafe-p line (1- pos)))))
          (t (or (and (unsafe-p line (1- pos))
                      (unsafe-p line pos)
                      (safe-p line (1+ pos)))
                 (and (safe-p line (1- pos))
                      (unsafe-p line pos)
                      (unsafe-p line (1+ pos)))
                 (and (unsafe-p line (1- pos))
                      (safe-p line pos)
                      (safe-p line (1+ pos)))
                 (and (safe-p line (1- pos))
                      (safe-p line pos)
                      (unsafe-p line (1+ pos))))))))

(defun next-line (prev-line)
  (let* ((len (length prev-line))
         (new-line (make-array len :element-type 'character))
         (safe-count 0))
    (loop for i below len do
         (setf (aref new-line i)
               (if (check-unsafe prev-line i)
                   #\^
                   (progn
                     (incf safe-count)
                     #\.))))
    (values new-line safe-count)))

(defun count-safe (initial times)
  (let ((safe-count 0)
        (current-line initial))
    (loop repeat times do
         (multiple-value-bind (new-line safe-count-per-line)
             (next-line current-line)
           (incf safe-count safe-count-per-line)
           (setf current-line new-line)))
    (+ safe-count (count #\. initial))))
    
(defun sol1 ()
  (-<> (read-input "./inputs/day18")
       (count-safe <> 39)))

(defun sol2 ()
  (-<> (read-input "./inputs/day18")
       (count-safe <> 399999)))
