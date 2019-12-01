(in-package :day20)

(defun mk-pair (line)
  (let* ((len (length line))
         (pos-dash (position #\- line))
         (lo (make-array pos-dash
                         :element-type 'character
                         :displaced-to line))
         (hi (make-array (- len pos-dash 1)
                         :element-type 'character
                         :displaced-to line
                         :displaced-index-offset (1+ pos-dash))))
    (list (parse-integer lo) (parse-integer hi))))

(defun read-input (pathstr)
  (with-open-file (s pathstr)
    (sort (loop for line = (read-line s nil nil) while line
             collect (mk-pair line))
          #'< :key #'first)))

(defun find-smallest-ip (ranges)
  (let* ((first (first ranges))
         (rest (rest ranges))
         (candidate (1+ (second first))))
    (loop for ran in rest do
         (destructuring-bind (lo hi)
             ran
           (when (<= lo candidate hi)
             (setf candidate (1+ hi)))))
    candidate))

(defun sol1 ()
  (-<> (read-input "./inputs/day20")
       (find-smallest-ip <>)))

(defun overlap-p (p q)
  "Checks whether ranges `p=(p-lo p-hi)' and `q=(q-lo q-hi)' overlap.
It must hold that `p-lo' <= `q-lo'."
  (let ((p-lo (first p)) (p-hi (second p))
        (q-lo (first q)))
    (<= p-lo q-lo p-hi)))

(defun merge-ranges (ranges)
  "Merges sorted (by first component) ranges."
  (let ((curr (first ranges))
        (rans (cdr ranges))
        (merged nil))
    (loop for r in rans do
         (cond ((overlap-p curr r)
                (setf curr (list (first curr) (max (second curr) (second r)))))
               (t (push curr merged)
                  (setf curr r)))
         finally (push curr merged))
    (nreverse merged)))

(defun blocked (non-overlapping-ranges)
  (loop for (r-lo r-hi) in non-overlapping-ranges sum (1+ (- r-hi r-lo))))

(defun sol2 ()
  (-<> (read-input "./inputs/day20")
       (merge-ranges <>)
       (blocked <>)
       (- (expt 2 32) <>)))
