(in-package :day21)

(defun read-input (pathstr)
  (with-open-file (s pathstr)
    (loop for line = (read-line s nil nil)
       while line collect line)))

(defun %swap-positions (pos1 pos2 str)
  (rotatef (char str pos1) (char str pos2))
  str)

(defun %swap-letters (letter1 letter2 str)
  (rotatef (char str (position letter1 str))
           (char str (position letter2 str)))
  str)

(defun %rotate-amount (amount str)
  (alexandria:rotate str amount)
  str)

(defun %rotate-by-position (letter str)
  (let ((pos (position letter str)))
    (%rotate-amount (+ pos (if (>= pos 4) 2 1)) str))
  str)

(defun %reverse-positions (start end str)
  (let ((window (make-array (1+ (- end start))
                            :element-type 'character
                            :displaced-to str
                            :displaced-index-offset start)))
    (setf window (nreverse window)))
  str)
           
(defun %move-position (from to str)
  (let ((c (char str from)))
    (if (< from to)
        (loop for i from from below to do
             (setf (char str i) (char str (1+ i))))
        (loop for i from from above to do
             (setf (char str i) (char str (1- i)))))
    (setf (char str to) c))
  str)

(defun position-of-second-space (str)
  (position #\Space str :start (1+ (position #\Space str))))

(defun exec (operations string)
  (loop for op in operations do
       (let* ((len (length op))
              (splitting-index (position-of-second-space op))
              (key (make-array splitting-index
                               :element-type 'character
                               :displaced-to op))
              (rest (make-array (- len splitting-index 1)
                                :element-type 'character
                                :displaced-to op
                                :displaced-index-offset (1+ splitting-index))))
         (alexandria:switch (key :test #'string=)
           ("swap position" (cl-ppcre:register-groups-bind ((#'parse-integer xpos ypos))
                                ("(\\d+) .*? position (\\d+)" rest)
                              (setf string (%swap-positions xpos ypos string))))
           ("swap letter" (cl-ppcre:register-groups-bind (((lambda (m) (char m 0)) letter1 letter2))
                              ("(\\w) .*? letter (\\w)" rest)
                            (setf string (%swap-letters letter1 letter2 string))))
           ("rotate left" (cl-ppcre:register-groups-bind ((#'parse-integer amount))
                              ("(\\d+) step" rest)
                            (setf string (%rotate-amount (- amount) string))))
           ("rotate right" (cl-ppcre:register-groups-bind ((#'parse-integer amount))
                               ("(\\d+) step" rest)
                             (setf string (%rotate-amount amount string))))
           ("rotate based" (cl-ppcre:register-groups-bind (((lambda (m) (char m 0)) letter))
                               ("on position of letter (\\w)" rest)
                             (setf string (%rotate-by-position letter string))))
           ("reverse positions" (cl-ppcre:register-groups-bind ((#'parse-integer start end))
                                    ("(\\d+) through (\\d+)" rest)
                                  (setf string (%reverse-positions start end string))))
           ("move position" (cl-ppcre:register-groups-bind ((#'parse-integer from to))
                                ("(\\d+) to position (\\d+)" rest)
                              (setf string (%move-position from to string)))))))
  string)

(defun sol1 ()
  (-<> (read-input "./inputs/day21")
       (exec <> "abcdefgh")))

;; from rosetta code because of lazyness
(defun next-perm (vec cmp)  ; modify vector
  (declare (type (simple-array * (*)) vec))
  (macrolet ((el (i) `(aref vec ,i))
             (cmp (i j) `(funcall cmp (el ,i) (el ,j))))
    (loop with len = (1- (length vec))
       for i from (1- len) downto 0
       when (cmp i (1+ i)) do
         (loop for k from len downto i
            when (cmp i k) do
              (rotatef (el i) (el k))
              (setf k (1+ len))
              (loop while (< (incf i) (decf k)) do
                   (rotatef (el i) (el k)))
              (return-from next-perm vec)))))

(defun brute-force (target ops)
  (loop for str = "abcdefgh" then (next-perm str #'char<) do
       (if (string= target (exec ops (copy-seq str)))
           (return-from brute-force str))))

;; Got lucky here that the result was found pretty fastly.
;; Another approach could be to define inverse operations and run
;; the operations backwards.
(defun sol2 ()
  (-<> (read-input "./inputs/day21")
       (brute-force "fbgdceah" <>)))
