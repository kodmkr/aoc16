(in-package :day08)

(defun read-rect (line)
  (register-groups-bind ((#'parse-integer width height))
      ("^rect\\s+(\\d+)x(\\d+)$" line)
    (list 'mkr width height)))

(defun read-col-rot (line)
  (register-groups-bind ((#'parse-integer col amount))
      ("^rotate\\s+column\\s+x=(\\d+)\\s+by\\s+(\\d+)$" line)
    (list 'rotcol col amount)))

(defun read-row-rot (line)
  (register-groups-bind ((#'parse-integer row amount))
      ("^rotate\\s+row\\s+y=(\\d+)\\s+by\\s+(\\d+)$" line)
    (list 'rotrow row amount)))

(defun read-input (inpath)
  (with-open-file (s inpath)
    (loop
       for line = (read-line s nil nil)
       while line
       collect (or (read-rect line)
                   (read-col-rot line)
                   (read-row-rot line)))))
(defun mk-screen (w h)
  (make-array (list h w) :initial-element #\.))

(defun render-screen (screen)
  (destructuring-bind (h w)
      (array-dimensions screen)
    (loop for i below h do
         (loop for j below w do
              (format t "~a " (aref screen i j)))
         (format t "~&"))))

(defun rect (screen w h)
  "Turns on a rectangle of width W and height H
starting at top left corner"
  (loop
     for i below h do
       (loop
          for j below w do (setf (aref screen i j) #\#)))
  screen)

;; this was complicated to get but it is still not good,
;; see the definitions of NSHIFT-* below: the dimension
;; is calculated thrice but only needed once.
;; It would be better if one could also vary the macro
;; body to put the three rotations there...
(defmacro nrev (mat rc rc-idx &key (start 0) end)
  (let ((m (gensym))
        (fst-idx (ecase rc
                   (row `(,rc-idx (+ ,start i)))
                   (col `((+ ,start i) ,rc-idx))))
        (snd-idx (ecase rc
                   (row `(,rc-idx (- last-idx i)))
                   (col `((- last-idx i) ,rc-idx))))
        (dim (ecase rc
               (row 1)
               (col 0))))
    `(let* ((,m ,mat)
            (last-idx (if ,end ,end (1- (array-dimension ,m ,dim))))
            (half (floor (1+ (- last-idx ,start)) 2)))
       (loop
          for i below half do
            (rotatef (aref ,m ,@fst-idx)
                     (aref ,m ,@snd-idx)))
       ,m)))

(defun nshift-row (mat row-idx amount)
  (nrev mat row row-idx)
  (nrev mat row row-idx :end (1- amount))
  (nrev mat row row-idx :start amount))

(defun nshift-col (mat col-idx amount)
  (nrev mat col col-idx)
  (nrev mat col col-idx :end (1- amount))
  (nrev mat col col-idx :start amount))

(defun shuffle-rect (instrs &key (w 50) (h 6))
  (let ((screen (mk-screen w h)))
    (loop
       for instr in instrs do
         (destructuring-bind (typ a b) instr
           (case typ
             (mkr (rect screen a b))
             (rotcol (nshift-col screen a b))
             (rotrow (nshift-row screen a b)))))
    screen))

(defun count-lit (screen)
  (let ((cnt 0))
    (destructuring-bind (h w)
        (array-dimensions screen)
      (loop for i below h do
           (loop for j below w
              if (char= (aref screen i j) #\#) do
                (incf cnt))))
    cnt))

(defun sol1 ()
  (-<> (read-input "./inputs/day08")
       (shuffle-rect <>)
       (count-lit <>)))
                
(defun sol2 ()
  (-<> (read-input "./inputs/day08")
       (shuffle-rect <>)
       (render-screen <>)))
