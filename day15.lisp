(in-package :day15)


(defun extract (line)
  (cl-ppcre:register-groups-bind ((#'parse-integer num-pos init-pos))
      ("Disc .* has (\\d+) positions;.*at position (\\d+)." line)
    (list :num-pos num-pos :init-pos init-pos)))

(defun read-input (filespec)
  (with-open-file (s filespec)
    (loop for line = (read-line s nil nil)
       while line collect (extract line))))

;; from Cormen et al.
(defun eea (a b)
  "Returns values (d x y) such that d = x*a + y*b."
  (if (zerop b)
      (values a 1 0)
      (multiple-value-bind (d xx yy)
          (eea b (mod a b))
        (values d yy (- xx (* (floor a b) yy))))))

(defun modular-inverse (number modulus)
  (multiple-value-bind (d x y)
      (eea number modulus)
    (declare (ignore y d))
    (mod x modulus)))

(defun collect-congurences (input)
  (loop
     for inp in input
     for i from 1 collect
       (let ((modulus (getf inp :num-pos)))
         (list (mod (- (+ (getf inp :init-pos) i)) modulus)
               modulus))))

(defun chinese-remainder (sim-congruences)
  (let ((m (reduce #'* sim-congruences :key #'cadr)))
    (loop
       for cgr in sim-congruences
       for mi = (cadr cgr)
       for ai = (/ m mi)
       for bi = (car cgr)
       for xi = (mod (* (modular-inverse ai mi) bi) mi)
       sum (* ai xi) into sol
       finally (return (mod sol m)))))

(defun sol1 ()
  (-<> (read-input "./inputs/day15")
       (collect-congurences <>)
       (chinese-remainder <>)))

(defun sol2 ()
  (-<> (read-input "./inputs/day15")
       (append <> (list (list :num-pos 11 :init-pos 0)))
       (collect-congurences <>)
       (chinese-remainder <>)))
