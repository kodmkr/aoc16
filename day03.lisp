(in-package :day03)

(defun read-input (inpath)
  (with-open-file (s inpath)
    (loop
       for line = (read-line s nil nil)
       while line
       collect (loop
                  for n in (split "\\s+" (string-left-trim '(#\Space) line))
                  collect (parse-integer n)))))

(defun triangle-valid-p (a b c)
  (destructuring-bind (x y z)
      (sort (list a b c) #'<)
    (> (+ x y) z)))

(defun count-possible-triangles (trips)
  (loop
     for (a b c) in trips
     if (triangle-valid-p a b c)
     count 1))

(defun sol1 ()
  (-<> (read-input "./inputs/day03")
       (count-possible-triangles <>)))

(defun count-possible-triangles-cols (trips &optional (cnt 0))
  (cond ((null trips) cnt)
        (t (let ((fst (car trips))
                 (snd (cadr trips))
                 (trd (caddr trips)))
             (count-possible-triangles-cols
              (cdddr trips)
              (+ cnt
                 (count-if-not #'null
                               (mapcar #'triangle-valid-p
                                       fst snd trd))))))))

(defun sol2 ()
  (-<> (read-input "./inputs/day03")
       (count-possible-triangles-cols <> 0)))
