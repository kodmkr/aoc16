(in-package :day04)

(defstruct secroom
  name
  sector-id
  checksum)

(defun read-input (inpath)
  (with-open-file (s inpath)
    (loop
       for line = (read-line s nil nil) while line
       collect (register-groups-bind (enc-name (#'parse-integer sec-id) cs)
                   ("^(.*?)-(\\d+)\\[(.*)\\]$" line)
                 (make-secroom :name enc-name :sector-id sec-id :checksum cs)))))

(defun rle (str)
  "Calculates the run-length encoding of STR.
NOTE: the string STR *must not* be empty."
  (let ((len (length str))
        (res nil))
    (loop
       for i = 0 then j
       for j = (position-if (lambda (c) (char/= c (char str i))) str :start i)
       while j
       do
         (push (cons (char str i) (- j i)) res)
       finally
         (push (cons (char str i) (- len i)) res))
    (nreverse res)))

(defun most-common-p (name)
  (let* ((sans-dash (remove #\- name))
         (srtd-sd (sort sans-dash #'char<))
         (rle (rle srtd-sd))
         (srtd-rle (sort rle (lambda (x y)
                               (or (> (cdr x) (cdr y))
                                   (char< (car x) (car y))))))
         (five-chars (loop for i below 5
                        for (c . num) in srtd-rle collect c)))
    (coerce  five-chars 'string)))

(defun room-legit-p (room)
  (string= (secroom-checksum room)
           (most-common-p (secroom-name room))))

(defun sec-id-sum (rooms)
  (reduce #'+ rooms :key #'secroom-sector-id))

(defun sol1 ()
  (-<> (read-input "./inputs/day04")
       (remove-if-not #'room-legit-p <>)
       (sec-id-sum <>)))

(defun shift (chr num)
  (let ((a-code (char-code #\a)))
    (-<> (char-code chr)
         (- <> a-code)
         (+ <> num)
         (mod <> 26)
         (+ <> a-code)
         (code-char <>))))

(defun decrypt (str num)
  (let ((w-spcs (substitute #\Space #\- str)))
    (map 'string
         (lambda (c)
           (if (char= c #\Space)
               c
               (shift c num)))
         w-spcs)))

(defun decrypt-rooms (lrooms)
  (loop
     for lroom in lrooms collect
       (cons (decrypt (secroom-name lroom) (secroom-sector-id lroom))
             (secroom-sector-id lroom))))

(defun find-np-obj-store (decrooms)
  (loop
     for (name . id) in decrooms
     if (search "north" name)
     collect (cons name id)))

(defun sol2 ()
  (-<> (read-input "./inputs/day04")
       (remove-if-not #'room-legit-p <>)
       (decrypt-rooms <>)
       (find-np-obj-store <>)
       (cdar <>)))
