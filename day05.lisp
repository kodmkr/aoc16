(in-package :day05)

(defparameter *puzzle-input* "reyedfim")

(defparameter *test-input* "abc")

(defun brute-force (input)
  (let ((candidates nil))
    (loop
       with i = 1
       with candidate
       for ctr from 0
       while (<= i 8)
       do
         (setf candidate (hex<- (md5sum-string (format nil "~a~d" input ctr))))
       if (string= "00000" candidate :end2 5)
       do
         (incf i)
         (push candidate candidates))
    (format t "~a~%" candidates)
    (nreverse candidates)))

(defun mk-password (candidates)
  (coerce (loop for cand in candidates collect (char cand 5)) 'string))

(defun sol1 ()
  (-<> (brute-force *puzzle-input*)
       (mk-password <>)))

(defun brute-force-2 (input)
  (let ((candidates nil)
        (password (make-array 8)))
    (loop
       with seen = 0
       with candidate
       for ctr from 0
       with pos-in-pw-c
       with pos-in-pw
       while (/= seen 255) ; => #*11111111
       do
         (setf candidate (hex<- (md5sum-string (format nil "~a~d" input ctr)))
               pos-in-pw-c (char candidate 5)
               pos-in-pw (- (char-code pos-in-pw-c) (char-code #\0)))
       if (and (string= "00000" candidate :end2 5)
               (char<= #\0 pos-in-pw-c #\7)
               (zerop (logand seen (ash 1 pos-in-pw))))
       do
         (setf seen (logior seen (ash 1 pos-in-pw)))
         (push candidate candidates)
         (setf (aref password pos-in-pw) (char candidate 6))
         ;; (format t "~a: ~a@~d|~a~%" password (char candidate 6) pos-in-pw (bits<- seen))
         )
    (values password (nreverse candidates))))

(defun sol2 ()
  (-<> (brute-force-2 *puzzle-input*)
       (coerce <> 'string)))
