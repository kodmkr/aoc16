(in-package :day14)

(defparameter +input+ "yjdafjpo")

(defparameter +test+ "abc")

(defun gen-hash (idx &key (salt +input+) &allow-other-keys)
  (bit-smasher:octets->hex
   (md5:md5sum-string
    (concatenate 'string salt (write-to-string idx)))))

(defun gen-stretched-hash (idx &key (salt +input+) cache)
  (anaphora:sif (gethash idx cache)
                (return-from gen-stretched-hash anaphora:it)
                (loop with hash = (gen-hash idx :salt salt)
                   repeat 2016 do
                     (setf hash (bit-smasher:octets->hex (md5:md5sum-string hash)))
                   finally
                     (setf (gethash idx cache) hash)
                     (return-from gen-stretched-hash hash))))

(defun has-three-consecutive-p (s)
  (cl-ppcre:register-groups-bind (((lambda (x) (char x 0)) c))
      ("(.)\\1\\1" s)
    c))

(defun has-five-consecutive-p (s c)
  (search (make-string 5 :initial-element c) s))

(defun check-next-thousand (starting-index char &key hash-gen salt cache)
  (loop for i from starting-index below (+ starting-index 1000)
     if (has-five-consecutive-p (funcall hash-gen i :salt salt :cache cache) char) do
       (return-from check-next-thousand i)))

(defun find-keys (&key hash-gen salt)
  (let ((key-vector (make-array 64 :initial-element -1 :fill-pointer t))
        (cache (make-hash-table)))
    (setf (fill-pointer key-vector) 0)
    (loop
       while (< (fill-pointer key-vector) 64)
       for i from 0
       for candidate = (funcall hash-gen i :salt salt :cache cache) do
         (alexandria:when-let*
             ((triple-char (has-three-consecutive-p candidate))
              (second-index (check-next-thousand (1+ i)
                                                 triple-char
                                                 :hash-gen hash-gen
                                                 :salt salt
                                                 :cache cache)))
           (vector-push i key-vector)))
    key-vector))

(defun sol1 ()
  (-<> (find-keys :hash-gen #'gen-hash :salt +input+)
       (aref <> 63)))

(defun sol2 ()
  (-<> (find-keys :hash-gen #'gen-stretched-hash :salt +input+)
       (aref <> 63)))
