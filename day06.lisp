(in-package :day06)

(defun read-input (inpath)
  (with-open-file (s inpath)
    (loop
       for line = (read-line s nil nil)
       while line
       collect line)))

(defun fill-matrix (lines)
  "Puts the lines into a 2d array. Assumes that every
string has the same length."
  (let* ((lines-len (length lines))
         (line-len (array-dimension (car lines) 0)))
    (make-array `(,lines-len ,line-len) :initial-contents lines)))

(defun find-letters-freq (mat &key (order #'>) (start 0))
  (let ((dims (array-dimensions mat)) ; must be 2-elem list
        (cnts (make-hash-table)))
    (destructuring-bind (rows cols) dims
      (let ((word-by-freq (make-array cols)))
        (loop for j below cols do
             (loop for i below rows do
                  (multiple-value-bind (_ p?)
                      (gethash (aref mat i j) cnts)
                    (declare (ignore _))
                    (if p?
                        (incf (gethash (aref mat i j) cnts))
                        (setf (gethash (aref mat i j) cnts) 1))))
             (setf (aref word-by-freq j) (loop
                                            with max-key
                                            with curr-cnt = start
                                            for k being the hash-key of cnts
                                            using (hash-value v)
                                            if (funcall order v curr-cnt) do
                                              (setf curr-cnt v
                                                    max-key k)
                                            finally (return max-key)))
             (clrhash cnts))
        word-by-freq))))

(defun sol1 ()
  (-<> (read-input "./inputs/day06")
       (fill-matrix <>)
       (find-letters-freq <>)
       (coerce <> 'string)))

(defun sol2 ()
  (-<> (read-input "./inputs/day06")
       (fill-matrix <>)
       (find-letters-freq <> :order #'< :start most-positive-fixnum) ; somewhat of a cheat
       (coerce <> 'string)))
