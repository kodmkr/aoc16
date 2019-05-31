(in-package :day10)

(defvar *initial-chips-distribution*
  "value\\s+(\\d+)\\s+goes to bot\\s+(\\d+)")

(defparameter *ongoing-distribution*
  "bot\\s+(\\d+)\\s+gives low to\\s+(\\w+) (\\d+)\\s+and high to\\s+(\\w+) (\\d+)")

(defun extract-initial-distribution (str)
  (register-groups-bind ((#'parse-integer val bot-recip))
      (*initial-chips-distribution* str)
    (list val bot-recip)))

(defun extract-ongoing-distribution (str)
  (register-groups-bind ((#'parse-integer src)
                         where-lo
                         (#'parse-integer wl-num)
                         where-hi
                         (#'parse-integer wh-num))
      (*ongoing-distribution* str)
    (list src where-lo wl-num where-hi wh-num)))

(defun read-input (path-str)
  (with-open-file (s path-str)
    (loop
       for line = (read-line s nil nil)
       while line
       if (char= (aref line 0) #\v)
       collect (extract-initial-distribution line) into initials
       else
       collect (extract-ongoing-distribution line) into execs
       finally (return (list initials execs)))))

(defun lohi (x y)
  (if (<= x y)
      (list x y)
      (list y x)))

(defun put-val (val dest-key map)
  (let ((vals? (gethash dest-key map)))
    (if vals?
        (push val (gethash dest-key map))
        (setf (gethash dest-key map) (list val)))))

(defun bot-give (src-bot
                 where-lo
                 where-hi
                 where-lo-key
                 where-hi-key
                 bmap
                 omap
                 &key hook lo-ref hi-ref src-key)
  (destructuring-bind (lo hi)
      (apply #'lohi (gethash src-bot bmap))
    (when hook
      (funcall hook lo-ref hi-ref lo hi src-key))
    (put-val lo where-lo-key (if (string= where-lo "output") omap bmap))
    (put-val hi where-hi-key (if (string= where-hi "output") omap bmap))
    (setf (gethash src-bot bmap) nil)))


(defun run (inits-execs &key hook lo-ref hi-ref)
  (let ((bots (make-hash-table))
        (outputs (make-hash-table)))
    (destructuring-bind (inits execs)
        inits-execs
      (loop
         for (val . (bot-key . _)) in inits
         do (put-val val bot-key bots))
      (setf execs (sort execs (lambda (a b)
                                (>= (length (gethash (car a) bots))
                                    (length (gethash (car b) bots))))))
      (loop
         while execs
         do
           (setf execs (sort execs (lambda (a b)
                                     (>= (length (gethash (car a) bots))
                                         (length (gethash (car b) bots))))))
           (destructuring-bind (src-bot where-lo where-lo-key where-hi where-hi-key)
               (car execs)
             (bot-give src-bot where-lo where-hi where-lo-key where-hi-key bots outputs
                       :hook hook :lo-ref lo-ref :hi-ref hi-ref :src-key src-bot)
             (setf execs (cdr execs)))))
    outputs))

(defun sol1 ()
  (flet ((hook (lo-ref hi-ref lo hi key)
           (when (and (= lo-ref lo)
                      (= hi-ref hi))
             (print key))))
    (-<> (read-input "./inputs/day10")
         (run <> :hook #'hook :lo-ref 17 :hi-ref 61))))

(defun multiply-0-1-2 (hash)
  (* (car (gethash 0 hash))
     (car (gethash 1 hash))
     (car (gethash 2 hash))))

(defun sol2 ()
  (-<> (read-input "./inputs/day10")
       (run <>)
       (multiply-0-1-2 <>)))
