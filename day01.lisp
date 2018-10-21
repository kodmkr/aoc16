(in-package :day01)


(defun read-input (path-str)
  "Read content of file specified by PATH-STR that is of
the form [RL]N+{, [RL]N+}* where N is an integer.

The output is a list of 2-tuples `(<X> <D>)`."
  (with-open-file (s path-str)
    (let ((raw (loop for line = (read-line s nil) while line nconc
                    (split ", " line)))
          res)
      (loop for dir in raw do
           (do-register-groups
               ((#'(lambda (x) (if (string= "R" x) 'R 'L)) d)
                (#'parse-integer c))
               ("\([RL]\)\(\\d+\)" dir)
             (push (list d c) res)))
      (nreverse res))))

(defun sol1 ())

(defun sol2 ())
