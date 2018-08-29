;;;; aoc16.lisp

(in-package #:aoc16)

(defun run (&key day sol)
  "Runs the solution SOL{sol} in package DAY{day},
where `day` is a two digit number from 1 to 25 and
`sol` is either the number 1 or the number 2.

EXAMPLE.
        (run 1 1) ; => causes DAY01:SOL1 to be run"

  (funcall (find-symbol (format nil "SOL~d" sol) (format nil "DAY~2,'0,d" day))))
