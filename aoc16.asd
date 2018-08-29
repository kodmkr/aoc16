;;;; aoc16.asd

(asdf:defsystem #:aoc16
  :description "Advent of Code 2016"
  :depends-on ("cl-ppcre")
  :serial t
  :components ((:file "package")
               (:file "day01")
               (:file "aoc16")))
