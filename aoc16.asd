;;;; aoc16.asd

(asdf:defsystem #:aoc16
  :description "Advent of Code 2016"
  :depends-on ("cl-ppcre" "arrows")
  :serial t
  :components ((:file "package")
               (:file "day01")
               (:file "day02")
               (:file "aoc16")))
