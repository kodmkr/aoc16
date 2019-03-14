;;;; aoc16.asd

(asdf:defsystem #:aoc16
  :description "Advent of Code 2016"
  :depends-on ("cl-ppcre" "arrows" "md5" "bit-smasher")
  :serial t
  :components ((:file "package")
               (:file "day01")
               (:file "day02")
               (:file "day03")
               (:file "day04")
               (:file "day05")
               (:file "day06")
               (:file "day07")
               (:file "aoc16")))
