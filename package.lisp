;;;; package.lisp

(defpackage #:aoc16
  (:use #:cl))

(defpackage :day01
  (:use :cl :arrows)
  (:import-from :cl-ppcre :split :scan-to-strings :do-register-groups))
