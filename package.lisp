;;;; package.lisp

(defpackage #:aoc16
  (:use #:cl))

(defpackage :day01
  (:use :cl :arrows)
  (:import-from :cl-ppcre :do-register-groups))

(defpackage :day02
  (:use :cl :arrows)
  (:import-from :cl-ppcre :do-register-groups))

(defpackage :day03
  (:use :cl :arrows)
  (:import-from :cl-ppcre :do-register-groups :split))

(defpackage :day04
  (:use :cl :arrows)
  (:import-from :cl-ppcre :do-register-groups :register-groups-bind))
