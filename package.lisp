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

(defpackage :day05
  (:use :cl :arrows :md5 :bit-smasher)
  (:import-from :cl-ppcre :do-register-groups :split))

(defpackage :day06
  (:use :cl :arrows))

(defpackage :day07
  (:use :cl :arrows)
  (:import-from :cl-ppcre :scan-to-strings :scan))

(defpackage :day08
  (:use :cl :arrows)
  (:import-from :cl-ppcre :register-groups-bind))

(defpackage :day09
  (:use :cl :arrows))

(defpackage :day10
  (:use :cl :arrows)
  (:import-from :cl-ppcre :register-groups-bind))

(defpackage :day11
  (:use :cl :arrows))

(defpackage :day12
  (:use :cl :arrows)
  (:import-from :cl-ppcre :split))

(defpackage :day13
  (:use :cl :arrows))

(defpackage :day14
  (:use :cl :arrows))

(defpackage :day15
  (:use :cl :arrows))

(defpackage :day16
  (:use :cl :arrows))

(defpackage :day18
  (:use :cl :arrows))

(defpackage :day19
  (:use :cl :arrows))

(defpackage :day20
  (:use :cl :arrows))

(defpackage :day21
  (:use :cl :arrows))
