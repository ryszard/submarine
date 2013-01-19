;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :asdf)
(defpackage :submarine-tests
  (:use :cl :submarine :fiveam :iterate)
  (:export #:run!))
(in-package :submarine-tests)
(def-suite submarine-tests)
