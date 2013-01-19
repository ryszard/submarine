;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :submarine)

;; DAO conditions

(define-condition dao-error (simple-error)
  ((type :initarg :type :reader table-name))
  (:documentation "Base class for DAO related errors."))

(define-condition dao-nonexistent-id (dao-error)
  ((id :initarg :id :reader id))
  (:documentation "Condition signaling the attempt to initialize a dao
with a non-existant ID."))

(define-condition unresolved-foreign-key (dao-error)
  ()
  (:documentation "Condition signaling that a class with a foreign-key
reference to a non-defined class has been tried to be used."))
