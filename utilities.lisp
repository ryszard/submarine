;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :submarine)

;; 
;; Utilities
;; 
;; 

(defun non-db-slot (slot)
  "Slot is transient or its allocation is :CLASS."
  (or (transient-p slot) (eql (slot-definition-allocation slot) :class)))

(defun class-db-slots (class)
  "Return the list of non-transient, instance allocated slots of a class."
  (remove-if #'non-db-slot (class-slots class)))

(defun db-slots-of (object)
  "List of non-transient, instance allocated slots of OBJECT"
  (remove-if #'non-db-slot (slots-of object)))


(defun with-connection-fun (connection-spec what-to-do)
  "Functional interface to the macro WITH-CONNECTION."
  (let ((postmodern:*database* (apply #'postmodern:connect connection-spec)))
    (unwind-protect (progn (funcall what-to-do))
      (postmodern:disconnect postmodern:*database*))))

;; This is postmodern's WITH-CONNECTION modified to use the functional
;; interface. Maybe it should be added to postmodern.
(defmacro with-connection ((database user password host &key (port 5432) pooled-p) &body body)
  "Binds *database* to a new connection and runs body in that scope."
  `(with-connection-fun '(,database ,user ,password ,host :port ,port :pooled-p ,pooled-p)
    (lambda () (progn ,@body))))

(defun with-object-connection-fun (object what-to-do)
  "Functional interface to the macro WITH-OBJECT-CONNECTION."
  (with-class-connection-fun (class-of object)
    what-to-do))

(defmacro with-object-connection ((object) &body body)
  "Run BODY in an environment with a connection to the database
specified by the DB-CLASS-CONNECTION-SPEC of the class of OBJECT."

  `(with-object-connection-fun ,object
    (lambda () (progn ,@body))))

(defun with-class-connection-fun (class what-to-do)
  "Functional interface to the macro WITH-CLASS-CONNECTION. the
connection will be pooled."
  (with-connection-fun (append (db-class-connection-spec class) '(:pooled-p t))
    (lambda ()
      (postmodern:with-transaction ()
        (funcall what-to-do)))))

(defmacro with-class-connection ((class) &body body)
  "Run BODY in an environment with an always pooled connection to the database
specified by the DB-CLASS-CONNECTION-SPEC of CLASS."

  `(with-class-connection-fun ,class
    (lambda () (progn ,@body))))

;;; Copyright (C) 2007
;;; Ryszard Szopa <ryszard.szopa@gmail.com> & 
;;; Streamtech http://streamtech.nl/
;;;
;;; This software is provided 'as-is', without any express or implied
;;; warranty. In no event will the authors be held liable for any
;;; damages arising from the use of this software.
;;;
;;; Permission is granted to anyone to use this software for any
;;; purpose, including commercial applications, and to alter it and
;;; redistribute it freely, subject to the following restrictions:
;;;
;;; 1. The origin of this software must not be misrepresented; you must
;;;    not claim that you wrote the original software. If you use this
;;;    software in a product, an acknowledgment in the product
;;;    documentation would be appreciated but is not required.
;;;
;;; 2. Altered source versions must be plainly marked as such, and must
;;;    not be misrepresented as being the original software.
;;;
;;; 3. This notice may not be removed or altered from any source
;;;    distribution.