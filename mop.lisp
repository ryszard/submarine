;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :submarine)


;;
;; Things related to MOP
;;
;;

(defclass db-class-slot-definition ()
  ((transient  :initform nil :initarg :transient :accessor transient-p
               :documentation
              "If non-NIL, this slot should be treated as transient and
ignored in all database related operations.")
   (not-null :initform nil :initarg :not-null :accessor not-null-p
             :documentation "If non-NIL, a NON NULL database
constrained will be introduced.")
   (foreign :initform nil :initarg :foreign :accessor foreign-type-p)
   (unique :initform nil :initarg :unique :accessor unique)
   (on-delete :initform :cascade :initarg :on-delete :accessor on-delete
              :documentation "Action to be performed for this slot
when the refering row in the database ceases to exist. Possible
values: :CASCADE, :RESTRICT, :SET-NULL, :SET-DEFAULT. If this slot is
not a foreign key, it does nothing.")
   (delayed-constraint :initform nil :accessor delayed-constraint
                        :documentation "Closures adding constraints
that, for some reason, could not be executed. If there's a slot with
this attribute not-NIL in a class definition, then there's something
wrong with its SQL counterpart.")))


(defmetaclass db-class (standard-class)
  ((indices :initarg :indices :initform () :reader db-class-indices)
   (unique :initarg :unique :initform () :reader db-class-unique)
   (connection-spec :initarg :connection-spec :initform nil :reader db-class-connection-spec)
   
   (unfinished-classes :initform nil :allocation :class :accessor db-class-unfinished-classes
                       :documentation "A class allocated slot
containing classes for whom not all the constraints could be
applied.")
   (foreign-keys :initform nil :accessor db-class-foreign-keys
                 :documentation "List of foreign-key slots.")
   (unique-keys :initform nil :accessor db-class-unique-keys
                :documentation "List of slots whose value should be unique."))
  (:documentation "Metaclass for PostgreSQL aware classes. It takes
two additional arguments in DEFTABLE: :INDICES (which slots are used
as indices) and :CONNECTION-SPEC, which specifies how the class should
connect to the database (its format is the same as in
POSTMODERN:CONNECT-TOPLEVEL). If :CONNECTION-SPEC is not provided,
SUBMARINE assumes it is a class created just for the sake of
inheritance and does not create any tables for it.")
  (:validate-superclasses standard-class)
  (:slot-fixtures db-class-slot-definition))

(defmacro defdao (name supers &body body)
  "Wrapper macro for defining a class inheriting from DAO and with the
metaclass set to DB-CLASS."  
  `(defclass ,name (dao ,@supers) ,@body
    (:metaclass db-class)))

;; Why we need this method is magic for me... I don't know why the
;; values of initargs are not propagated to the effective slots by
;; default... Copied form mopintro.
(defmethod compute-effective-slot-definition :around
    ((class db-class) slot-name direct-slot-definitions)
  (let ((slotd (call-next-method)))
    (setf (transient-p slotd) (every #'transient-p direct-slot-definitions))
    (setf (foreign-type-p slotd) (foreign-type-p (car direct-slot-definitions)))
    (setf (not-null-p slotd) (not-null-p (car direct-slot-definitions)))
    (setf (unique slotd) (unique (car direct-slot-definitions)))
    (setf (slot-definition-type slotd) (slot-definition-type (car direct-slot-definitions)))
    slotd))

(defmethod shared-initialize :after ((class db-class) slots &rest rest)
  ;; create the right table, or, if a table of the right name exists,
  ;; check whether it meets the spec. Alter it if this is not the
  ;; case.
  (declare (ignore rest slots))
  (finalize-inheritance class)
  (collect-constraints class)
  (when (db-class-connection-spec class)
    (create-table class)
    (try-delayed-constraints class)))

(defmethod collect-constraints ((class db-class))
  ;the slots collecting the constraints should be set to nil before
  ;doing anything!
  (setf (db-class-foreign-keys class) nil)
  (setf (db-class-unique-keys class) nil)
  (iter (for slot in (class-slots class))
        (when (foreign-type-p slot)
          (push slot (db-class-foreign-keys class)))
        (when (unique slot)
          (push slot (db-class-unique-keys class))))
  (iter (for constraint in (db-class-unique class))
        (push (mapcar (lambda (x) (get-slot-by-name class x)) constraint) (db-class-unique-keys class))))

(defgeneric remove-finished-classes (class)
  (:documentation "Remove classes that are already finished from UNFINISHED-CLASSES of CLASS.")
  (:method ((class db-class))
    (setf (db-class-unfinished-classes class)
          (delete-if (lambda (x) (notany #'delayed-constraint (class-slots  x))) (db-class-unfinished-classes class)))))

(defgeneric try-delayed-constraints (class)
  (:documentation "Try to call all the delayed constraints present.")
  (:method ((class db-class))
    (remove-finished-classes class)
    (iter (for dirty-class in (db-class-unfinished-classes class))
          (iter (for slot in (class-slots dirty-class))
                (when (delayed-constraint slot)
                  (handler-case (progn (funcall (delayed-constraint slot))
                                       (setf (delayed-constraint slot) nil))
                    (database-error ()
                      nil)))))
    (remove-finished-classes class)))


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