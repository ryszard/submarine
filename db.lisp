;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :submarine)


;;
;; Database stuff
;;
;;


(defgeneric create-table (class)
  (:documentation "Create a table whose columns match the fields of
CLASS in the connected database. If CLASS is a symbol, the class it
names is used."))

;; FIXME: CREATE-TABLE tests for the right columns and column-types,
;; but doesn't test if the constraints of the table are right. So, if
;; we change a foreign key in the class, PostgreSQL doesn't notice it.
(defmethod create-table ((class db-class))
  (unless (class-finalized-p class)
    (error "The class is not finalized."))
  (with-class-connection (class)
    (handler-case 
        (let ((class-name (class-name class)))
          (labels ((index-name (fields)
                     (format nil "~A~{_~A~}_index" (to-sql-name class-name) (mapcar #'to-sql-name fields)))
                   (type-and-reference (slot)
                     (if (foreign-type-p slot)
                         (list :type (if (not-null-p slot) 'integer '(or db-null integer)))
                         (list :type (if (not-null-p slot)
                                         (slot-definition-type  slot)
                                         `(or db-null ,(slot-definition-type slot)))))))
            (postmodern:with-transaction ()
              (postmodern:execute
               (sql-compile
                `(:create-table ,class-name
                  ,(iter (for slot in (class-db-slots  class))
                         (collect
                             (cons (slot-definition-name slot) (type-and-reference slot))))
                  (:primary-key id))))      ;id is hardcoded as a primary key
              
              (dolist (index (mapcar (lambda (x) (if (consp x) x (list x)))(db-class-indices class)))
                (postmodern:execute (sql-compile  `(:create-index (:raw ,(index-name index))
                                                    :on ,class-name
                                                    :fields ,@index))))
              (postmodern:execute (:create-sequence (id-seq-name class))))))
      (database-error (err)
        (if (string= (database-error-code err) "42P07") ;table already exists
            (progn
              (postmodern:with-transaction ()
                ;; FIXME: This may be considered as a quick
                ;; hack. S-SQL should be changed to support ALTER
                ;; commands.
                (handler-case
                    (database-consistent-with-specification class)
                  (sql-column-does-not-exist (err)
                    (restart-case (error err)
                      
                      (fix-it () :report "Add the column to the
table. (Note that allocate-instance the changes for one table are done
in a transaction, so if at any time you choose abort the changes will
be reverted.)"
                              (postmodern:execute (format nil "ALTER TABLE ~A ADD COLUMN ~A ~A;"
                                                          (sql-ize (table-name err)) (column-name err) (column-type err))))
                      (ignore () ())))
                  (sql-column-error (err)
                    (restart-case (error err)
                      (fix-it () :report "Alter the type of the
   column. (Note that all the changes for one table are done in a
   transaction, so if at any time you choose abort the changes will be
   reverted."
                              (postmodern:execute (format nil "ALTER TABLE ~A ALTER COLUMN ~A TYPE ~A"
                                                          (sql-ize (table-name err)) (column-name err) (needed-type err))))
                      (ignore () ())))))
              
              (warn "Table ~A already exists." (class-name class)))
            (error err))))
    ; here should be some code dropping constraints that are not necessary
    (apply-foreign-key-constraints class)
    (apply-unique-constraints class)))

(defmethod create-table ((name symbol))
  (let ((class (find-class name)))
    (create-table class)))

(defgeneric add-constraint (slot constraint)
  (:documentation "Add a constraint to this slot's class' table."))

(defmethod add-constraint :around ((slot db-class-slot-definition) constraint)
         (let ((class (slot-value slot 'sb-pcl::allocation-class)))
           (with-class-connection (class)
             (postmodern:execute (format nil "ALTER TABLE ~A ADD ~A;"
                                         (sql-ize  (class-name class))
                                         (call-next-method))))))

(defmethod add-constraint ((slot db-class-slot-definition) (constraint (eql :foreign-key)))
  (format nil "FOREIGN KEY (~A) REFERENCES ~A ON DELETE ~A"
          (sql-ize (slot-definition-name slot))
          (sql-ize (slot-definition-type slot))
          (case (on-delete slot)
            (:set-null "SET NULL")
            (:set-default "SET DEFAULT")
            (otherwise (on-delete slot)))))

(defmethod add-constraint ((slot db-class-slot-definition) (constraint (eql :unique)))
  (format nil "UNIQUE \(~A\)"
          (sql-ize (slot-definition-name slot))))

(defmethod add-constraint ((list list) (constraint (eql :unique)))
  (assert (every (lambda (x)
                   (typep x 'db-class-slot-definition)) list))
  (let ((class (slot-value (car list) 'sb-pcl::allocation-class)))
    (with-class-connection (class)
      (postmodern:execute (format nil "ALTER TABLE ~A ADD UNIQUE (~A~{, ~A~});"
                                  (sql-ize  (class-name class))
                                  (sql-ize (slot-definition-name  (car list)))
                                  (mapcar #'(lambda (x)
                                              (sql-ize (slot-definition-name x))) (cdr list)))))))


(defmethod apply-foreign-key-constraints ((class db-class))
  (iter (for slot in (db-class-foreign-keys class))
        (let ((closure (lambda ()
                         (add-constraint slot :foreign-key))))
          (handler-case 
              (funcall closure)
            (database-error ()
              (setf (delayed-constraint slot) closure)
              (unless (member class (db-class-unfinished-classes class)
                              :test (lambda (x y)
                                      (eql (class-name x) (class-name y))))
                (push class (db-class-unfinished-classes class))))))))

(defmethod apply-unique-constraints ((class db-class))
  (iter (for slot in (db-class-unique-keys class))
        (add-constraint slot :unique)))

(define-condition sql-error (simple-error)
  ((table :initarg :table-name :reader table-name))
  (:documentation "Base class for SQL related errors."))

(define-condition sql-column-error (sql-error)
  ((name :initarg :name :reader column-name)
   (needed-type :initarg :needed :reader needed-type)
   (actual-type :initarg :actual :reader actual-type))
  (:documentation "An error symbolizing that there is a column of the
right name in the table, but its type is wrong"))

(define-condition sql-column-does-not-exist (sql-error)
  ((name :initarg :name :reader column-name)
   (type :initarg :type :reader column-type))
  (:documentation "Condition signaling a missing column in an existing table."))

(defgeneric database-consistent-with-specification (class)
  (:documentation "Return nothing if TABLE in the connected-p database
meets its specification. Otherwise, throw an appropriate error.  There
must exist a connection to the DATABASE in order to run this
function.")
  (:method ((class db-class))
    (let ((class-name (class-name class)))
      (labels
          ((ensure-list (x)
             (if (listp x)
                 x
                 (list x)))
           (prepare-field (field)
             (list (sql-ize (slot-definition-name field))
                   (string-downcase (apply #'sql-type-name (ensure-list (or (and (foreign-type-p field) 'integer)
                                                                            (slot-definition-type field)))))))
           (extract-type (field)
             (read-from-string (second field)))
           (field= (field1 field2)
             (and
              (equal (car field1) (car field2))
              (string= (extract-type field1) (extract-type field2)))))
        (let ((fields 
               (mapcar #'prepare-field (class-db-slots class)))
              (columns (postmodern::query (sql (:select 'column-name 'data-type
                                            :from 'information_schema.columns
                                            :where (:= 'table-name (sql-ize class-name)))))))
          ;; we test whether there are any additional columns in the table
          ;; in the database
          (iter (for column in columns)
                (unless (member column fields :test #'equal)
                  (warn "There's an additional column \"~A\" of type ~A in
table ~A. This may indicate that something is wrong." (first column) (second column) class-name)))

          ;; we test if all the specified fields have an appropriate column
          ;; counterpart in the db
          (iter (for field in fields)
                (unless (and
                         (member field columns :test #'field=))
                  (if (member (car field) (mapcar #'car columns) :test #'string=)
                      ;; column exists, but type is wrong
                      (error 'sql-column-error
                             :table-name class-name
                             :name (first field)
                             :needed (second field)
                             :actual (assoc (car field) columns :test #'string=)
                             :format-control "Column ~A exists in table ~A, but its type is wrong."
                             :format-arguments (list field class-name))
                      ;; the column does not exist
                      (error 'sql-column-does-not-exist
                             :table-name class-name
                             :name (first field)
                             :type (second field)
                             :format-control "Column ~A in table ~A does not exist."
                             :format-arguments (list (car field) class-name)))))))
      ; do here something with constraints involving the table
      (values))))

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
