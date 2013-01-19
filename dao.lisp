;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :submarine)

;;
;; DAO
;;
;;

(defclass dao ()
  ((id :type integer :accessor get-id :initarg :id))
  (:indices)
  (:metaclass db-class)
  (:documentation "Base class for any PostgreSQL aware classes."))


(defgeneric save-dao (object)
  (:documentation "Save a dao: update it when it already exists, insert it otherwise.")
  (:method ((dao  dao))
    (if (dao-exists-p dao)
        (update-dao dao)
        (insert-dao dao))))

(defgeneric dao-exists-p (dao)
  (:documentation "Return a boolean indicating whether the given DAO
exists in the database.")
  (:method ((dao dao))
    (with-object-connection (dao)
        (and (slot-boundp dao 'id)
             (postmodern::query (:select (:exists (:select 1 :from (class-name-of dao) :where (:= 'id (get-id dao)))))
                                :single)))))

(defun slot-value-or-id-if-foreign (object slot)
  "SLOT-VALUE if SLOT in OBJECT is not foreign, the ID of the object
in the slot otherwise. If the object is not saved, save it."
  (let ((value (slot-value object slot)))
    (cond ((and (typep value 'dao) (dao-exists-p value))
           (get-id value))
          ((typep value 'dao) (get-id (save-dao value)))
          (t value))))

(defun set-fields (object)
  (iter (for field in (mapcar #'slot-definition-name (db-slots-of object)))
        (appending `(,field ,(slot-value-or-id-if-foreign object field)))))

(defun id-seq-name (class)
  (intern (format nil "~A-ID-SEQ" (class-name class)) :keyword))

(defgeneric insert-dao (dao)
  (:documentation "Insert the given DAO into the database.")
  (:method ((dao dao))
    (with-object-connection (dao)
      (unless (slot-boundp dao 'id)
        (setf  (slot-value dao 'id) (postmodern::sequence-next (id-seq-name (class-of dao)))))
      (postmodern::execute (sql-compile `(:insert-into ,(class-name-of dao) :set ,@(set-fields dao)))))
    dao))

(defgeneric update-dao (dao)
  (:documentation "Update the DAO's representation in the database
with the values in the given object and return it."))

(defmethod update-dao ((dao dao))
  (with-object-connection (dao)
    (postmodern::execute (sql-compile 
                          `(:update ,(class-name-of dao) :set ,@(set-fields dao)
                            :where (:= id ,(get-id dao))))))
  dao)

(defmethod shared-initialize :before ((dao dao) slot-names &rest initargs &key &allow-other-keys)
  (declare (ignore slot-names initargs))
  (let ((class (class-of  dao)))
    (when (member class (db-class-unfinished-classes class))
      (error 'unresolved-foreign-key
	     :format-control "You are trying to create an
	     instance of a class with a foreign, non-existing
	     reference. Create the class you are refering
	     to." :type (class-name class)))))

(defmethod shared-initialize :after ((dao dao) slot-names &rest initargs &key  &allow-other-keys)
  "If :ID is provided, set the values of the slots according to the
database. If other initargs where provided, assert they are consistent
with the database. A NIL value in a slot is considered consistent with
everything and is overwritten with the value from the database (if
any)."
  (declare (ignore initargs))
  (labels ((ensure-not-db-null (x)
             (unless (equal x :NULL)
               x)))
    (when (slot-boundp dao 'id)
      (with-object-connection (dao)
        (let ((query-result (postmodern::query 
                             (:limit (:select '*
					      :from (class-name-of  dao)
					      :where (:= 'id (get-id dao))) 1) :alist)))
          (unless query-result
            (error (make-condition 
                    'dao-nonexistent-id
                    :format-control "You tried to create a DAO with a non-existant ID."
                    :id (get-id dao) :type (class-name-of  dao))))
          (iter
            (for slot in  (db-slots-of  dao))
        
            (let ((slot-name (slot-definition-name slot)))
              (unless (equal 'id slot-name)
                (let ((value-from-db (ensure-not-db-null
                                      (cdr (assoc (intern (symbol-name slot-name) 'keyword) query-result)))))
                  (when (slot-boundp dao slot-name)
                    (let* ((slot-value (slot-value dao slot-name))
                           (value-from-init (if (and slot-value (foreign-type-p slot))
                                                (get-id slot-value)
                                                slot-value)))
                      (assert 
                       (or (null value-from-init) (equal value-from-init value-from-db))
                       (value-from-init value-from-db)
                       "Values inconsistent with the database for
                       slot ~A. ~%Value from init: ~A, value from
                       database: ~A~%" slot-name value-from-init
                       value-from-db)))
                  (setf (slot-value dao slot-name) 
                        (if (foreign-type-p slot)
                            (make-instance (slot-definition-type slot) :id value-from-db) 
                            value-from-db)))))))))))

(defun make-dao (type initargs)
  "Create a DAO of the given `TYPE' and initialize it according
  to the values of the alist `INITARGS'. `Initargs' may contain
  additional values, not used in the initialization proccess."
  (let ((instance (make-instance type)))
    (iter (for slot in (slots-of instance))
	  (setf (slot-value instance (slot-definition-name slot))
		(let ((the-value (cdr (assoc (intern (symbol-name (slot-definition-name slot)) 'keyword) initargs))))
		  (if (foreign-type-p slot)
		      (make-instance (sb-pcl:slot-definition-type slot) :id the-value)
		      the-value))))
    instance))

(defun query-dao-fun (type query)
  "Execute the given `QUERY' (which can be either a string or a
S-SQL expression, which will be evaluated) and return the result
as daos of the given `TYPE'. The names of the fields returned by
the query must match the slots of the dao. This is a functional
interface for `QUERY-DAO'."
  (mapcar (lambda (x)
	    (make-dao type x))
	  (with-class-connection ((find-class type))
	    (postmodern:query (postmodern:sql-compile query) :alists))))

(defmacro query-dao (type query)
  "Execute the given `QUERY' (which can be either a string or a
S-SQL expression, which won't be evaluated) and return the result
as daos of the given `TYPE'. The names of the fields returned by
the query must match the slots of the dao."
  `(query-dao-fun ,type ',query))

(defun select-dao-fun (type test)
  "Functional interface for SELECT-DAO."
  (query-dao-fun type `(:select * :from ,type
			       :where (:raw ,(if (stringp test)
						test
						(sql-compile test))))))

(defmacro select-dao (type &optional (test t))
  "Select daos of TYPE for the rows in its table for which the given TEST
holds."
  `(select-dao-fun ,type ',test))

(defgeneric dao= (left right &key test)
  (:documentation "T if all the slot-values of `LEFT' and `RIGHT'
  are the same. If the compared values are not DAO's, they are
  compared using `TEST' \(which defaults to `EQL'\). Otherwise,
  they are `DAO='ed, with the same `TEST' argument as specified
  in the topmost call of `DAO='. If neither `LEFT' nor `RIGHT'
  are DAOs, `TEST' is called."))

(defmethod dao= (left right &key (test 'eql))
  (funcall test left right))

(defmethod dao= ((left dao) (right dao) &key (test 'eql))
  (and (eql (class-of left) (class-of right))
       (iter (for slot-name in (slot-names-of left))
	     (always (dao= (slot-value left slot-name)
			   (slot-value right slot-name) :test test)))))

(defgeneric delete-dao (dao)
  (:documentation "Delete the given dao from the database.")
  (:method ((dao dao))
    (with-object-connection (dao)
      (postmodern:execute (:delete-from (class-name-of dao) :where (:= 'id (get-id dao)))))))

(defun get-dao (type id)
  "Get the dao corresponding to the given primary key,
or return nil if it does not exist."
 (let ((dao (make-instance type :id id)))
   (when (dao-exists-p dao)
     dao)))



(defgeneric get-all (type object)
  (:documentation "Get all objects of TYPE that stand in a many-to-one
relation with OBJECT.")
  (:method (type (object t))
    (let ((name-of-column
           (iter (for slot in (class-slots (find-class type)))
                 (finding (slot-definition-name slot) 
                          such-that (equal (slot-definition-type slot) (type-of object))))))                    
    (select-dao-fun type `(:= ,name-of-column ,(get-id object))))))

;; acache documentation for def-many-to-many:
;;   "Defines many-to-many mapping between two persistent classes.

;; CLASS*-NAME can be either (unquoted) symbol with class name, or
;; list (CLASS*-METHOD CLASS*-NAME).  When method name is not given,
;; other class' name is taken for method.

;; Defines additional mapping persistent class `CLASS1-NAME-CLASS2-NAME'
;; and for each of classes, a method that retrieves list of instances of
;; second class, and SETF method of the same name that updates the
;; mapping.  Mapping class has two slots, named `CLASS1-NAME' and
;; `CLASS2-NAME' that hold instances of first and second class.

;; User is responsible for SETFing relation pseudo-accessor to NIL or
;; periodically cleaning up relation class with slots of value NIL."

(defmacro def-many-to-many (class1-name class2-name &key connection-spec name)
  "Defines a possibly named many-to-many mapping between two
persistent classes. There can be only one unnamed mapping between any
two classes, and any number of named mappings.  CLASS*-NAME should be
an \(unquoted\) symbol with class name.

Defines additional mapping persistent class `NAME' \(if `NAME' is
not provided, `CLASS1-NAME-CLASS2-NAME' is used\), and for each of
classes, a method that retrieves list of instances of second class
\(named like that class or, if the relation is named,
CLASS-NAME-IN-NAME. Mapping class has three slots, `CLASS1-NAME' and
`CLASS2-NAME' that hold instances of first and second class, and
`WEIGHT' that holds an integer representing the strength of the
relationship. The following functions are available:

 * A predicate `RELATEDP', taking a keyword argument `RELATION', which
tells whether two objects are in a relation \(if `RELATION' is not
provided, the unnamed relation is assumed\). It returns a `NAME' or
`CLASS1-NAME-CLASS2-NAME' object. It has an accessor `WEIGHT'.

 * A function `RELATE' \(`RELATE-IN-NAME'\) that establishes a
relationship between its two arguments. It takes a key-argument,
`WEIGHT', by default 1. The order in which the object arguments are
provided is irrelevant.  `CONNECTION-SPEC' specifies the connection to
be used to create the link table."
  (let ((table-name (or name (intern (format nil "~A-~A" class1-name class2-name))))
        (class1-method-name class2-name)
        (class2-method-name class1-name))
    `(progn 
      (defclass ,table-name (dao)
        ((,class1-name :type ,class1-name :accessor ,class1-name :foreign t :on-delete :cascade)
         (,class2-name :type ,class2-name :accessor ,class2-name :foreign t :on-delete :cascade)
         (weight :type integer :accessor weight-of :initform 1 :initarg :weight))
        ;; A unique constraint here
        ;; or better no: thing of tags.
        (:unique (,class1-name ,class2-name))
        (:connection-spec ,@connection-spec)
        (:metaclass db-class))
      (defmethod ,class1-method-name ((object ,class1-name))
        (mapcar #',class2-name (select-dao-fun ',table-name (list := ',class1-name (get-id object)))))
      (defmethod ,class2-method-name ((object ,class2-name))
        (mapcar #',class1-name (select-dao-fun ',table-name (list := ',class2-name (get-id object)))))
      (defmethod relatedp-method ((object1 ,class1-name) (object2 ,class2-name) (relation ,(if name `(eql ',name) 'null)))
        (select-dao-fun ',table-name `(and
                                       (:= ,',class1-name ,(get-id object1))
                                       (:= ,',class2-name ,(get-id object2)))))
      (defmethod relatedp-method ((object2 ,class2-name) (object1 ,class1-name) relation)
        (relatedp-method object1 object2 relation))
      (defmethod relate-method
          ((object1 ,class1-name) (object2 ,class2-name) (relation ,(if name `(eql ',name) 'null)) weight)
        (let ((instance (make-instance ',table-name :weight weight)))
          (setf (slot-value instance ',class1-name) object1)
          (setf (slot-value instance ',class2-name) object2)
          (save-dao instance)))

      (defmethod relate-method ((object1 ,class2-name) (object2 ,class1-name) relation weight)
        (relate-method object2 object1 relation weight))
      
      (defmethod unrelate-method ((object1 ,class1-name) (object2 ,class2-name) (relation ,(if name `(eql ',name) 'null)))
        (delete-dao (car (relatedp object1 object2 :relation relation))))
      (defmethod unrelate-method ((object2 ,class2-name) (object1 ,class1-name) relation)
        (unrelate-method object1 object2 relation)))))

(defgeneric relate-method (object1 object2 relation weight)
  (:documentation "Relate `OBJECT1' with `OBJECT2' in `RELATION'."))
(defgeneric relatedp-method (object1 object2 relation)
  (:documentation "Tell if `OBJECT1' is related to `OBJECT2' in `RELATION'."))
(defgeneric unrelate-method (object1 object2 relation)
  (:documentation "`OBJECT1' stops being with `OBJECT2' in `RELATION'."))

(defun relate (object1 object2 &key relation (weight 1))
  "Make OBJECT1 and OBJECT2 enter into the many-to-many relation
`RELATION'. If `RELATION' is not provided, the unnamed relation is
assumed."
  (relate-method object1 object2 relation weight))

(defun relatedp (object1 object2 &key relation)
  "Tell whether `OBJECT1' and `OBJECT2' are in the many-to-many relation
`RELATION' \(or the unnamed relation if `RELATION' is not provided\)."
  (relatedp-method object1 object2 relation))

(defun unrelate (object1 object2 &key relation)
  "`OBJECT1' stops being with `OBJECT2' in `RELATION'."
  (unrelate-method object1 object2 relation))

(defmacro make-and-save (type &rest rest)
  "Make a DAO and immediately save it. Takes the same arguments as
\(make-instance \(\(dao dao\)\)\)."
  `(save-dao (make-instance ,type ,@rest)))

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