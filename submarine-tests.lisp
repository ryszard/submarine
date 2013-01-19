;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-


(in-package :submarine-tests)


(def-fixture db-fixture ()
  (progn (set-up)
         (unwind-protect
              (handler-bind ((warning #'muffle-warning))
                (&body))
             (clean))))

(defmacro db-test (name &body body)
  `(test ,(append (fiveam::ensure-list name) '(:fixture db-fixture)) ,@body))

(defun set-up ()
  (postmodern:with-connection ( "test" "zagwozdka" "dupa" "localhost")
    (postmodern:execute "CREATE DATABASE \"submarine-test\";")))

(defun clean ()
  (pomo:clear-connection-pool)
  (sleep .1)
  (pomo:with-connection ( "test" "zagwozdka" "dupa" "localhost")
    (postmodern:execute "DROP DATABASE \"submarine-test\";")))

(defmacro defdao-example (name supers &body body)
  "Wrapper macro for defining a class inheriting from DAO with the
metaclass set to HTML and the right connection-spec."
  `(defclass ,name (dao ,@supers) 
    ,@body
    (:connection-spec "submarine-test" "zagwozdka" "dupa" "localhost")
    (:metaclass db-class)))

(defmacro defmany (x y &key name)
  `(def-many-to-many ,x ,y :name ,name :connection-spec ("submarine-test" "zagwozdka" "dupa" "localhost")))


(in-suite submarine-tests)

(db-test class-connection 
  (defdao-example class-connection ()
    ())
  (finishes  (with-class-connection ((find-class 'class-connection))
               (pomo::query "SELECT 1;"))))

(db-test (dao-definition :depends-on class-connection) 
  (finishes
    (defdao-example foo ()
      ((foo :type integer)))))

(db-test (dao-redefinition :depends-on dao-definition) 
  (finishes
    (defdao-example foo ()
      ((foo :type integer)))
    (defdao-example foo ()
      ((foo :type integer)))))

(db-test (database-consistent-with-specification :depends-on dao-definition)
  (defdao-example trigram ()
    ((name :type (string 3) :accessor trigram-of :not-null t :initarg :trigram)
     ))
  (finishes (with-class-connection ((find-class 'trigram))
              (submarine::database-consistent-with-specification (find-class 'trigram)))))

(db-test (dao-definition-with-foreign-key :depends-on dao-definition)
  (defdao-example foo ()
      ((foo :type integer)))
  (finishes
    (defdao-example bar ()
      ((foo :type foo :foreign t)))))

(db-test (dao-definition-redefine-id :depends-on dao-definition)
  (finishes (defdao-example new-word ()
	      ((submarine::id :type string :accessor get-id :initarg :id :unique t)))))

(db-test (dao-definition-with-foreign-key-wrong-order :depends-on dao-definition-with-foreign-key)
  (finishes
    (defdao-example bar ()
      ((foo :type foo :foreign t))))
   (let ((class (find-class 'bar)))
     (is (member class (submarine::db-class-unfinished-classes class)))
     (defdao-example foo ()
       ((foo :type integer)))
     (is-false (member class (submarine::db-class-unfinished-classes class)))))

(db-test (unresolved-references :depends-on dao-definition-with-foreign-key)
  (defdao-example wrong-foo ()
      ((foreign-key :foreign t :accessor foreign-key :type bar :initarg :foreign :initform nil)))
  (signals unresolved-foreign-key
    (make-instance 'wrong-foo :foreign 1))
  (signals unresolved-foreign-key
    (make-and-save 'wrong-foo :foreign "foo"))
  (setf (submarine::db-class-unfinished-classes (find-class 'wrong-foo)) nil))

(db-test (get-all :depends-on dao-definition-with-foreign-key)
  (defdao-example foo ()
      ((foo :type integer :initform 0)))
  (defdao-example bar ()
    ((foo :type foo :foreign t :initarg :foo :accessor foo)))
  (let ((the-foo (make-and-save 'foo :foo 1)))
    (iter (for i from 1 to 10)
          (make-and-save 'bar :foo the-foo))
    (is (= 10 (length (get-all 'bar the-foo))))))


(db-test (many-to-many-definition :depends-on dao-definition-with-foreign-key) 
  (finishes
    (defdao-example foo ()
      ((foo :type integer)))
    (defdao-example  bar ()
      ((bar :type integer)))
    (defmany foo bar)))

(db-test (named-many-to-many-definition :depends-on many-to-many-definition) 
  (defdao-example foo ()
    ((foo :type integer)))
  (defdao-example  bar ()
    ((bar :type integer)))
  (finishes (defmany foo bar :name barfoo)))

(db-test (named-many-to-many :depends-on named-many-to-many-definition)
  (defdao-example foo ()
    ())
  (defdao-example  bar ()
    ())
  (defmany foo bar :name barfoo-1)
  (defmany foo bar :name barfoo-2)
  (let ((bar (make-and-save 'bar))
        (foo (make-and-save 'foo))
        (foo2 (make-and-save 'foo)))
    (relate bar foo :relation 'barfoo-1)
    (relate bar foo2 :relation 'barfoo-2)
    
    (is (relatedp foo bar :relation 'barfoo-1))
    (is-false (relatedp bar foo2 :relation 'barfoo-1))
    
    (is (relatedp bar foo2 :relation 'barfoo-2))
    (unrelate  bar foo2 :relation 'barfoo-2)
    (is-false (relatedp foo2 bar :relation 'barfoo-2))))
(db-test (many-to-many-redefinition :depends-on many-to-many-definition)
  (finishes
    (defdao-example foo ()
      ((foo :type integer)))
    (defdao-example bar ()
      ((bar :type integer)))
    (defmany foo bar)
    (defmany foo bar)))

(db-test (many-to-many :depends-on many-to-many-definition)
  (defdao-example uno ()
    ((foo :type integer :accessor foo :initarg :foo)))
  (defdao-example  dos ()
    ((bar :type integer :accessor bar :initarg :bar)))

  (defmany uno dos)

  (let* ((uno (save-dao (make-instance 'uno :foo 1)))
         (dos (make-and-save 'dos :bar 1))
         (otro-dos (make-and-save 'dos :bar 991)))
    
    (finishes (relate uno dos))

    (is (relatedp uno dos))
    (is-false (relatedp uno otro-dos))
    
    (is (= 1 (length (uno dos))))
    (is (= 1 (length (dos uno))))

    (is (= 1 (bar (car (dos uno)))))))


(db-test (unrelate :depends-on many-to-many)
  (defdao-example tres ()
    ((foo :type integer :accessor foo :initarg :foo)))
  (defdao-example  cuatro ()
    ((bar :type integer :accessor bar :initarg :bar)))
  
  (defmany tres cuatro)

  (let* ((tres (save-dao (make-instance 'tres :foo 1)))
         (cuatro (make-and-save 'cuatro :bar 1)))
    
    (finishes (relate tres cuatro))
    
    (is (relatedp tres cuatro))
    (unrelate tres cuatro)
                                        ;    (break "~A" (select-dao 'tres-cuatro))
    (is-false (relatedp tres cuatro)))
  )

(db-test (make-dao :depends-on dao-definition)
  (defdao-example foo ()
    ((foo :type integer :initarg :foo :accessor foo)))
  (finishes (make-instance 'foo :foo 1)))

(db-test (get-dao-from-db :depends-on make-dao)
  (defdao-example foo ()
    ((foo :type integer :initarg :foo :accessor foo)))
  (let ((instance (make-instance 'foo :foo 3)))
    (finishes (save-dao instance))
    (is (= (foo instance) (foo (make-instance 'foo :id (get-id instance)))))))

(db-test (select-dao :depends-on get-dao-from-db)
  (defdao-example foo ()
    ((foo :type integer :initarg :foo :accessor foo)))
  (iter (for i from 1 to 10)
        (make-and-save 'foo :foo 3))
  (let ((select (select-dao 'foo)))
    (is (= (length select) 10))))

(db-test (select-dao-with-test :depends-on select-dao)
  (defdao-example foo ()
    ((foo :type integer :initarg :foo :accessor foo)))
  (iter (for i from 1 to 17)
        (make-and-save 'foo :foo 17))
  (iter (for i from 1 to 13)
        (make-and-save 'foo :foo 13))
  (is (= 13 (length (select-dao 'foo (:= 13 foo)))))
  (is (= 17 (length (select-dao 'foo (:= 17 foo))))))


(db-test (save-dao :depends-on dao-definition-with-foreign-key)
  (defdao-example baz ()
    ((baz :accessor baz :initarg :baz :type integer)))

  (defdao-example quux ()
    ((baz :accessor baz :initarg :baz :type baz :foreign t)))
  
  (let*  ((baz (submarine::make-and-save 'baz :baz 1))
          (quux (make-instance 'quux :baz baz)))
    (finishes (save-dao quux))
    (is (equal 1 (baz (baz quux))))
    ))

(db-test (save-dao-recursively :depends-on save-dao)
  (defdao-example baz ()
    ((baz :accessor baz :initarg :baz :type integer)))

  (defdao-example quux ()
    ((baz :accessor baz :initarg :baz :type baz :foreign t)))
  
  (let*  ((baz (make-instance 'baz :baz 1))
          (quux (make-instance 'quux :baz baz)))
    
    (setf (baz baz) 2)
    (finishes (save-dao quux))
    (is (equal 2 (baz (baz quux))))

    (let ((new-quux (make-instance 'quux :id (get-id quux))))
      (is (equal 2 (baz (baz new-quux)))))))

(db-test (save-dao-wrong-id :depends-on save-dao)
  (defdao-example rabe ()
    ((rabe :accessor rabe :initarg :rabe :type integer)))

  (defdao-example tike ()
    ((rabe :accessor rabe :initarg :rabe :type rabe :foreign t)))
  (signals dao-nonexistent-id (make-instance 'rabe :id 666 :rabe 1))  )

(db-test (save-existing-dao :depends-on make-dao)
  (defdao-example foo ()
    ((foo :type integer :initarg :foo :accessor foo)))
  (let ((foo (make-and-save 'foo :foo 1)))
    (setf (foo foo) 777)
    (finishes (save-dao foo))
    (is (= 777 (foo (make-instance 'foo :id (get-id foo)))))))

(db-test (save-dao-wrong-id-in-reference :depends-on save-dao)
  (defdao-example rabe ()
    ((rabe :accessor rabe :initarg :rabe :type integer)))

  (defdao-example tike ()
    ((rabe :accessor rabe :initarg :rabe :type rabe :foreign t)))

  (let* ((rabe (make-instance 'rabe :rabe 1))
         (tike (make-instance 'tike :rabe rabe)))
    (setf (get-id rabe) 666) ;we set id to a random number
    (save-dao tike)
    (is (= (rabe rabe) (rabe (rabe (make-instance 'tike :id (get-id tike))))))))

(db-test (save-dao-transaction :depends-on save-dao-recursively)
  (defdao-example rabe ()
    ((rabe :accessor rabe :initarg :rabe :type integer)))

  (defdao-example tike ()
    ((rabe :accessor rabe :initarg :rabe :type rabe :foreign t)
     (tike :accessor tike :initarg :tike :type integer)))

  (let* ((rabe (make-and-save 'rabe :rabe 777))
         (tike (make-and-save 'tike :rabe rabe :tike 113)))
    (setf (rabe (rabe tike)) 666)

    (let ((method (defmethod save-dao :after ((tike tike))
                    (error "This had to happen."))))
      (signals error (save-dao tike))
      (remove-method #'save-dao method))

    (is (= 777 (rabe (rabe (make-instance 'tike :id (get-id tike))))))))

(db-test (delete-dao :depends-on dao-definition)
  (defdao-example some-long-name ()
    ((foo :initarg :foo :accessor foo :type integer)))
  (make-and-save 'some-long-name :foo 1)
  (let ((dao (car (select-dao 'some-long-name))))
    (delete-dao dao))

  (is (null (select-dao 'some-long-name))))

(db-test (unique-constraints :depends-on dao-definition)
  (finishes (defdao-example cinco ()
              ((foo :type integer :accessor foo :initarg :foo)
               (bar :type integer :accessor bar :initarg :bar ))
              (:unique (foo bar))))
  (finishes (defdao-example seis ()
	      ((foo :type integer :accessor foo :initarg :foo :unique t))))
  
  (make-and-save 'cinco :foo 1 :bar 1)
  (finishes (make-and-save 'cinco :foo 1 :bar 2))
  (finishes (make-and-save 'cinco :foo 2 :bar 1))
  (signals cl-postgres:database-error (make-and-save 'cinco :foo 1 :bar 1))

  (make-and-save 'seis :foo 1)
  (signals cl-postgres:database-error (make-and-save 'seis :foo 1)))

(db-test (dao= :depends-on dao-definition)
  (defdao-example jeden ()
    ())
  (defdao-example jeden-prim ()
    ())
  (let* ((jeden (make-and-save 'jeden))
	 (same-jeden (get-dao 'jeden (get-id jeden)))
	 (other-jeden (make-and-save 'jeden))
	 (bad-jeden (make-and-save 'jeden-prim)))
    (is (dao= jeden same-jeden))
    (is-false (dao= jeden other-jeden))
    (is-false (dao= jeden bad-jeden))))

(db-test (dao=-foreign :depends-on (and dao= dao-definition-with-foreign-key))
  (defdao-example zero ()
    ())
  (defdao-example jeden ()
    ((zero :initarg :zero :accessor zero :type zero :foreign t)))
  (defdao-example jeden-prim ()
    ((zero :initarg :zero :accessor zero :type zero :foreign t)))
  (let* ((zero (make-and-save 'zero))
	 (jeden (make-and-save 'jeden :zero zero))
	 (same-jeden (get-dao 'jeden (get-id jeden)))
	 (other-jeden (make-and-save 'jeden :zero zero))
	 (bad-jeden (make-and-save 'jeden-prim :zero zero)))
    (is (dao= jeden same-jeden))
    (is-false (dao= jeden other-jeden))
    (is-false (dao= jeden bad-jeden))))


(db-test (make-dao :depends-on (and dao= dao-definition))
  (defdao-example  ocho ()
    ((ene :initarg :ene :accessor ene-of :type integer)
     (due :initarg :due :accessor due-of :type integer)))

  (let* ((ocho (make-and-save 'ocho :ene 1 :due 2))
	 (select (with-object-connection (ocho)
		   (postmodern::query (:limit (:select '* :from 'ocho) 1) :alist)))
	 (canned-ocho (submarine::make-dao 'ocho select)))
    (is (dao= ocho canned-ocho))))

(db-test (make-dao-foreign :depends-on (and dao= dao-definition))
  (defdao-example diez ()
    ())
  (defdao-example  nueve ()
    ((diez :initarg :diez :accessor diez-of :type diez :foreign t)))


  (let* ((diez (make-and-save 'diez))
	 (nueve (make-and-save 'nueve :diez diez))
	 (select (with-object-connection (nueve)
		   (postmodern::query (:limit (:select '* :from 'nueve) 1) :alist)))
	 (canned-nueve (submarine::make-dao 'nueve select)))
    (is (dao= nueve canned-nueve))))

(db-test (query-dao :depends-on (and make-dao make-dao-foreign))
  (defdao-example once ()
    ((foo :initarg :foo :type integer)))
  (let ((once (make-and-save 'once :foo 44))
	(canned-once (car (query-dao 'once (:limit (:select * :from once) 1)))))
    (is (dao= once canned-once))))
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

