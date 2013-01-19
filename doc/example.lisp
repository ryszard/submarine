;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-
(in-package :asdf)

(defpackage :submarine-example
  (:use :cl :submarine))
(in-package :submarine-example)


;; DEFDAO is a wrapper macro, that creates a class inheriting from DAO
;; and with the metaclass set to db-class.
(defdao affiliation ()
  ((name        :type string :initarg :name        :accessor affiliation-name))
  (:connection-spec "submarine-test" "richard" "dupa" "localhost"))

;; Let's macroexpand last form:

;; (DEFCLASS AFFILIATION (DAO)
;;           ((NAME :TYPE STRING :INITARG :NAME :ACCESSOR AFFILIATION-NAME))
;;           (:CONNECTION-SPEC "submarine-test" "richard" "dupa" "localhost")
;;           (:METACLASS DB-CLASS))


;; Every slot that is not transient must have a TYPE---it's needed by
;; PostgreSQL. A person may not have an affiliation, but he or she
;; must have a name. (Not-null is by default set to NIL.)
(defdao person ()
  ((name        :initarg :name :accessor person-name :type string :not-null t)
   (affiliation :accessor person-affiliation :type affiliation :foreign t :initform nil
                :initarg :affiliation))
  (:connection-spec "submarine-test" "richard" "dupa" "localhost"))

(defdao club ()
  ((name :initarg :name :accessor club-name :type string))
  (:connection-spec "submarine-test" "richard" "dupa" "localhost"))

;; there's a many-to-many relationships between persons and clubs: a
;; club can have several members, one person may be the member of a
;; number of clubs.


(def-many-to-many person club :connection-spec ("submarine-test" "richard" "dupa" "localhost"))
;; This the macroexpansion of the last form. We can see it defines two
;; methods, for retrieving all objects related to its argument.
;; 
;; (progn
;;  (defclass person-club (dao)
;;            ((person :type person :accessor person)
;;             (club :type club :accessor club))
;;            (:connection-spec "submarine-test" "richard" "dupa" "localhost")
;;            (:metaclass db-class))
;;  (defmethod club ((submarine::object person))
;;             (mapcar #'club
;;                     (submarine::select-dao-fun 'person-club
;;                                                (list := 'person
;;                                                      (get-id
;;                                                       submarine::object)))))
;;  (defmethod person ((submarine::object club))
;;             (mapcar #'person
;;                     (submarine::select-dao-fun 'person-club
;;                                                (list := 'club
;;                                                      (get-id
;;                                                       submarine::object))))))

(let* ((lions (save-dao (make-instance 'club :name "Lion's")))
       (rotary (save-dao (make-instance 'club :name "Rotary")))
       (lodge (save-dao (make-instance 'club :name "The Lodge")))
       (democ (save-dao (make-instance 'affiliation :name "Democrats")))
       (repub (save-dao (make-instance 'affiliation :name "Republicans")))
       (john (make-instance 'person :name "John"))
       (roger (make-instance 'person :name "Roger" :affiliation democ))
       (henry (make-instance 'person :name "Henry" :affiliation repub)))
  ;; We had to populate our database. As save-dao returns its argument
  ;; after saving it, we can do it at the same time as initializing
  ;; the variables.
  
  ;; Let's add the last names.
  (setf (person-name henry) "Henry Johnson")
  (setf (person-name roger) "Roger Monroe")

  ;; Little Johny declares himself as a Democrat:
  (setf (person-affiliation john) democ)

  (dolist (person (list john roger henry))
    (save-dao person))

  ;; Apparently, they are all masons!
  (dolist (person (list john roger henry))
    (relate person lodge))
  
  
  (relate john lions)
  (relate roger rotary)
  (relate henry lions)
  (relate henry rotary)
  
  ;; Now, let's see what do we know about our small world
  (format t "~@{~2&All ~A: ~%~{ * ~A~&~}~}~&"
	  "persons"            (mapcar 'person-name (select-dao 'person))
	  "democrats"          (mapcar 'person-name (get-all 'person democ))
	  "masons"             (mapcar 'person-name (person lodge))
	  "the clubs of Henry" (mapcar 'club-name (club henry))))

;; The effect of executing the last form:

; All persons: 
;  * John
;  * Roger Monroe
;  * Henry Johnson

; All democrats: 
;  * John
;  * Roger Monroe

; All masons: 
;  * John
;  * Roger Monroe
;  * Henry Johnson

; All the clubs of Henry: 
;  * The Lodge
;  * Lion's
;  * Rotary

;; We can also retrieve dao's from the database if we have their ID:
(let ((john (make-instance 'person :id 1)))
  (format t "This is number 1: ~A" (person-name john)))
;; After executing the last form:

;  This is number 1: John

;; (Well, I knew it.)


