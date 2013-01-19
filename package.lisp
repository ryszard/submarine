;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-

(in-package :asdf)

(defpackage :submarine 
  (:use :cl :s-sql :cl-postgres #+sbcl :sb-mop #-sbcl :closer-mop :iterate :mop-utils)
  (:documentation "A very simple quasi-object persistency system built on top of Postmodern.")
  (:export 
   #:db-class #:foreign-type-p #:defdao
   #:dao-nonexistent-id #:dao-error #:unresolved-foreign-key
   #:class-non-transient-slots #:slot-value-or-id-if-foreign 
   #:with-connection #:with-object-connection #:with-class-connection
   #:def-many-to-many #:relate #:relatedp #:unrelate
   #:make-and-save
   #:dao #:defdao #:save-dao #:dao-exists-p #:insert-dao #:update-dao #:dao= #:make-dao
   #:select-dao #:select-dao-fun #:query-dao #:query-dao-fun #:get-dao #:delete-dao #:get-id 
   #:get-all))

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
   
