;; -*- Mode: Lisp; -*-

(in-package :cl-user)

(load-plob)
(open-my-session)


;; all symbols (only for testing)
(unless (p-boundp '*root*)
  (setf #!*root* nil))

(unless (p-boundp '*keyword-index*)
  (setf #!*keyword-index* (make-btree :test 'equal)))

(defclass preference ()
  ((keyword :initarg :keyword :reader preference-keyword)
   (weight :initarg :weight :accessor preference-weight :initform 1.0))
  (:metaclass persistent-metaclass))

(defun make-preference (keyword weight)
  (with-transaction
   ()
   (push (make-instance 'preference :keyword keyword :weight weight)
         #!*root*)
   (first #!*root*)))

(defmethod initialize-instance :after ((this preference)
                                       &key keyword objid
                                       &allow-other-keys)
  (format t "(1) Here is initialize-instance :after objid ~A~%" objid)
  (format t "(1) this is ~A~%" this)
  (unless objid
    (push this (getbtree keyword #!*keyword-index*)))
  (format t "(2) Here is initialize-instance :after~%")
  )

(defun preference-instances (keyword)
  (getbtree keyword #!*keyword-index*))

(defun remove-preference-instance (keyword instance)
  (let ((new-instances (remove instance (getbtree keyword #!*keyword-index*))))
    (if new-instances
        (setf (getbtree keyword #!*keyword-index*) new-instances)
      (rembtree keyword #!*keyword-index*))
    new-instances))
