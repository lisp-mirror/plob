;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-sexpr.lisp
;;;; Author	Heiko Kirschke
;;;; Copyright	(C) 1997 Heiko Kirschke
;;;; Date	1997/09/08
;;;; Description	Storing/loading of s-expressions.
;;;; --------------------------------------------------------------------------

(in-package :plob)

;;; ---------------------------------------------------------------------------
;;; Classes
;;; ---------------------------------------------------------------------------
(defclass persistent-s-expression ()

 ((source :initform nil :initarg :source
	  :accessor sexpr-source
	  :extent :cached-write-through
	  #+:lisp-doc :documentation #+:lisp-doc "
 Source code of the s-expression.")

  (code :initform nil :initarg :code
	:accessor sexpr-code
	:extent :transient
	#+:lisp-doc :documentation #+:lisp-doc "
 Compiled version of source code as found in slot
 source."))

 (:metaclass persistent-metaclass)

 #+:lisp-doc (:documentation "A class for persistent s-expressions."))

;;; ---------------------------------------------------------------------------
;;; Interface: Generic functions
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Implementation: Methods
;;; ---------------------------------------------------------------------------
(defmethod (setf sexpr-source) :around
	   (new-value (object persistent-s-expression))
  (let ((*default-depth* :cached))
    (call-next-method)
    (setf (sexpr-code object) nil))
  new-value)

(defmethod sexpr-code :around ((object persistent-s-expression))
  (let ((code (call-next-method)))
    (unless code
      (let ((*default-depth* :cached)
	    (source (sexpr-source object)))
	(setf code 
	  (if (eq (car source) 'defun)
	      (compile (cadr source) (cons 'lambda (cddr source)))
	    (compile nil source))))
      (setf (sexpr-code object) code))
    code))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
