;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-metaclass-2.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	9.3.94
;;;; Description	PLOB metaclass for persistent classes
;;;;
;;;; The bibliography references used are:
;;;; [AP91]	Andreas Paepke:
;;;;		User-Level Language Crafting:
;;;;		Introducing the CLOS Metaobject Protocol
;;;;		HP Laboratories Technical Report HPL-91-169, October, 1991
;;;; and from:
;;;; [MOP91]	Gregor Kiczales, Jim des Rivieres, and Daniel G. Bobrow:
;;;;		The Art of the Metaobject Protocol
;;;;		The MIT Press, Cambridge, Massachusetts, 1991
;;;;
;;;; Copyright	PLOB! Copyright 1994--2001 Heiko Kirschke.
;;;;		All rights reserved.
;;;;
;;;; Unlimited use, reproduction, modification and distribution of
;;;; this software is permitted.  Any copy or modified version of this
;;;; software must include both the above copyright notice of Heiko
;;;; Kirschke and this paragraph; for each modified version, an
;;;; additional statement must be added telling the year of
;;;; modification and quoting the author of the modification.  Any
;;;; distribution of this software must comply with all applicable
;;;; German export control laws.  This software is made available AS
;;;; IS, and HEIKO KIRSCHKE DISCLAIMS ALL WARRANTIES, EXPRESS OR
;;;; IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AND
;;;; NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
;;;; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
;;;; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT
;;;; (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF HEIKO
;;;; KIRSCHKE IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
;;;;
;;;; Please note that these license terms adhere only to the code of
;;;; PLOB!  itself. PLOB! uses POSTORE (Persistent Object Store) as a
;;;; low-level persistent memory; it is provided in binary form within
;;;; PLOB! with the permission of the University of St. Andrews
;;;; (http://www-ppg.dcs.st-andrews.ac.uk/Default.html).  Contact the
;;;; University of St. Andrews for getting their license terms on
;;;; POSTORE.
;;;;
;;;; $Header$
;;;;
;;;; --------------------------------------------------------------------------

(in-package :plob)

;;; ---------------------------------------------------------------------------
(defvar *in-bootstrap-p* nil
  #+:lisp-doc "
\\Purposelabel
 A flag indicating if the bootstrap is active. If \\nonnil,
 this indicates to some functions that the system is in the
 bootstrap phase; this will causes some methods to call
 low-level accessor functions directly.
\\Seealsolabel
 \\Fcite{*in-bootstrap-wait-timeout*};
 \\fcite{open-heap}.")

;;; ---------------------------------------------------------------------------
;;; HACK: Unknown slot options will cause the LispWorks compiler to #$%&^;
;;; so the extra slot options must be handled here.
;;; See also [MOP91], p. 286, (defun canonicalize-direct-slot ...)
;;; ---------------------------------------------------------------------------
#+:lispworks
(defconstant +extra-slot-options+ '(:extent :deferred :index :location)
  #+:lisp-doc "
\\Purposelabel
 List of additional allowed \\clos\\ slot options.
\\Seealsolabel
 \\Fcite{canonicalize-defclass-slot :around (persistent-metaclass t)}.")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod clos::canonicalize-defclass-slot :around
  ((prototype persistent-metaclass) slot)
  #+:lisp-doc "\\lw\\ signals an error on unknown slot options;
 so this method removes any extra allowed options
 before calling the default method and returns the canonicalized extra
 options concatenated to the result of the default method.
 The extra allowed options are the value of the
 \\fcite{+extra-slot-options+}."
  (let ((extra-slot-options ())
        (rest-options ())
        (result ()))
    (do ((olist (cdr slot) (cddr olist)))
        ((null olist))
      (let ((option (car olist)))
        (cond
         ((find option +extra-slot-options+)
          (setf (assoc option extra-slot-options) (cadr olist)))
         (t
          (push (cadr olist) rest-options)
          (push (car olist) rest-options))))
    (setf result (call-next-method prototype (cons (car slot) rest-options))))
    (dolist (option extra-slot-options)
      (push-on-end (car option) result)
      (push-on-end `(quote ,(cdr option)) result))
    result))

;;; ---------------------------------------------------------------------------
;;; HACK: Unknown class options will cause the LispWorks compiler to barf;
;;; so the extra class options must be handled here.
;;; ---------------------------------------------------------------------------
#+:lispworks
(defconstant +extra-class-options+
  '(:constructor :dependent :extent :schema-evolution)
  #+:lisp-doc "
\\Purposelabel
 List of additional allowed \\clos\\ class options.
\\Seealsolabel
 \\Fcite{canonicalize-class-options :around (persistent-metaclass t)}.")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod clos::canonicalize-class-options :around
  ((prototype persistent-metaclass) class-options)
  #+:lisp-doc "\\lw\\ signals an error on unknown class options;
 so this method removes any extra allowed options
 before calling the default method and returns the canonicalized extra
 options concatenated to the result of the default method.
 The extra allowed options are the value of the
 \\fcite{+extra-class-options+}."
  (let ((extra-class-options nil)
	(rest-options ())
	(result ()))
    (dolist (o class-options)
      (let ((option (car o)))
        (cond
         ((find option +extra-class-options+)
          (setf (assoc option extra-class-options) (cadr o)))
         (t
	  (push o rest-options)))))
    (setf result (call-next-method prototype rest-options))
    (dolist (option extra-class-options)
      (push-on-end (car option) result)
      (push-on-end `(quote ,(cdr option)) result))
    result))

;;; ---------------------------------------------------------------------------
;;; Slot metaclasses: persistent-direct-slot-definition,
;;; persistent-effective-slot-definition
;;; ---------------------------------------------------------------------------

(defclass persistent-direct-slot-definition
     (standard-direct-slot-definition)
  (
   (t-extent
    :accessor persistent-slot-definition-extent
    :initarg :extent
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The extent of the slot. This is the value of the
 \\lisp{:extent}\\ slot option given in the
 \\lisp{defclass}\\ statement.")

   (t-deferred
    :accessor persistent-slot-definition-deferred
    :initarg :deferred
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The deferred option of the slot. This is the value of the
 \\lisp{:deferred}\\ slot option given in the
 \\lisp{defclass}\\ statement.")

   (t-index
    :accessor persistent-slot-definition-index
    :initarg :index
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The index to maintain for the slot. This is the value of the
 \\lisp{:index}\\ slot option given in the
 \\lisp{defclass}\\ statement.")

   (t-location
    :accessor persistent-slot-definition-location
    :initarg :location
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The location of the slot in the low-level representation.
 This is the value of the \\lisp{:location}\\ slot option given
 in the \\lisp{defclass}\\ statement. The \\lisp{:location}\\ slot
 option must only be specified for the built-in predefined classes.")

   (description
    :accessor persistent-slot-definition-description
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The persistent slot description object
 of the slot, an instance of \\fcite{direct-slot-description}.")

   )
  #+:lisp-doc (:documentation "
\\Purposelabel
 A slot metaclass for direct slot descriptions.
\\Remarkslabel
 It looks as if in \\lw\\ multiple inheritance for subclasses of
 \\class{standard-direct-slot-definition}\\ and
 \\class{standard-effective-slot-definition}\\ is
 not possible; so the \\spc\\ \\sltmc{}es are declared for each
 \\class{persistent-\\{direct,effective\\}-slot-definition}\\ seperately.
 When I used multiple
 inheritance, very strange errors occured when classes using these
 \\spc\\ \\sltmc{}es were defined (\\lw\\ tried to finalize a
 class which was not used at all by the \\sltmc{}es defined here;
 this looks to me like internal problems with multiple inheritance on
 (at least \\spc\\ \\slt-) \\mc[es]).
\\Seealsolabel
 \\Fcite{persistent-effective-slot-definition}."))

;;; ---------------------------------------------------------------------------
(defclass persistent-effective-slot-definition
     (standard-effective-slot-definition)
  ((t-extent
    :accessor persistent-slot-definition-extent
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 See slot {\\bf t-extent} of
 \\fcite{persistent-direct-slot-definition}.")

   (t-deferred
    :accessor persistent-slot-definition-deferred
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 See slot {\\bf t-deferred} of
 \\fcite{persistent-direct-slot-definition}.")

   (t-index
    :accessor persistent-slot-definition-index
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 See slot {\\bf t-index} of
 \\fcite{persistent-direct-slot-definition}.")

   (t-location
    :accessor persistent-slot-definition-location
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 See slot {\\bf t-location} of
 \\fcite{persistent-direct-slot-definition}.")

   (t-allocation
    :accessor persistent-slot-definition-allocation
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The allocation
 of the transient standard-effective-slot definition.")

   (description
    :accessor persistent-slot-definition-description
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The persistent slot description object
 of the slot, an instance of
 \\fcite{effective-slot-description}."))

  #+:lisp-doc (:documentation "
\\Purposelabel
 A slot metaclass for effective slot descriptions.
\\Seealsolabel
 \\Fcite{persistent-direct-slot-definition}."))

;;; ---------------------------------------------------------------------------
;;; Method metaclasses: persistent-reader-method, persistent-writer-method
;;;
;;; My first idea for an efficient access to persistent slots was as follows:
;;;
;;;  1.	I define my own reader- and writer-method metaobject classes
;;;	persistent-reader-method and persistent-writer-method
;;;
;;;  2.	These method metaobject classes are used from instances of
;;;	persistent-metaclass (which are in turn classes with persistent
;;;	instances) by returning the class metaobjects of
;;;	persistent-reader-method and persistent-writer-method from a call
;;;	to the methods reader-method-class resp. writer-method-class
;;;	specialized to persistent-metaclass.
;;;
;;;  3.	The efficient slot access code is computed by make-method-lambda
;;;	specialized to persistent-reader-method resp.
;;;	persistent-writer-method.
;;;
;;; Since LispWorks 3.1.1 does not support the generic functions
;;; reader-method-class resp. writer-method-class, slot access has to be
;;; done the 'hard (and slow) way' with methods for slot-value-using-class
;;; specialized to persistent-metaclass.
;;;
;;; Still more onerous in LispWorks, the slot-definition-allocation of a slot
;;; cannot be set to a value others than :class or :instance; this violates
;;; the specification as given in [MOP91], p. 101.
;;; ---------------------------------------------------------------------------

(defclass persistent-reader-method (standard-reader-method)
  ()
  #+:lisp-doc (:documentation
   "
\\Purposelabel
 A method metaclass for reader methods for persistent
 classes."))

;;; ---------------------------------------------------------------------------
(defclass persistent-writer-method (standard-writer-method)
  ()
  #+:lisp-doc (:documentation
   "
\\Purposelabel
 A method metaclass for writer methods for persistent
 classes."))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
