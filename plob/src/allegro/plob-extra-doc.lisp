;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-extra-doc.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	1.6.94
;;;; Description	PLOB extra documentation items
;;;;	This file contains no real LISP code; the definitions found here
;;;;	are only for documentation purpose, e.g. for the [MOP91] generic
;;;;	functions which have methods defined in PLOB, a short reference
;;;;	to [MOP91] is given where the generic function itself is defined.
;;;;	This file is read additionally by function scan-plob-files when
;;;;	making the reference manual.
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
#+:lisp-doc
(defgeneric clos::canonicalize-class-options
     (clos::prototype clos::class-options)
  #+:lisp-doc (:documentation "
 See \\fcite{canonicalize-defclass-options}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::canonicalize-defclass-slot
     (clos::prototype clos::slot)
  #+:lisp-doc (:documentation "
 See \\fcite{canonicalize-direct-slot}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::class-slots (clos::class)
  #+:lisp-doc (:documentation "
 See \\fcite{class-slots}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::class-name (clos::class)
  #+:lisp-doc (:documentation "
 See \\fcite{class-name}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::class-prototype (clos::class)
  #+:lisp-doc (:documentation "
 See \\fcite{class-prototype}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::compute-effective-slot-definition
     (clos::class clos::name clos::direct-slot-definitions)
  #+:lisp-doc (:documentation "
 See \\fcite{compute-effective-slot-definition}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::direct-slot-definition-class
     (clos::class &rest clos::initargs)
  #+:lisp-doc (:documentation "
 See \\fcite{direct-slot-definition-class}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::effective-slot-definition-class
     (clos::class &rest clos::initargs)
  #+:lisp-doc (:documentation "
 See \\fcite{effective-slot-definition-class}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::finalize-inheritance (clos::class)
  #+:lisp-doc (:documentation "
 See \\fcite{finalize-inheritance}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::initialize-instance (clos::instance &rest clos::initargs)
  #+:lisp-doc (:documentation "
 See \\fcite{initialize-instance}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::make-method-lambda
     (clos::generic-function clos::method clos::lambda-expression
                             clos::environment)
  #+:lisp-doc (:documentation "
 See \\fcite{make-method-lambda}. This \\gfn\\ has a non-\\mop-conforming
 $\\lambda$-list in \\lw\\ 3.1.1."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::make-instance (clos::class &rest clos::initargs)
  #+:lisp-doc (:documentation "
 See \\fcite{make-instance}."))

;;; ---------------------------------------------------------------------------
;; #+:lisp-doc
#+:never
(defgeneric clos::make-instances-obsolete (clos::class)
  #+:lisp-doc (:documentation "
 See \\fcite{make-instances-obsolete}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::obsolete-instance-trap-internal
     (clos::class clos::old-instance clos::old-wrapper)
  #+:lisp-doc (:documentation "

\\Argumentslabel

 \\isacls{\\funarg{class}}
 \\isa{\\funarg{old-instance}}
      {an obsolete \\clos\\ instance}
 \\isa{\\funarg{old-wrapper}}
      {the old class wrapper with a minimal class description
       for \\funarg{old-instance}}

\\Purposelabel

 This generic function is called by \\lw\\ when an instance
 is referenced whose class was marked as having obsolete
 instances by a call to the \\fcite{make-instances-obsolete}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::print-object (clos::instance stream)
  #+:lisp-doc (:documentation "
 See \\fcite{print-object}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::reader-method-class
     (clos::class clos::direct-slot &rest clos::initargs)
  #+:lisp-doc (:documentation "
 See \\fcite{reader-method-class}. This \\gfn\\ is not supported by
 \\lw\\ 3.1.1."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::reinitialize-instance (clos::instance &rest clos::initargs)
  #+:lisp-doc (:documentation "
 See \\fcite{reinitialize-instance}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::shared-initialize
 (clos::instance clos::slot-names &rest clos::initargs)
  #+:lisp-doc (:documentation "
 See \\fcite{shared-initialize}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::slot-boundp-using-class
     (clos::class clos::object clos::slot)
  #+:lisp-doc (:documentation "
 See \\fcite{slot-boundp-using-class}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::slot-definition-allocation (clos::slot)
  #+:lisp-doc (:documentation "
 See \\fcite{slot-definition-allocation}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::slot-definition-initargs (clos::slot)
  #+:lisp-doc (:documentation "
 See \\fcite{slot-definition-initargs}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::slot-definition-location (clos::slot)
  #+:lisp-doc (:documentation "
 See \\fcite{slot-definition-location}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::slot-definition-name (clos::slot)
  #+:lisp-doc (:documentation "
 See \\fcite{slot-definition-name}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::slot-definition-type (clos::slot)
  #+:lisp-doc (:documentation "
 See \\fcite{slot-definition-type}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::slot-makunbound-using-class
     (clos::class clos::object clos::slot)
  #+:lisp-doc (:documentation "
 See \\fcite{slot-makunbound-using-class}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::slot-value-using-class
     (clos::class clos::object clos::slot)
  #+:lisp-doc (:documentation "
 See \\fcite{slot-value-using-class}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric (setf clos::slot-value-using-class)
     (clos::new-value clos::class clos::object clos::slot)
  #+:lisp-doc (:documentation "
 See \\fcite{(setf slot-value-using-class)}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::update-instance-for-redefined-class
     (clos::instance clos::added-slot clos::discarded-slots
                     clos::property-list &rest clos::initargs)
  #+:lisp-doc (:documentation "
 See \\fcite{update-instance-for-redefined-class}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::validate-superclass (clos::class clos::superclass)
  #+:lisp-doc (:documentation "
 See \\fcite{validate-superclass}."))

;;; ---------------------------------------------------------------------------
#+:lisp-doc
(defgeneric clos::writer-method-class
     (clos::class clos::direct-slot &rest clos::initargs)
  #+:lisp-doc (:documentation "
 See \\fcite{writer-method-class}. This \\gfn\\ is not supported by
 \\lw\\ 3.1.1."))

;;; ---------------------------------------------------------------------------
#+(and :lispworks ::lisp-doc)
(defgeneric lispworks:get-inspector-values
     (lispworks::object lispworks::mode)
  #+:lisp-doc (:documentation "
 See \\fcite{get-inspector-values}."))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
