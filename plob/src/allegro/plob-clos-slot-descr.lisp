;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-clos-slot-descr.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	9.3.94
;;;; Description	PLOB allocate and accessor functions for
;;;;		LISP CLOS slot descriptions
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
;;;; Copyright	PLOB! Copyright 1994--2002 Heiko Kirschke.
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
;;; Transient CLOS slot description
;;; ---------------------------------------------------------------------------
(defun fill-slot-description-into
    (slot-definition t-slot-descr the-class default-slot-extent)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{slot-definition}}
      {a slot definition metaobject}
 \\isa{\\funarg{t-slot-descr}}
      {a slot description}
 \\isacls{\\funarg{the-class}}
\\Valueslabel
 \\retarg{\\funarg{t-slot-descr}}
\\Purposelabel
 A workhorse for different methods of \\fcite{fill-slot-description}.
\\Seealsolabel
 \\Fcite{fill-slot-description}."

  (setf (slot-description-name t-slot-descr)
    (slot-definition-name slot-definition))
  (setf (slot-description-initargs t-slot-descr)
    (slot-definition-initargs slot-definition))
  (setf (slot-description-initform t-slot-descr)
    (slot-definition-initform slot-definition))
  #+:store-functions
  (setf (slot-description-initfunction t-slot-descr)
    (slot-definition-initfunction slot-definition))
  #-:store-functions
  (setf (slot-description-initfunction t-slot-descr)
    (not (null (slot-definition-initfunction slot-definition))))
  (setf (slot-description-type t-slot-descr)
    (slot-definition-type slot-definition))
  (let ((extent nil))
    (when the-class
      (setf extent (slot-extent slot-definition the-class))
      (unless extent
	(setf extent (class-extent the-class))))
    (setf (slot-description-extent t-slot-descr)
      (if extent extent default-slot-extent)))
  (let ((deferred nil))
    (when the-class
      (setf deferred (slot-deferred slot-definition the-class)))
    (setf (slot-description-deferred t-slot-descr) deferred))
  t-slot-descr)

;;; ---------------------------------------------------------------------------
(defconstant +clos-slot-extents+ '(:transient :cached)
  #+:lisp-doc "
\\Purposelabel
 List with allowed slot extents for non-\\plob\\ \\clos\\ classes,
 i.e.\\ for \\clos\\ classes without a
 \\lisp{(:metaclass persistent-metaclass)}\\ class option.
\\Seealsolabel
 \\Fcite{(setf slot-extent)};
 \\fcite{+plob-slot-extents+};
 \\fcite{+structure-slot-extents+}.")

;;; ---------------------------------------------------------------------------
(defun fill-transient-slot-description
    (slot-definition the-class t-slot-descr)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{slot-definition}}
      {an instance of [a superclass of]
       \\class{standard-direct-slot-definition}\\ or
       \\class{standard-effective-slot-definition}}
 \\isacls{\\funarg{the-class}}
 \\isa{\\funarg{t-slot-descr}}
      {a slot description}
\\Valueslabel
 \\retarg{\\funarg{t-slot-descr}}
\\Purposelabel
 A workhorse for different methods of \\fcite{fill-slot-description}.
\\Seealsolabel
 \\Fcite{fill-slot-description}."

  (fill-slot-description-into
   slot-definition t-slot-descr the-class *default-clos-slot-extent*)
  (let ((slot-extent (slot-description-extent t-slot-descr)))
    (unless (member slot-extent +clos-slot-extents+)
      (error +illegal-extent-error-prompt+
	     slot-extent (slot-definition-name t-slot-descr)
	     the-class +clos-slot-extents+)))
  (setf (slot-description-allocation t-slot-descr)
    (slot-definition-allocation slot-definition))
  t-slot-descr)

;;; ---------------------------------------------------------------------------
(defconstant +plob-slot-extents+
    '(:transient :cached :cached-write-through :persistent
      :object :objid)
  #+:lisp-doc "
\\Purposelabel
 List with allowed slot extents for \\plob\\ \\clos\\ classes,
 i.e.\\ for \\clos\\ classes with a
 \\lisp{(:metaclass persistent-metaclass)}\\ class option.
\\Seealsolabel
 \\Fcite{(setf slot-extent)};
 \\fcite{+clos-slot-extents+};
 \\fcite{+plob-slot-write-through-extents+};
 \\fcite{+plob-slot-represented-extents+};
\\fcite{+structure-slot-extents+}.")

;;; ---------------------------------------------------------------------------
(defun fill-persistent-slot-description
    (slot-definition the-class t-slot-descr)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{slot-definition}}
      {an instance of [a subclass of]
       \\class{persistent-direct-slot-definition}\\ or
       \\class{persistent-effective-slot-definition}}
 \\isacls{\\funarg{the-class}}
 \\isa{\\funarg{t-slot-descr}}
      {a slot description}
\\Valueslabel
 \\retarg{\\funarg{t-slot-descr}}
\\Purposelabel
 A workhorse for different methods of \\fcite{fill-slot-description}.
\\Seealsolabel
 \\Fcite{fill-slot-description}."

  (fill-slot-description-into
   slot-definition t-slot-descr the-class *default-plob-slot-extent*)
  (let ((slot-extent (slot-description-extent t-slot-descr)))
    (unless (member slot-extent +plob-slot-extents+)
      (error +illegal-extent-error-prompt+
	     slot-extent (slot-definition-name t-slot-descr)
	     the-class +plob-slot-extents+)))
  (setf (slot-description-index t-slot-descr)
    (persistent-slot-definition-index slot-definition))
  t-slot-descr)

;;; ---------------------------------------------------------------------------
(defmethod fill-slot-description :around
	   (slot-definition the-class
	    &optional t-slot-descr (p-heap *default-persistent-heap*))
  #+:lisp-doc "Make sure that only a transient description will be filled."
  (declare (ignore slot-definition the-class t-slot-descr p-heap))
  (let ((*transient-slot-value* t))
    (call-next-method)))

;;; ---------------------------------------------------------------------------
(defmethod fill-slot-description
    ((slot-definition direct-slot-definition) the-class
     &optional t-slot-descr (p-heap *default-persistent-heap*))
  (unless t-slot-descr
    (setf t-slot-descr (make-instance 'direct-slot-description
			 :p-heap p-heap
			 :store-cached-slots nil)))
  (fill-transient-slot-description slot-definition the-class t-slot-descr)
  (setf (slot-description-readers t-slot-descr)
    (slot-definition-readers slot-definition))
  (setf (slot-description-writers t-slot-descr)
    (slot-definition-writers slot-definition))
  t-slot-descr)

;;; ---------------------------------------------------------------------------
(defmethod fill-slot-description
    ((slot-definition effective-slot-definition) the-class
     &optional t-slot-descr (p-heap *default-persistent-heap*))
  (unless t-slot-descr
    (setf t-slot-descr (make-instance 'effective-slot-description
			 :p-heap p-heap
			 :store-cached-slots nil)))
  (fill-transient-slot-description slot-definition the-class t-slot-descr))

;;; ---------------------------------------------------------------------------
(defmethod fill-slot-description
  ((slot-definition persistent-direct-slot-definition) the-class
   &optional t-slot-descr (p-heap *default-persistent-heap*))
  (unless t-slot-descr
    (setf t-slot-descr (make-instance 'direct-slot-description
			 :p-heap p-heap
			 :store-cached-slots nil)))
  (fill-persistent-slot-description slot-definition the-class t-slot-descr)
  (setf (slot-description-allocation t-slot-descr)
        (slot-definition-allocation slot-definition))
  (setf (slot-description-readers t-slot-descr)
        (slot-definition-readers slot-definition))
  (setf (slot-description-writers t-slot-descr)
        (slot-definition-writers slot-definition))
  t-slot-descr)

;;; ---------------------------------------------------------------------------
(defmethod fill-slot-description
  ((slot-definition persistent-effective-slot-definition) the-class
   &optional t-slot-descr (p-heap *default-persistent-heap*))
  (unless t-slot-descr
    (setf t-slot-descr (make-instance 'effective-slot-description
			 :p-heap p-heap
			 :store-cached-slots nil)))
  (fill-persistent-slot-description slot-definition the-class t-slot-descr)
  (setf (slot-description-allocation t-slot-descr)
    (persistent-slot-definition-allocation slot-definition))
  (setf (slot-description-location t-slot-descr)
    (persistent-slot-definition-location slot-definition))
  t-slot-descr)

;;; ---------------------------------------------------------------------------
(defun slot-description-equal-p-1 (t-slot-descr p-slot-descr next-method
				   &optional verbose)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-slot-descr}\\ resp.\\ \\funarg{p-slot-descr}}
      {a slot description}
 \\isa{\\funarg{next-method}}
      {a function taking no arguments}
\\Valueslabel
 See \\fcite{slot-description-equal-p}.
\\Purposelabel
 A workhorse for different methods of \\fcite{slot-description-equal-p}.
\\Seealsolabel
 \\Fcite{slot-description-equal-p}."

  (multiple-value-bind (equal-p reason)
      (funcall next-method)
    (block nil
      (unless equal-p
	(return (values nil reason)))
      (unless (equal (slot-definition-initargs t-slot-descr)
		     (slot-definition-initargs p-slot-descr))
	(return (values nil
			(when verbose
			  (format nil "slot ~A initargs; tr.: ~A / pe.: ~A"
				  (slot-description-name t-slot-descr)
				  (slot-definition-initargs t-slot-descr)
				  (slot-definition-initargs p-slot-descr))))))
      (unless (equal (slot-definition-initform t-slot-descr)
		     (slot-definition-initform p-slot-descr))
	(return (values nil
			(when verbose
			  (format nil "slot ~A initargs; tr.: ~A / pe.: ~A"
				  (slot-description-name t-slot-descr)
				  (slot-definition-initform t-slot-descr)
				  (slot-definition-initform p-slot-descr))))))
      (unless (eq (slot-definition-allocation t-slot-descr)
		  (slot-definition-allocation p-slot-descr))
	(return (values nil
			(when verbose
			  (format nil "slot ~A allocation; tr.: ~A / pe.: ~A"
				  (slot-description-name t-slot-descr)
				  (slot-definition-allocation t-slot-descr)
				  (slot-definition-allocation
				   p-slot-descr))))))
      t)))

;;; ---------------------------------------------------------------------------
(defmethod slot-description-equal-p
     ((t-slot-descr direct-slot-description)
      (p-slot-descr direct-slot-description)
      &optional verbose)
  (multiple-value-bind (equal-p reason)
      (slot-description-equal-p-1 t-slot-descr p-slot-descr
				  #'call-next-method verbose)
    (block nil
      (unless equal-p
	(return (values nil reason)))
      (unless (equal (slot-description-index t-slot-descr)
		     (slot-description-index p-slot-descr))
	(return (values nil
			(when verbose
			  (format nil "slot ~A index; tr.: ~A / pe.: ~A"
				  (slot-description-name t-slot-descr)
				  (slot-description-index t-slot-descr)
				  (slot-description-index p-slot-descr))))))
      t)))

;;; ---------------------------------------------------------------------------
(defmethod slot-description-equal-p
     ((t-slot-descr effective-slot-description)
      (p-slot-descr effective-slot-description)
      &optional verbose)
  (slot-description-equal-p-1 t-slot-descr p-slot-descr
			      #'call-next-method verbose))

;;; ---------------------------------------------------------------------------
;;; Persistent CLOS slot description
;;; ---------------------------------------------------------------------------
#+(and :lisp-doc (not :document-api))
(:defdoc
 "p-slot-description- ..."
 "Slot Description Accessors"
 "
\\Purposelabel
 All following functions
 {\\bf p-slot-description-\\textsl{\\lt{}slot-name\\gt}} and
 {\\bf (setf p-slot-description-\\textsl{\\lt{}slot-name\\gt}\\/)}
 with \\textsl{\\lt{}slot-name\\gt} being one of the slot names of
 \\fcite{slot-description}, \\fcite{direct-slot-description}\\ or
 \\fcite{effective-slot-description}\\ without the `p-' prefix
 access directly a slot of a persistent \\clos\\ instance
 of one of these classes in the \\sh.

 The accessor functions defined here are merely used in the
 bootstrap phase. After the bootstrap, the persistent objects of
 these classes are created and accessed as all
 persistent \\clos\\ instances by employing their
 class-descriptions, i.e.\\ all information needed to create
 a persistent \\clos\\ instance or to access a persistent
 \\clos\\ instance's slot is contained in its class-description.
\\Seealsolabel
 \\Fcite{slot-description};
 \\fcite{direct-slot-description};
 \\fcite{effective-slot-description};
 section \\fcite{bootstrap ...}.")

;;; ---------------------------------------------------------------------------
(defun p-slot-description-name
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-name}
 of the persistent slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-name+
	     depth
	     p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-name)
     (t-name p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-name}}
      {a symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-name}
 of the persistent slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-name}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-name+
		   depth
		   p-heap)
      t-name)))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-initargs
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-initargs}
 of the persistent slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-init-args+
	     depth
	     p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-initargs)
     (t-initargs p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-initargs}}
      {a list of (keyword) symbols}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-initargs}
 of the persistent slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-initarg}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-init-args+
	          depth
                  p-heap)
      t-initargs)))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-initform
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-initform}
 of the persistent slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-init-form+
	     depth
	     p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-initform)
     (t-initform p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-initform}}
      {an expression}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-initform}
 of the persistent slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-initform}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-init-form+
		   depth
		   p-heap)
      t-initform)))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-initfunction
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-initfunction}
 of the persistent slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-init-function+
	     depth
	     p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-initfunction)
    (t-initfunction p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-initfunction}}
      {a function}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-initfunction}
 of the persistent slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-initfunction}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-init-function+
		   depth
		   p-heap)
      t-initfunction)))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-type
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-type}
 of the persistent slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-type+
	     depth
	     p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-type)
    (t-type p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-type}}
      {a type specifier}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-type}
 of the persistent slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-type}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-type+
	          depth
                  p-heap)
      t-type)))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-allocation
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-allocation}
 of the persistent slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-allocation+
	     depth
	     p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-allocation)
    (t-allocation p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-allocation}}
      {a keyword symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-allocation}
 of the persistent slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-allocation}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
   (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
	          +slot-description-location-allocation+
	          depth
                  p-heap)
         t-allocation)))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-extent
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-extent}
 of the persistent slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-extent+
	     depth
	     p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-extent)
    (t-extent p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-extent}}
      {a (keyword) symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-extent}
 of the persistent slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-extent}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-extent+
		   depth
		   p-heap)
      t-extent)))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-deferred
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-deferred}
 of the persistent slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-deferred+
	     depth
	     p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-deferred)
    (t-deferred p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-deferred}}
      {a (keyword) symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-deferred}
 of the persistent slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-deferred}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-deferred+
		   depth
		   p-heap)
      t-deferred)))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-index
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-index}
 of the persistent slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-index+
	     depth
	     p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-index)
    (t-index p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-index}}
      {an index-defining expression }
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-index}
 of the persistent slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-index}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-index+
		   depth
		   p-heap)
      t-index)))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-into
    (t-into-descr p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-into-descr}}
      {a slot-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Read the persistent slot-description referenced by
 \\funarg{p-objid}\\ destructively into \\funarg{t-into-descr}.
\\Seealsolabel
 \\Fcite{p-direct-slot-description};
 \\fcite{p-effective-slot-description}."

  (setf (persistent-object-objid t-into-descr) p-objid)
  (setf (slot-description-name t-into-descr)
	(p-slot-description-name p-objid depth p-heap))
  (setf (slot-description-initargs t-into-descr)
	(p-slot-description-initargs p-objid depth p-heap))
  (setf (slot-description-initform t-into-descr)
	(p-slot-description-initform p-objid depth p-heap))
  #+:store-functions
  (setf (slot-description-initfunction t-into-descr)
	(p-slot-description-initfunction p-objid depth p-heap))
  (setf (slot-description-type t-into-descr)
	(p-slot-description-type p-objid depth p-heap))
  (setf (slot-description-allocation t-into-descr)
	(p-slot-description-allocation p-objid depth p-heap))
  (setf (slot-description-extent t-into-descr)
	(p-slot-description-extent p-objid depth p-heap))
  (setf (slot-description-deferred t-into-descr)
	(p-slot-description-deferred p-objid depth p-heap))
  (setf (slot-description-index t-into-descr)
	(p-slot-description-index p-objid depth p-heap))
  t-into-descr)

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description)
  (t-descr p-objid
   &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a slot-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient slot-description in
 \\funarg{t-descr}\\ to the
 persistent slot-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-direct-slot-description};
 \\fcite{p-effective-slot-description}."

  (setf (persistent-object-objid t-descr) p-objid)
  (setf (p-slot-description-name p-objid depth p-heap)
	(slot-definition-name t-descr))
  (setf (p-slot-description-initargs p-objid depth p-heap)
	(slot-definition-initargs t-descr))
  (setf (p-slot-description-initform p-objid depth p-heap)
	(slot-definition-initform t-descr))
  #+:store-functions
  (setf (p-slot-description-initfunction p-objid depth p-heap)
	(slot-definition-initfunction t-descr))
  (setf (p-slot-description-type p-objid depth p-heap)
	(slot-definition-type t-descr))
  (setf (p-slot-description-allocation p-objid depth p-heap)
	(slot-definition-allocation t-descr))
  (setf (p-slot-description-extent p-objid depth p-heap)
	(slot-description-extent t-descr))
  (setf (p-slot-description-deferred p-objid depth p-heap)
	(slot-description-deferred t-descr))
  (setf (p-slot-description-index p-objid depth p-heap)
	(slot-description-index t-descr))
  t-descr)

;;; ---------------------------------------------------------------------------
;;; Persistent CLOS direct slot description
;;; ---------------------------------------------------------------------------
(defun p-allocate-direct-slot-description 
  (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 direct-slot-description
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{direct-slot-description};
 \\fcite{p-allocate}."

  (p-allocate-instance *direct-slot-description-objid* p-heap))

;;; ---------------------------------------------------------------------------
(defun p-direct-slot-description-p
  (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of
 \\fcite{direct-slot-description},
 \\lispnil\\ otherwise."

  (and (= (p-type-tag-of p-objid p-heap) +instance-type-tag+)
       (with-transaction (p-heap)
	 (= (p-index p-heap p-objid +clos-location-class-wrapper+)
	    *direct-slot-description-objid*))))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-readers
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-readers}
 of the persistent direct-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-readers+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-readers)
     (t-readers p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-readers}}
      {a list of reader function symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-readers}
 of the persistent direct-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-readers}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-readers+
		   depth p-heap)
      t-readers)))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-writers
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-writers}
 of the persistent direct-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-writers+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-writers)
     (t-writers p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-writers}}
      {a list of writer function symbols}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-writers}
 of the persistent direct-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-writers}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-writers+
	          depth p-heap)
      t-writers)))

;;; ---------------------------------------------------------------------------
(defun store-direct-slot-description (t-descr p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a direct-slot-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient direct-slot-description in
 \\funarg{t-descr}\\ to the
 persistent direct-slot-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-direct-slot-description};
 \\fcite{p-effective-slot-description}."

  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-descr))
      (unless p-objid
	(setf p-objid (p-allocate-direct-slot-description p-heap))
	(setf (persistent-object-objid t-descr) p-objid)
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-descr depth
			       +instance-type-tag+ force-write)
	(setf (p-slot-description p-objid depth p-heap) t-descr)
	(setf (p-slot-description-readers p-objid depth p-heap)
	  (slot-definition-readers t-descr))
      (setf (p-slot-description-writers p-objid depth p-heap)
	(slot-definition-writers t-descr)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-direct-slot-description)
    (t-descr &optional p-objid (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a direct-slot-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient direct-slot-description in
 \\funarg{t-descr}\\ to the
 persistent direct-slot-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-direct-slot-description};
 \\fcite{p-effective-slot-description}."

  (values t-descr
	  (store-direct-slot-description t-descr p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-direct-slot-description 
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 direct-slot-description
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-direct-slot-description)}."

  (let* ((t-object (is-registered-objid p-objid))
	 (force-read (null t-object)))
    (unless t-object
      (let ((*transient-slot-value* t))
	(setf t-object (make-instance 'direct-slot-description
			 :p-heap p-heap
			 :store-cached-slots nil)))
      (register-to-cache p-objid t-object)
      (with-transaction (p-heap)
       (with-read-lock (p-heap p-objid depth +instance-type-tag+ force-read)
	 (p-slot-description-into t-object p-objid depth p-heap)
	 (setf (slot-description-readers t-object)
	   (p-slot-description-readers p-objid depth p-heap))
	 (setf (slot-description-writers t-object)
	   (p-slot-description-writers p-objid depth p-heap)))))
    t-object))

;;; ---------------------------------------------------------------------------
;;; Persistent CLOS effective slot description
;;; ---------------------------------------------------------------------------
(defun p-allocate-effective-slot-description 
  (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 effective-slot-description
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{effective-slot-description};
 \\fcite{p-allocate}."

  (p-allocate-instance *effective-slot-description-objid* p-heap))

;;; ---------------------------------------------------------------------------
(defun p-effective-slot-description-p
     (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of
 \\fcite{effective-slot-description},
 \\lispnil\\ otherwise."

  (and (= (p-type-tag-of p-objid p-heap) +instance-type-tag+)
       (with-transaction (p-heap)
	 (= (p-index p-heap p-objid +clos-location-class-wrapper+)
	    *effective-slot-description-objid*))))

;;; ---------------------------------------------------------------------------
(defun p-slot-description-location
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-location}
 of the persistent effective-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +slot-description-location-location+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-description-location)
  (t-location p-objid
   &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-location}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-location}
 of the persistent effective-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-location}.
\\Seealsolabel
 Section \\fcite{p-slot-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +slot-description-location-location+
		   depth p-heap)
      t-location)))

;;; ---------------------------------------------------------------------------
(defun store-effective-slot-description (t-descr p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {an effective-slot-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient effective-slot-description in
 \\funarg{t-descr}\\ to the
 persistent effective-slot-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-direct-slot-description};
 \\fcite{p-effective-slot-description}."

  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-descr))
      (unless p-objid
	(setf p-objid (p-allocate-effective-slot-description p-heap))
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-descr depth
			       +instance-type-tag+ force-write)
	(setf (p-slot-description p-objid depth p-heap) t-descr)
	(setf (p-slot-description-location p-objid depth p-heap)
	  (slot-definition-location t-descr)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-effective-slot-description)
    (t-descr &optional p-objid (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {an effective-slot-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient effective-slot-description in
 \\funarg{t-descr}\\ to the
 persistent effective-slot-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-direct-slot-description};
 \\fcite{p-effective-slot-description}."

  (values t-descr
	  (store-effective-slot-description t-descr p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-effective-slot-description 
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 effective-slot-description
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-effective-slot-description)}."

  (let* ((t-object (is-registered-objid p-objid))
	 (force-read (null t-object)))
    (unless t-object
      (let ((*transient-slot-value* t))
	(setf t-object (make-instance 'effective-slot-description
			 :p-heap p-heap
			 :store-cached-slots nil)))
      (register-to-cache p-objid t-object)
      (with-transaction (p-heap)
	(with-read-lock (p-heap p-objid depth +instance-type-tag+ force-read)
	  (p-slot-description-into t-object p-objid depth p-heap)
	  (setf (slot-description-location t-object)
	    (p-slot-description-location p-objid depth p-heap)))))
    t-object))

;;; ---------------------------------------------------------------------------
;;; Storing of CLOS slot metaobjects
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid-using-class ((t-object direct-slot-definition)
                                            (t-class standard-class)
				            depth p-heap)
  #+:lisp-doc "Stores \\funarg{t-object}\\ as an instance of
 \\fcite{direct-slot-description}."

  (let ((p-objid (is-registered-object t-object)))
    (unless p-objid
      (setf p-objid
	(store-direct-slot-description (fill-slot-description t-object nil)
				       nil depth p-heap)))
    p-objid))

;;; ---------------------------------------------------------------------------
(defmethod t-object-to-p-objid-using-class ((t-object
					     effective-slot-definition)
                                            (t-class standard-class)
				            depth p-heap)
  #+:lisp-doc "Stores \\funarg{t-object}\\ as an instance of
 \\fcite{effective-slot-description}."

  (let ((p-objid (is-registered-object t-object)))
    (unless p-objid
      (setf p-objid
	(store-effective-slot-description (fill-slot-description t-object nil)
					  nil depth p-heap)))
    p-objid))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
