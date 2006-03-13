;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-struct-slot-descr.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	23.2.94	Derived from plob-struct.lisp
;;;; Description	PLOB allocate and accessor functions for
;;;;		LISP structure slot descriptions
;;;;
;;;; Copyright	PLOB! Copyright 1994--2006 Heiko Kirschke.
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
;;;; $Id$
;;;;
;;;; --------------------------------------------------------------------------

(in-package :plob)

;;; ---------------------------------------------------------------------------
;;; Transient structure slot description
;;; ---------------------------------------------------------------------------

(defun make-structure-slot-description (&optional objid)
  #+:lisp-doc "
\\Purposelabel
 Returns a new allocated transient structure-slot-description.
\\Seealsolabel
 \\Fcite{structure-slot-description}."

  (let ((slot-description
	 (make-structure-slot-description-internal :objid objid)))
    (setf (structure-slot-description-p-descr slot-description)
	  *structure-slot-description*)
    slot-description))

;;; ---------------------------------------------------------------------------
(defmethod slot-description-equal-p (t-slot-descr p-slot-descr
				     &optional verbose)
  (block nil
    (unless (eq (slot-definition-name t-slot-descr)
		(slot-definition-name p-slot-descr))
      (return (values nil
		      (when verbose
			(format nil "slot name; tr.: ~A / pe.: ~A"
				(slot-definition-name t-slot-descr)
				(slot-definition-name p-slot-descr))))))
    (unless (eq (slot-definition-type t-slot-descr)
		(slot-definition-type p-slot-descr))
      (return (values nil
		      (when verbose
			(format nil "slot ~A type; tr.: ~A / pe.: ~A"
				(slot-definition-name t-slot-descr)
				(slot-definition-type t-slot-descr)
				(slot-definition-type p-slot-descr))))))
    (unless (eq (slot-description-extent t-slot-descr)
		(slot-description-extent p-slot-descr))
      (return (values nil
		      (when verbose
			(format nil "slot ~A extent; tr.: ~A / pe.: ~A"
				(slot-definition-name t-slot-descr)
				(slot-description-extent t-slot-descr)
				(slot-description-extent p-slot-descr))))))
    (unless (eq (slot-description-deferred t-slot-descr)
		(slot-description-deferred p-slot-descr))
      (return (values nil
		      (when verbose
			(format nil "slot ~A deferred; tr.: ~A / pe.: ~A"
				(slot-definition-name t-slot-descr)
				(slot-description-deferred t-slot-descr)
				(slot-description-deferred p-slot-descr))))))
    t))

;;; ---------------------------------------------------------------------------
(defmethod slot-description-equal-p
     ((t-slot-descr structure-slot-description)
      (p-slot-descr structure-slot-description)
      &optional verbose)
  (multiple-value-bind (equal-p reason)
      (call-next-method)
    (block nil
      (unless equal-p
	(return (values nil reason)))
      (unless (eq (structure-slot-description-p-reader t-slot-descr)
		  (structure-slot-description-p-reader p-slot-descr))
	(return (values nil
			(when verbose
			  (format nil "slot ~A reader; tr.: ~A / pe.: ~A"
				  (structure-slot-description-p-name
				   t-slot-descr)
				  (structure-slot-description-p-reader
				   t-slot-descr)
				  (structure-slot-description-p-reader
				   p-slot-descr))))))
      t)))

;;; ---------------------------------------------------------------------------
(defun structure-slot-description-writer (struct-slot-descr)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{struct-slot-descr}}
      {a structure-slot-description}
\\Purposelabel
 Returns a symbol bound to a function which writes a value to the
 transient structure slot described by \\funarg{struct-slot-descr}.
\\Seealsolabel
 \\Fcite{call-structure-slot-description-writer}."

  (let ((writer (structure-slot-description-t-writer struct-slot-descr)))
    (cond
     ((null writer)
      (let* ((reader (structure-slot-description-p-reader struct-slot-descr))
             (writer-symbol (intern (concatenate 'string
                                                 "PLOB-SET-SLOT "
                                                 (symbol-name reader))
                                    (symbol-package reader))))
        (unless (fboundp writer-symbol)
	  (compile-silent writer-symbol
			  `(lambda (struct new-value)
			     (setf (,reader struct) new-value))))
	(setf (structure-slot-description-t-writer struct-slot-descr)
	      writer-symbol)
	writer-symbol))
     ((eq writer :no-writer)
      nil)
     (t
      writer))))

;;; ---------------------------------------------------------------------------
(defun call-structure-slot-description-writer
     (struct-descr slot-descr p-heap t-object new-value)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{struct-descr}}
      {a structure-description}
 \\isa{\\funarg{slot-descr}}
      {a structure-slot-description}
 \\isa{\\funarg{t-object}}
      {a transient structure object}
 \\isanobject{\\funarg{new-value}}
\\Valueslabel
 \\retarg{\\funarg{new-value}}
\\Purposelabel
 Write \\funarg{new-value}\\ into the slot of the structure class
 instance \\funarg{t-object}\\ described by
 \\funarg{slot-descr}\\ of structure class \\funarg{struct-descr}.
\\Seealsolabel
 \\Fcite{structure-slot-description-writer}."

  (flet ((make-slot-transient
	     (struct-descr slot-descr p-heap)
	   (let* ((curr-struct-descr
		   (p-find-class-description
		    (structure-description-p-name struct-descr)
		    p-heap))
		  (curr-slot-descr
		   (when curr-struct-descr
		     (find-slot-description slot-descr
					    curr-struct-descr))))
	     (unless curr-struct-descr
	       (setf curr-struct-descr struct-descr))
	     (unless curr-slot-descr
	       (setf curr-slot-descr slot-descr))
	     (unless (eq (slot-description-extent curr-slot-descr)
			 :transient)
	       (let* ((new-struct-descr
		       (copy-structure-description curr-struct-descr))
		      (new-slot-descr (find-slot-description
				       slot-descr
				       new-struct-descr)))
		 (when new-slot-descr
		   (setf (slot-description-extent new-slot-descr)
		     :transient)
		   (update-class new-struct-descr p-heap)))))))

    (let ((slot-writer (structure-slot-description-writer slot-descr)))
      (when slot-writer
	(handler-case (funcall slot-writer t-object new-value)
	  (undefined-function ()
	    (setf (structure-slot-description-t-writer slot-descr)
	      :no-writer)
	    (when (and *verbose* (>= *verbose* 1))
	      (cerror "Mark that slot as :transient."
		      "Cannot write to slot ~S of structure ~S."
		      (slot-definition-name slot-descr)
		      (structure-description-p-name struct-descr)))
	    (make-slot-transient struct-descr slot-descr p-heap))))))
  new-value)

;;; ---------------------------------------------------------------------------
;;; Mapping from the generic functions slot-description-* and
;;; (setf slot-description-*) to structure accessors:
;;; ---------------------------------------------------------------------------
(defmethod (setf slot-description-name)
     (name (slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-slot-description-p-name)}; see also
 \\fcite{structure-slot-description}."

  (setf (structure-slot-description-p-name slot-description)
        name))

;;; ---------------------------------------------------------------------------
(defmethod slot-definition-name
     ((slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-slot-description-p-name}; see also
 \\fcite{structure-slot-description}."

  (structure-slot-description-p-name slot-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-description-initargs)
     (initarg (slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-slot-description-p-initargs)}; see also
 \\fcite{structure-slot-description}."

  (setf (structure-slot-description-p-initarg slot-description)
        initarg))

;;; ---------------------------------------------------------------------------
(defmethod slot-definition-initargs
     ((slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-slot-description-p-initargs}; see also
 \\fcite{structure-slot-description}."

  (structure-slot-description-p-initarg slot-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-description-location)
     (location (slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-slot-description-p-location)}; see also
 \\fcite{structure-slot-description}."

  (setf (structure-slot-description-p-location slot-description)
        location))

;;; ---------------------------------------------------------------------------
(defmethod slot-definition-location
     ((slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-slot-description-p-location}; see also
 \\fcite{structure-slot-description}."

  (structure-slot-description-p-location slot-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-description-type)
     (type (slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-slot-description-p-type)}; see also
 \\fcite{structure-slot-description}."

  (setf (structure-slot-description-p-type slot-description)
        type))

;;; ---------------------------------------------------------------------------
(defmethod slot-definition-type
     ((slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-slot-description-p-type}; see also
 \\fcite{structure-slot-description}."

  (structure-slot-description-p-type slot-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-description-extent)
     (extent (slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-slot-description-p-extent)}; see also
 \\fcite{structure-slot-description}."

  (setf (structure-slot-description-p-extent slot-description)
        extent))

;;; ---------------------------------------------------------------------------
(defmethod slot-description-extent
     ((slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-slot-description-p-extent}; see also
 \\fcite{structure-slot-description}."

  (structure-slot-description-p-extent slot-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-description-deferred)
     (deferred (slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-slot-description-p-deferred)}; see also
 \\fcite{structure-slot-description}."

  (setf (structure-slot-description-p-deferred slot-description)
        deferred))

;;; ---------------------------------------------------------------------------
(defmethod slot-description-deferred
     ((slot-description structure-slot-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-slot-description-p-deferred}; see also
 \\fcite{structure-slot-description}."

  (structure-slot-description-p-deferred slot-description))

;;; ---------------------------------------------------------------------------
(defmethod slot-definition-allocation
     ((slot-description structure-slot-description))
  #+:lisp-doc "Returns always \\lisp{:structure}\\ to mark the
 \\funarg{slot-description}\\ as the description of a structure slot for
 the caller."

  :structure)

;;; ---------------------------------------------------------------------------
(defmethod slot-description-index
     ((slot-description structure-slot-description))
  #+:lisp-doc "Since there can be no indices on structure slots this method always
 returns \\lispnil."

  nil)

;;; ---------------------------------------------------------------------------
;;; Structure slot description
;;; ---------------------------------------------------------------------------

(defun p-allocate-structure-slot-description
     (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 structure-slot-description
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{structure-slot-description};
 \\fcite{p-allocate}."

  (let ((p-objid (p-allocate p-heap
                             +structure-type-tag+
                             +structure-slot-description-size+)))
    (with-transaction (p-heap)
      (setf (p-index p-heap p-objid +structure-location-description+)
	*structure-slot-description-objid*))
    p-objid))

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description-p
     (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of class
 structure-slot-description,
 \\lispnil\\ otherwise.
\\Seealsolabel
 \\Fcite{structure-slot-description}."

  (and (= (p-type-tag-of p-objid p-heap) +structure-type-tag+)
       (with-transaction (p-heap)
	 (= (p-index p-heap p-objid +structure-location-description+)
	    *structure-slot-description-objid*))))

;;; ---------------------------------------------------------------------------
#+(and :lisp-doc (not :document-api))
(:defdoc
 "p-structure-slot-description- ..."
 "Structure Slot Description Accessors"
 "
\\Purposelabel
 All following functions
 {\\bf p-structure-slot-description-\\textsl{\\lt{}slot-name\\gt}} and
 {\\bf (setf p-structure-slot-description-\\textsl{\\lt{}slot-name\\gt}\\/)}
 with \textsl{\\lt{}slot-name\\gt} being one of the slot names of
 \\fcite{structure-slot-description}\\ without the `p-' prefix
 access directly a slot of a persistent structure instance
 of \\fcite{structure-description}\\ in the \\sh.

 The accessor functions defined here are merely used in the
 bootstrap phase. After the bootstrap, the persistent objects of
 \\fcite{structure-slot-description}\\ are created and accessed as all
 persistent structure objects by employing their
 structure-descriptions, i.e.\\ all information needed to create
 a persistent structure object or to access a persistent structure
 object's slot is contained in its structure-description.
\\Seealsolabel
 \\Fcite{structure-slot-description};
 section \\fcite{bootstrap ...}.")

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description-name
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-name}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-slot-description-location-name+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-name object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-slot-description-name)
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
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-name}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (t-slot-to-p-objid t-name depth p-heap p-objid
	            +structure-slot-description-location-name+
	            #'(lambda (new-value object)
			(setf (structure-slot-description-p-name object)
			  new-value))
		    +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description-initarg
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-initarg}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-slot-description-location-initarg+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-initarg object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-slot-description-initarg)
    (t-initarg p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-initarg}}
      {a (keyword) symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-initarg}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-initarg}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (t-slot-to-p-objid t-initarg depth p-heap p-objid
		     +structure-slot-description-location-initarg+
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-initarg object)
			   new-value))
	             +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description-reader
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-reader}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-slot-description-location-reader+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-reader object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-slot-description-reader)
    (t-reader p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-reader}}
      {a symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-reader}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-reader}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (t-slot-to-p-objid t-reader depth p-heap p-objid
		     +structure-slot-description-location-reader+
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-reader object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description-location
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-location}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-slot-description-location-location+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-location object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-slot-description-location)
    (t-location p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-location}}
      {a fixnum}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-location}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-location}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (t-slot-to-p-objid t-location depth p-heap p-objid
		     +structure-slot-description-location-location+
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-location object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description-init
     (p-objid &optional (depth :value) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-init}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-slot-description-location-init+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-init object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-slot-description-init)
    (t-init p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-init}}
      {an expression}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-init}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-init}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (t-slot-to-p-objid t-init depth p-heap p-objid
		     +structure-slot-description-location-init+
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-init object)
			   new-value))
	             +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description-type
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-type}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-slot-description-location-type+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-type object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-slot-description-type)
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
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-type}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (t-slot-to-p-objid t-type depth p-heap p-objid
		     +structure-slot-description-location-type+
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-type object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description-extent
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-extent}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-slot-description-location-extent+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-extent object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-slot-description-extent)
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
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-extent}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (t-slot-to-p-objid t-extent depth p-heap p-objid
		     +structure-slot-description-location-extent+
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-extent object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description-deferred
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-deferred}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-slot-description-location-deferred+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-deferred object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-slot-description-deferred)
    (t-deferred p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-deferred}}
      {either \\lispnil\\ or a fixnum}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-deferred}
 of the persistent structure-slot-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-deferred}.
\\Seealsolabel
 Section \\fcite{p-structure-slot-description- ...}."

  (t-slot-to-p-objid t-deferred depth p-heap p-objid
		     +structure-slot-description-location-deferred+
		     #'(lambda (new-value object)
			 (setf (structure-slot-description-p-deferred object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description-into
    (t-into-descr p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-into-descr}}
      {a structure-slot-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Read the persistent structure-slot-description referenced by
 \\funarg{p-objid}\\ destructively into \\funarg{t-into-descr}.
\\Seealsolabel
 \\Fcite{p-structure-slot-description}."

  (setf (structure-slot-description-objid t-into-descr) p-objid)
  (with-transaction (p-heap)
    (with-read-lock (p-heap p-objid depth +structure-type-tag+ nil)
      (setf (slot-description-name t-into-descr)
	(p-structure-slot-description-name p-objid depth p-heap))
      (setf (slot-description-initargs t-into-descr)
	(p-structure-slot-description-initarg p-objid depth p-heap))
      (setf (structure-slot-description-p-reader t-into-descr)
	(p-structure-slot-description-reader p-objid depth p-heap))
      (setf (slot-description-location t-into-descr)
	(p-structure-slot-description-location p-objid depth p-heap))
      (setf (structure-slot-description-p-init t-into-descr)
	(p-structure-slot-description-init p-objid depth p-heap))
      (setf (slot-description-type t-into-descr)
	(p-structure-slot-description-type p-objid depth p-heap))
      (setf (slot-description-extent t-into-descr)
	(p-structure-slot-description-extent p-objid depth p-heap))
      (setf (slot-description-deferred t-into-descr)
	(p-structure-slot-description-deferred p-objid depth p-heap))))
  t-into-descr)

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-description
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 structure-slot-description
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-structure-slot-description)}."

  (let ((t-object (is-registered-objid p-objid)))
    (unless t-object
      (setf t-object (make-structure-slot-description))
      (register-to-cache p-objid t-object)
      (p-structure-slot-description-into t-object p-objid depth p-heap))
    t-object))

;;; ---------------------------------------------------------------------------
(defun store-structure-slot-description (t-descr p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a structure-slot-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient structure-slot-description in
 \\funarg{t-descr}\\ to the
 persistent structure-slot-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-structure-slot-description}."

  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-descr))
      (unless p-objid
	(setf p-objid (p-allocate-structure-slot-description p-heap))
	(setf (structure-slot-description-objid t-descr) p-objid)
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-descr depth
			       +structure-type-tag+ force-write)
	(setf (p-structure-slot-description-name p-objid depth p-heap)
	  (slot-definition-name t-descr))
	(setf (p-structure-slot-description-initarg p-objid depth p-heap)
	  (slot-definition-initargs t-descr))
	(setf (p-structure-slot-description-reader p-objid depth p-heap)
	  (structure-slot-description-p-reader t-descr))
	(setf (p-structure-slot-description-location p-objid depth p-heap)
	  (slot-definition-location t-descr))
	(setf (p-structure-slot-description-init p-objid depth p-heap)
	  (structure-slot-description-p-init t-descr))
	(setf (p-structure-slot-description-type p-objid depth p-heap)
	  (slot-definition-type t-descr))
	(setf (p-structure-slot-description-extent p-objid depth p-heap)
	  (slot-description-extent t-descr))
	(setf (p-structure-slot-description-deferred p-objid depth p-heap)
	  (slot-description-deferred t-descr)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-slot-description)
    (t-descr &optional p-objid (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a structure-slot-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient structure-slot-description in
 \\funarg{t-descr}\\ to the
 persistent structure-slot-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-structure-slot-description}."

  (values t-descr
	  (store-structure-slot-description t-descr p-objid depth p-heap)))
  
;;; ---------------------------------------------------------------------------
;;; Storing of CLOS slot metaobjects for Allegro
;;; In Allegro, structure effective slot definitions are CLOS objects
;;; inheriting from effective-slot-definition; this is quite different
;;; to LispWorks, where the structure's MOP is kept seperate from the
;;; CLOS' MOP.
;;; ---------------------------------------------------------------------------
#+:allegro
(defmethod t-object-to-p-objid-using-class
    ((t-object
      #-(version>= 6)
      clos::structure-effective-slot-definition
      #+(version>= 6)
      excl::structure-effective-slot-definition)
     (t-class standard-class) depth p-heap)
  #+:lisp-doc "Stores \\funarg{t-object}\\ as an instance of
 \\fcite{structure-slot-description}."

  (let ((p-objid (is-registered-object t-object)))
    (unless p-objid
      (setf p-objid
	(store-structure-slot-description (fill-slot-description t-object nil)
					  nil depth p-heap)))
    p-objid))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
