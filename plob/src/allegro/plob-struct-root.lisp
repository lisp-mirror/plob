;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-struct-root.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@informatik.uni-hamburg.de
;;;; Date	1996/10/189	Created
;;;; Description	PLOB allocate and accessor functions for
;;;;		the LISP root object
;;;;
;;;; Copyright	PLOB! Copyright 1994--1998 Heiko Kirschke.
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
;;;; --------------------------------------------------------------------------

(in-package :plob)

;;; ---------------------------------------------------------------------------
(defconstant +persistent-lisproot-class+ (find-class 'persistent-lisproot)
  #+:lisp-doc "The \\clsmo\\ of structure \\fcite{persistent-lisproot}.")

;;; ---------------------------------------------------------------------------
;;; LISP root object
;;; ---------------------------------------------------------------------------

(defun p-allocate-lisproot (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 LISP root
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (let ((p-objid (p-allocate p-heap +structure-type-tag+ +root-size+)))
    (with-transaction (p-heap)
      (p-initialize-lisproot p-objid))
    p-objid))

;;; ---------------------------------------------------------------------------
(defun p-initialize-lisproot (p-objid
                              &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Initializes a LISP root allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate-lisproot}."

  (setf (p-index p-heap p-objid +structure-location-description+)
	*lisproot-description-objid*)
  (setf (p-index p-heap p-objid +root-location-version+
		 +structure-type-tag+ +null-objid+ +fixnum-type-tag+)
	*lisproot-version*)
  (setf (p-index p-heap p-objid +root-location-name->package-table+)
	(p-allocate-btree p-heap))
  (let ((table (p-allocate-btree p-heap)))
    (setf (btree-test table) 'equal)
    (setf (p-index p-heap p-objid +root-location-symbol->class-table+) table))
  (let ((table (p-allocate-btree p-heap)))
    (setf (btree-test table) 'eq)
    (setf (p-index p-heap p-objid +package-location-externals+) table))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun p-lisprootp (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of \\fcite{persistent-lisproot},
 \\lispnil\\ otherwise."

  (and (= (p-type-tag-of p-objid p-heap) +structure-type-tag+)
       (with-transaction (p-heap)
	 (= (p-index p-heap p-objid +structure-location-description+)
	    *lisproot-description-objid*))))

;;; ---------------------------------------------------------------------------
#+(and :lisp-doc (not :document-api))
(:defdoc
 "p-lisproot- ..."
 "Persistent Lisproot Accessors"
 "
\\Purposelabel
 All following functions
 {\\bf p-lisproot-\\textsl{\\lt{}slot-name\\gt}} and
 {\\bf (setf p-lisproot-\\textsl{\\lt{}slot-name\\gt}\\/)}
 with \\textsl{\\lt{}slot-name\\gt} being one of the slot names of
 \\fcite{persistent-lisproot}\\ without the `p-' prefix
 access directly a slot of a persistent structure instance
 of \\fcite{persistent-lisproot}\\ in the \\sh.

 The accessor functions defined here are merely used in the
 bootstrap phase. After the bootstrap, the persistent objects of
 \\fcite{persistent-lisproot}\\ are created and accessed as all
 persistent structure objects by employing their
 structure-descriptions, i.e.\\ all information needed to create
 a persistent structure object or to access a persistent structure
 object's slot is contained in its structure-description.
\\Seealsolabel
 \\Fcite{persistent-lisproot};
 section \\fcite{bootstrap ...}.")

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-version) (t-version
				  p-objid
				  &optional (depth *default-depth*)
				  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanumber{\\funarg{t-version}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-version}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-version}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-version depth p-heap p-objid
		     +root-location-version+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-version object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-version (p-objid
			   &optional (depth *default-depth*)
			   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-version}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-version+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-version object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-formatted) (t-formatted
			            p-objid
			            &optional (depth *default-depth*)
			            (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isakeyword{\\funarg{t-formatted}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-formatted}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-formatted}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-formatted depth p-heap p-objid
		     +root-location-lisp-formatted+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-formatted object)
			   new-value))
                     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-formatted (p-objid
		             &optional (depth *default-depth*)
		             (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-formatted}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-lisp-formatted+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-formatted object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-time) (t-time
			       p-objid
			       &optional (depth *default-depth*)
			       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanumber{\\funarg{t-time}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-time}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-time}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-time depth p-heap p-objid
		     +root-location-time-formatted+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-time object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-time (p-objid
			&optional (depth *default-depth*)
			(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-time}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-time-formatted+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-time object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-name->package-table)
     (t-name->package-table p-objid
			    &optional (depth *default-depth*)
			    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabtree{\\funarg{t-name->package-table}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-name->package-table}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-name->package-table}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-name->package-table depth p-heap p-objid
		     +root-location-name->package-table+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-name->package-table
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-name->package-table (p-objid
		                       &optional (depth *default-depth*)
		                       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-name->package-table}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-name->package-table+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-name->package-table
				object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-symbol->class-table)
     (t-symbol->class-table p-objid
			    &optional (depth *default-depth*)
			    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabtree{\\funarg{t-symbol->class-table}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-symbol->class-table}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-symbol->class-table}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-symbol->class-table depth p-heap p-objid
		     +root-location-symbol->class-table+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-symbol->class-table
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-symbol->class-table (p-objid
		                       &optional (depth *default-depth*)
		                       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-symbol->class-table}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-symbol->class-table+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-symbol->class-table
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-structure-description)
     (t-structure-description p-objid
			      &optional (depth *default-depth*)
			      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-structure-description}}
      {the structure description object}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-structure-description}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-structure-description}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-structure-description depth p-heap p-objid
		     +root-location-structure-description+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-structure-description
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-structure-description (p-objid
		                         &optional (depth *default-depth*)
		                         (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-structure-description}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-structure-description+
		     depth p-heap 
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-structure-description
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-structure-slot-description)
     (t-structure-slot-description p-objid
                                   &optional (depth *default-depth*)
			           (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-structure-slot-description}}
      {the structure slot description object}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-structure-slot-description}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-structure-slot-description}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid
   t-structure-slot-description depth p-heap p-objid
   +root-location-structure-slot-description+
   #'(lambda (new-value object)
       (setf (persistent-lisproot-p-structure-slot-description
				object) new-value))
   +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-structure-slot-description
     (p-objid &optional (depth *default-depth*)
              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-structure-slot-description}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot
   p-objid +root-location-structure-slot-description+
   depth p-heap
   #'(lambda (new-value object)
       (setf (persistent-lisproot-p-structure-slot-description
	      object) new-value))
   +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-package-description)
     (t-package-description p-objid
			    &optional (depth *default-depth*)
			    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-package-description}}
      {the package description object}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-package-description}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-package-description}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-package-description depth p-heap p-objid
		     +root-location-package-description+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-package-description
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-package-description (p-objid
		                       &optional (depth *default-depth*)
		                       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-package-description}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-package-description+
		     depth p-heap 
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-package-description
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-plob-description)
     (t-plob-description p-objid
			 &optional (depth *default-depth*)
			 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-plob-description}}
      {the plob description object}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-plob-description}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-plob-description}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-plob-description depth p-heap p-objid
		     +root-location-plob-description+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-plob-description object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-plob-description (p-objid
		                    &optional (depth *default-depth*)
		                    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-plob-description}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-plob-description+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-plob-description object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-class-description)
     (t-class-description p-objid
                          &optional (depth *default-depth*)
                          (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-class-description}}
      {the class description object}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-class-description}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-class-description}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-class-description depth p-heap p-objid
		     +root-location-class-description+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-class-description object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-class-description (p-objid
		                     &optional (depth *default-depth*)
		                     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-class-description}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-class-description+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-class-description object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-slot-description)
     (t-slot-description p-objid
			 &optional (depth *default-depth*)
			 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-slot-description}}
      {the slot description object}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-slot-description}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-slot-description}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-slot-description depth p-heap p-objid
		     +root-location-slot-description+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-slot-description object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-slot-description (p-objid
		                    &optional (depth *default-depth*)
		                    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-slot-description}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-slot-description+
		     depth p-heap 
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-slot-description object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-direct-slot-description)
     (t-direct-slot-description p-objid
                                &optional (depth *default-depth*)
				(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-direct-slot-description}}
      {the direct slot description object}
 \\isabtree{\\funarg{t-direct-slot-description}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-direct-slot-description}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-direct-slot-description}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-direct-slot-description depth p-heap p-objid
		     +root-location-direct-slot-description+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-direct-slot-description
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-direct-slot-description (p-objid
		                           &optional (depth *default-depth*)
		                           (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-direct-slot-description}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-direct-slot-description+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-direct-slot-description
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-effective-slot-description)
     (t-effective-slot-description p-objid
			           &optional (depth *default-depth*)
			           (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-effective-slot-description}}
      {the effective slot description object}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-effective-slot-description}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-effective-slot-description}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid
   t-effective-slot-description depth p-heap p-objid
   +root-location-effective-slot-description+
   #'(lambda (new-value object)
       (setf (persistent-lisproot-p-effective-slot-description
	      object) new-value))
   +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-effective-slot-description
     (p-objid &optional (depth *default-depth*)
	      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-effective-slot-description}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot
   p-objid +root-location-effective-slot-description+
   depth p-heap 
   #'(lambda (new-value object)
       (setf (persistent-lisproot-p-effective-slot-description
	      object) new-value))
   +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-method-description)
     (t-method-description p-objid
			   &optional (depth *default-depth*)
			   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-method-description}}
      {the method description object}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-method-description}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-method-description}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-method-description depth p-heap p-objid
		     +root-location-method-description+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-method-description
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-method-description (p-objid
		                      &optional (depth *default-depth*)
		                      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-method-description}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-method-description+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-method-description
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-lisproot-pfs) (t-pfs p-objid
			            &optional (depth *default-depth*)
			            (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-pfs}}
      {the persistent file system descriptor}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-pfs}
 of the persistent-lisproot referenced by
 \\funarg{p-objid}\\ to \\funarg{t-pfs}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (t-slot-to-p-objid t-pfs depth p-heap p-objid
		     +root-location-pfs+
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-pfs object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-lisproot-pfs (p-objid
		       &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-pfs}
 of the persistent-lisproot referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-lisproot- ...}."

  (p-objid-to-t-slot p-objid +root-location-pfs+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-lisproot-p-pfs object) new-value))
		     +structure-type-tag+))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
