;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-struct-descr.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	23.2.94	Derived from plob-struct.lisp
;;;; Description	PLOB allocate and accessor functions for
;;;;		LISP structure descriptions
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
(defconstant +persistent-structure-class+ (find-class 'persistent-structure)
  #+:lisp-doc "The \\clsmo\\ of \\fcite{persistent-structure}.")

;;; ---------------------------------------------------------------------------
(defconstant +structure-persistent-slot-names+
    (loop for slot in (class-slots +persistent-structure-class+)
	if (symbolp slot)
	collect slot
	else
	collect (slot-definition-name slot))
	   
  #|
  (remove-if #'(lambda (s)
                 (eq (slot-extent s +persistent-structure-class+)
                     :transient))
             (class-slots +persistent-structure-class+))
  |#
  #+:lisp-doc "
\\Purposelabel
 A list with all non-transient,
 i.e.\\ persistent slots of \\fcite{persistent-structure}.
\\Seealsolabel
 \\Fcite{+reverse-structure-persistent-slot-names+}.")

;;; ---------------------------------------------------------------------------
(defconstant +reverse-structure-persistent-slot-names+
  (reverse +structure-persistent-slot-names+)
  #+:lisp-doc "The reverse list of the value of
 \\fcite{+structure-persistent-slot-names+}.")

;;; ---------------------------------------------------------------------------
(defconstant +length-structure-persistent-slot-names+
  (length +structure-persistent-slot-names+)
  #+:lisp-doc "The length of the list in
 \\fcite{+structure-persistent-slot-names+}.")

;;; ---------------------------------------------------------------------------
(defun persistent-structure-slot-p (slot)
  #+:lisp-doc "Check if \\funarg{slot}\\ is a member of the slots found in
 \\fcite{+structure-persistent-slot-names+}."
  (has-slot-p slot +persistent-structure-class+))

;;; ---------------------------------------------------------------------------
(defgeneric ensure-allocated-object
     (t-object &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{t-object}}
      {a persistent object}
\\Valueslabel
 Returns the numeric \\objid\\ of \\funarg{t-object}.
\\Purposelabel
 Check if \\funarg{t-object}\\ is allocated on the \\sh;
 if not, do so."))

;;; ---------------------------------------------------------------------------
(defgeneric fill-description
     (the-class &optional t-descr (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isa{\\funarg{t-descr}}
      {a class description}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Stores a description of \\funarg{the-class}\\ to \\funarg{t-descr};
 if \\funarg{t-descr}\\ is missing, the method returns a new created
 transient description object.
\\Seealsolabel
 \\Fcite{fill-slot-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric class-description-equal-p (t-class-descr p-class-descr
				       &optional verbose)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{t-class-descr}\\ resp.\\ \\funarg{p-class-descr}}
      {a class description}
\\Purposelabel
 Returns \\nonnil\\ iff the (transient) class description
 \\funarg{t-class-descr}\\ and the (persistent) class description
 \\funarg{p-class-descr}\\ are \\lisp{equal}, \\lispnil\\ otherwise."))

;;; ---------------------------------------------------------------------------
(defgeneric generate-description
     (class-descr &optional add-extra-slot-options add-extra-class-options)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{class-descr}}
      {a class description}
 \\isabool{\\funarg{add-extra-slot-options}\\ resp.\\ %
           \\funarg{add-extra-class-options}}
\\Purposelabel
 Returns a human-readable \\cl\\ adequate definition of
 \\funarg{class-descr}."))

;;; ---------------------------------------------------------------------------
(defgeneric compile-description (class-descr)
  #+:lisp-doc (:documentation "
\\Purposelabel
 Compile the \\funarg{class-descr}\\ into the current transient
 \\cl\\ environment."))

;;; ---------------------------------------------------------------------------
(defgeneric slot-description-equal-p (t-slot-descr p-slot-descr
				      &optional verbose)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{t-slot-descr}\\ resp.\\ \\funarg{p-slot-descr}}
      {a slot description}
\\Purposelabel
 Check if the (transient) slot description \\funarg{t-slot-descr}\\ and
 the (persistent) slot description \\funarg{p-slot-descr}\\ are
 \\lisp{equal}.
\\Seealsolabel
 \\Fcite{structure-slot-description};
 \\fcite{slot-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric update-class
     (class-descr &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{class-descr}}
      {a class description}
\\Purposelabel
 Updates a class description in the \\sh.
\\Seealsolabel
 \\Fcite{structure-description};
 \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defun p-find-name (name &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{name}}
      {either a string or a symbol}
\\Purposelabel
 Returns \\funarg{name}\\ mapped to a string.
\\Seealsolabel
 \\Fcite{p-find-package-name}."

  ;; (assert-open-session-p p-heap)
  (cond
   ((stringp name)
    (external-to-internal-name name))
   ((symbolp name)
    (symbol-name name))
   (t
    (when (or (integerp name)
	      (subtypep (type-of name) 'persistent-object))
      (cond
       ((p-stringp name p-heap)
	(external-to-internal-name (p-string name :cached p-heap)))
       ((p-symbolp name p-heap)
        (p-symbol-name name :cached p-heap)))))))

;;; ---------------------------------------------------------------------------
(defun p-find-class (name
		     &optional (depth :flat)
		     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{name}}
      {a string or a symbol}
\\Valueslabel
 Returns a class description or \\lispnil.
\\Purposelabel
 Search for a persistent class named \\funarg{name}; iff such a
 class is found, return its class description; otherwise, return
 \\lispnil.
\\Remarkslabel
 There's a problem with the different packages assigned to symbols
 across more than one LISP system in conjunction with the names
 of persistent classes and the missing \\lisp{use-package}\\ support
 for persistent packages in \\plob. For example, in \\lw\\ the symbol
 \\lisp{standard-class}\\ belongs to the \\lisp{clos}\\ package; so,
 a \\plob\\ database formatted by a \\lw\\ system will put the name
 of the class \\class{standard-class}\\ into the persistent
 \\lisp{CLOS}\\ package. On the contrary, in \\allegro\\ the symbol
 \\lisp{standard-class}\\ belongs to the \\lisp{common-lisp}\\ package;
 consequently, an \\allegro\\ system won't find the symbol
 \\lisp{standard-class}\\ in a database formatted by \\lw.

 The current fixup is to load all class name symbols from the
 persistent heap when an unknown class name is passed to
 \\fcite{p-find-class}. This will put all class' names into
 the cache, where in turn they will be found by \\fcite{p-find-symbol}.
\\Seealsolabel
 \\Fcite{class-description};
 \\fcite{structure-description};
 \\fcite{p-apropos-classes};
 \\fcite{find-class}."

  (assert-open-session-p p-heap)
  (let ((*default-depth* depth)
	(p-symbol-objid (is-registered-object name)))
    (unless p-symbol-objid
      ;; Force reading in all class name symbols
      ;; to resolve ambiguities:
      (mapbtree #'(lambda (key data)
		    (declare (ignore key data))
		    t)
		*symbol->class-table* :key-depth :cached :data-depth :objid)
      (setf p-symbol-objid (p-find-symbol name :depth :objid :p-heap p-heap)))
    (if p-symbol-objid
	(getbtree-by-objid p-symbol-objid *symbol->class-table* depth p-heap)
      (values nil nil))))

;;; ---------------------------------------------------------------------------
(defun p-find-class-cache-entry (name p-heap)
  #+:lisp-doc "Find the cached-btree entry for the class named \\funarg{name}."
  (declare (inline p-find-class-cache-entry))
  (let ((p-symbol-objid (is-registered-object name)))
    (unless p-symbol-objid
      (setf p-symbol-objid
            (p-find-symbol name :depth :objid :p-heap p-heap)))
    (if p-symbol-objid
        (gethash p-symbol-objid (cached-btree-key->data-cache
				 *symbol->class-table*)))))

;;; ---------------------------------------------------------------------------
(defun p-find-class-objid (name p-heap)
  #+:lisp-doc "Find the \\objid\\ of a class description."
  (declare (inline p-find-class-objid))
  (let* ((cache-entry (p-find-class-cache-entry name p-heap))
         (p-objid (when cache-entry
                    (btree-cache-entry-objid cache-entry)))
	 (p-type-tag (when cache-entry
		       (btree-cache-entry-type-tag cache-entry))))
    (if cache-entry
        (values p-objid p-type-tag)
      (multiple-value-bind (object foundp p-objid p-type-tag)
	  (p-find-class name :objid p-heap)
        (values p-objid p-type-tag)))))

;;; ---------------------------------------------------------------------------
(defun p-find-class-description (name p-heap)
  #+:lisp-doc "Find the class description of the class named \\funarg{name}."
  (let* ((cache-entry (p-find-class-cache-entry name p-heap))
         (p-object (when cache-entry
                     (btree-cache-entry-data cache-entry))))
    (if p-object
        p-object
      (p-find-class name :cached p-heap))))

;;; ---------------------------------------------------------------------------
(defun p-delete-class (name
		       &optional
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{name}}
      {a string or a symbol}
\\Valueslabel
 Returns \\nonnil\\ iff the class was found and removed,
 \\lispnil\\ otherwise.
\\Purposelabel
 Remove the persistent class named \\funarg{name}\\ from
 the \\sh.
\\Remarkslabel
 If there are still instances of the deleted class reachable
 in the \\sh, the persistent class will be put back into the
 class table contained in
 \\fcite{*symbol->class-table*}\\ next time when such an
 instance is loaded."

  (assert-open-session-p p-heap)
  (let ((the-class (find-class name nil))
        (found-class (p-find-class-objid name p-heap)))
    (when the-class
      (setf (class-description-of the-class) nil))
    (when found-class
      (rembtree-by-objid (p-find-symbol name :depth :objid :p-heap p-heap)
			 +short-objid-tag+ *symbol->class-table* p-heap)
      (unregister-by-objid found-class)
      t)))

;;; ---------------------------------------------------------------------------
(defun p-structurep (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of type
 persistent structure,
 \\lispnil\\ otherwise. No further class checking is done by this
 function; it is only checked if the persistent object referenced
 by \\funarg{p-objid}\\ is a persistent structure object at all."

  (assert-open-session-p p-heap)
  (= (p-type-tag-of p-objid p-heap) +structure-type-tag+))

;;; ---------------------------------------------------------------------------
;;; Transient structure description
;;; ---------------------------------------------------------------------------

(defun make-structure-description (&optional objid)
  #+:lisp-doc "
\\Purposelabel
 Returns a new allocated transient structure-description.
\\Seealsolabel
 \\Fcite{structure-description}."

  (let ((structure-description
	 (make-structure-description-internal :objid objid)))
    (setf (structure-description-p-descr structure-description)
	  *structure-description*)
    structure-description))

;;; ---------------------------------------------------------------------------
(defun get-structure-constructor (class-of-struct)
  #+:lisp-doc "
\\Purposelabel
 Call to \\fcite{class-constructor}\\ with some
 additional user-interaction if required.
\\Seealsolabel
 \\Fcite{class-constructor}."

  (let ((constructor (class-constructor class-of-struct)))
    (unless (or (and constructor
                     (fboundp constructor))
                (eq (class-extent class-of-struct) :transient))
      ;; No constructor found; ask if to mark the structure as transient:
      (when (and *verbose* (>= *verbose* 1))
	(cerror "Mark this structure as transient."
	      "Cannot locate constructor function for structure ~S."
	      class-of-struct))
      (setf (class-extent class-of-struct) :transient))
    constructor))

;;; ---------------------------------------------------------------------------
(defun get-structure-slot-reader (class-of-struct slot-name)
  #+:lisp-doc "
\\Purposelabel
 Call to \\fcite{structure-slot-reader}\\ with some
 additional user-interaction if required.
\\Seealsolabel
 \\Fcite{structure-slot-reader}."

  (let ((slot-symbolic-name (if (symbolp slot-name)
				slot-name
			      (slot-definition-name slot-name)))
	(accessor (structure-slot-reader class-of-struct slot-name)))
    (unless (or (and accessor (fboundp accessor))
		(persistent-structure-slot-p slot-symbolic-name))
      (if (and *verbose* (>= *verbose* 1))
	  (loop
	    (restart-case
		(error "Cannot locate accessor for structure ~S, slot ~S."
		       class-of-struct slot-symbolic-name)
	      (continue
		  ()
		  :report
		    "Mark the whole structure as transient."
		(setf (class-extent class-of-struct) :transient)
		(return))
	      (mark-slot-as-transient
		  ()
		  :report
		    "Mark the single slot as transient."
		(setf (slot-extent slot-symbolic-name class-of-struct)
		  :transient)
		(return))))
	(setf (class-extent class-of-struct) :transient)))
    accessor))

;;; ---------------------------------------------------------------------------
(defun get-structure-slot-default-init (class-of-struct slot-name)
  #+:lisp-doc "Calls to this function are trapped to
 \\fcite{structure-slot-default-init}."
  (structure-slot-default-init class-of-struct slot-name))

;;; ---------------------------------------------------------------------------
(defun get-structure-slot-type (class-of-struct slot-name)
  #+:lisp-doc "Calls to this function are trapped to
 \\fcite{structure-slot-type}."
  (let ((slot-type (structure-slot-type class-of-struct
                                        slot-name)))
    (if slot-type
        slot-type
      t)))

;;; ---------------------------------------------------------------------------
(defconstant +structure-slot-extents+
  '(:transient :cached :cached-demand-load)
  #+:lisp-doc "
\\Purposelabel
 List with allowed structure slot extents.
\\Seealsolabel
 \\Fcite{(setf slot-extent)};
 \\fcite{+clos-slot-extents+};
 \\fcite{+plob-slot-extents+}.")

;;; ---------------------------------------------------------------------------
(defconstant +illegal-extent-error-prompt+
  "~S is an illegal extent for slot ~S of ~A; allowed are ~S."
  #+:lisp-doc "Prompt which is shown in the error message
 concerning invalid slot extents.")

;;; ---------------------------------------------------------------------------
(defconstant +illegal-deferred-error-prompt+
  "~S is an illegal deferred option for slot ~S of ~A; allowed is only NIL or a number."
  #+:lisp-doc "Prompt which is shown in the error message
 concerning invalid \\lisp{deferred}\\ slot options.")

;;; ---------------------------------------------------------------------------
(defmethod fill-description
     ((the-class structure-class)
      &optional t-descr (p-heap *default-persistent-heap*))
  (declare (ignore p-heap))
  (unless t-descr
    (setf t-descr (make-structure-description)))
  (let* ((list-of-slots
	  ;; Sort slots by location and name:
	  (sort (copy-seq (class-slots the-class))
		#'(lambda (slot-1 slot-2)
		    (let ((location-1 (slot-location slot-1 the-class))
			  (location-2 (slot-location slot-2 the-class)))
		      (cond
		       ((and location-1 location-2
			     (< location-1 location-2))
			t)
		       ((and location-1 location-2
			     (> location-1 location-2))
			nil)
		       (location-1
			nil)
		       (location-2
			t)
		       (t
			(string< (symbol-name
				  (if (symbolp slot-1)
				      slot-1
				    (slot-definition-name slot-1)))
				 (symbol-name
				  (if (symbolp slot-2)
				      slot-2
				  (slot-definition-name slot-2))))))))))
         (total-number-of-slots 0))
    ;; Mark all still unmarked slots:
    (let ((class-extent (class-extent the-class)))
      (unless class-extent
        (setf class-extent *default-structure-slot-extent*))
      (dolist (s list-of-slots)
        (unless (slot-extent s the-class)
	  (setf (slot-extent s the-class) class-extent))))
    ;; Add the inherited slots of structure plob::persistent-structure
    (dolist (inherited-persistent-slot
		+reverse-structure-persistent-slot-names+)
      (pushnew inherited-persistent-slot list-of-slots
	       :key #'(lambda (slot)
			(if (symbolp slot)
			    slot
			  (slot-definition-name slot)))))
    (setf total-number-of-slots (length list-of-slots))
    (setf (class-description-name t-descr) (class-name the-class))
    (let ((evolution (schema-evolution the-class)))
      (unless evolution
        (setf evolution *default-structure-schema-evolution*)
        (setf (schema-evolution the-class) evolution))
      (setf (class-description-schema-evolution t-descr) evolution))
    (setf (class-description-next-generation t-descr) nil)
    (setf (class-description-slot-numbers t-descr) total-number-of-slots)
    (unless (eq (class-extent the-class) :transient)
      (let ((slot-descr-vector (make-array total-number-of-slots))
	    (persistent-slots 0) (next-location 0))
	(loop for i from 0 below total-number-of-slots
	    do
	      (setf (svref slot-descr-vector i)
		(make-structure-slot-description)))
	(loop for slot in list-of-slots
	    for i from 0
	    as slot-extent =
	      (slot-extent slot
			   (if (persistent-structure-slot-p slot)
			       +persistent-structure-class+
			     the-class))
	    as slot-descr = (svref slot-descr-vector i)
	    do
	      (unless (member slot-extent +structure-slot-extents+)
		(error +illegal-extent-error-prompt+
		       slot-extent 
		       (if (symbolp slot) slot (slot-definition-name slot))
		       the-class
		       +structure-slot-extents+))
	      (setf (slot-description-extent slot-descr) slot-extent)
	      (unless (eq slot-extent :transient)
		(let ((location (slot-location slot the-class)))
		  (when (and (null location)
			     (persistent-structure-slot-p slot))
		    (setf location (slot-location
				    slot
				    +persistent-structure-class+)))
		  (if (and location *in-bootstrap-p*)
		      (progn
			(setf (slot-description-location slot-descr) location)
			(setf next-location (max next-location
						 (1+ location))))
		    (incf persistent-slots)))))
	(incf persistent-slots next-location)
	(when (> persistent-slots 0)
	  (loop for slot in list-of-slots
	      for i from 0
	      as slot-symbolic-name = (if (symbolp slot)
					  slot
					(slot-definition-name slot))
	      as slot-deferred = (slot-deferred slot the-class)
	      as slot-descr = (svref slot-descr-vector i)
	      do
		(unless (or (not slot-deferred) (numberp slot-deferred))
		  (error +illegal-deferred-error-prompt+
			 slot-deferred slot-symbolic-name the-class))
		(setf (slot-description-name slot-descr) slot-symbolic-name)
		(setf (slot-description-deferred slot-descr) slot-deferred)
		(let ((initarg (slot-initarg slot the-class)))
		  (setf (slot-description-initargs slot-descr)
		    (if initarg
                        initarg
                      (intern (symbol-name slot-symbolic-name) :keyword))))
		(setf (structure-slot-description-p-init slot-descr)
		  (get-structure-slot-default-init the-class slot))
		(setf (slot-description-type slot-descr)
		  (get-structure-slot-type the-class slot))
		(setf (structure-slot-description-p-reader slot-descr)
		  (get-structure-slot-reader the-class slot))
		(unless (eq (slot-description-extent slot-descr) :transient)
		  (let ((location (slot-definition-location slot-descr)))
		    (unless (and *in-bootstrap-p* location)
		      (setf (slot-description-location slot-descr)
			next-location)
		      (incf next-location))))))
	(setf (class-description-persistent-slot-numbers t-descr)
	  persistent-slots)
	;; Store the slot descriptions only if there is at least one
	;; persistent slot:
	(setf (class-description-effective-slots t-descr)
	  (if (> persistent-slots 0)
	      slot-descr-vector
	    nil)))))
  (setf (structure-description-p-constructor t-descr)
	(get-structure-constructor the-class))
  (setf (structure-description-p-dependent t-descr)
	(class-dependent the-class))
  t-descr)

;;; ---------------------------------------------------------------------------
(defun copy-structure-description (original)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{original}}
      {a structure-description}
\\Purposelabel
 Returns a copy of \\funarg{original}.
\\Seealsolabel
 \\Fcite{structure-description}."

  (let ((copy (copy-structure-description-internal original)))
    (setf (structure-description-objid copy) nil)
    (setf (class-description-time-stamp copy)
	  (round (get-universal-time) 60))
    (let* ((original-slots (class-slots original))
           (copy-slots (copy-seq original-slots))
           (i 0))
      (setf (class-description-effective-slots copy) copy-slots)
      (map nil
           #'(lambda (s)
               (let ((copy-slot-descr (copy-structure-slot-description s)))
		 (setf (svref copy-slots i) copy-slot-descr)
		 (setf (structure-slot-description-objid copy-slot-descr)
                       nil)
                 (incf i)))
           original-slots))
    copy))

;;; ---------------------------------------------------------------------------
;;; Mapping from the generic functions class-description-* and
;;; (setf class-description-*) to structure accessors:
;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-name)
     (name (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-p-name)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-p-name class-description) name))

;;; ---------------------------------------------------------------------------
(defmethod class-name
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-p-name}; see also
 \\fcite{structure-description}."

  (structure-description-p-name class-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-next-generation)
     (next-generation (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-p-next-generation)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-p-next-generation class-description)
        next-generation))

;;; ---------------------------------------------------------------------------
(defmethod class-description-next-generation
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-p-next-generation}; see also
 \\fcite{structure-description}."

  (structure-description-p-next-generation class-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-persistent-slot-numbers)
     (persistent-slot-numbers (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-p-persistent-slot-numbers)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-p-persistent-slot-numbers class-description)
        persistent-slot-numbers))

;;; ---------------------------------------------------------------------------
(defmethod class-description-persistent-slot-numbers
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-p-persistent-slot-numbers}; see also
 \\fcite{structure-description}."

  (structure-description-p-persistent-slot-numbers class-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-schema-evolution)
     (schema-evolution (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-p-schema-evolution)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-p-schema-evolution class-description)
        schema-evolution))

;;; ---------------------------------------------------------------------------
(defmethod class-description-schema-evolution
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-p-schema-evolution}; see also
 \\fcite{structure-description}."

  (structure-description-p-schema-evolution class-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-constructor)
     (constructor (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-p-constructor)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-p-constructor class-description)
        constructor))

;;; ---------------------------------------------------------------------------
(defmethod class-constructor
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-p-constructor}; see also
 \\fcite{structure-description}."

  (structure-description-p-constructor class-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-dependent)
     (dependent (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-p-dependent)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-p-dependent class-description)
        dependent))

;;; ---------------------------------------------------------------------------
(defmethod class-dependent
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-p-dependent}; see also
 \\fcite{structure-description}."

  (structure-description-p-dependent class-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-slot-numbers)
     (slot-numbers (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-p-slot-numbers)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-p-slot-numbers class-description)
        slot-numbers))

;;; ---------------------------------------------------------------------------
(defmethod class-description-slot-numbers
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-p-slot-numbers}; see also
 \\fcite{structure-description}."

  (structure-description-p-slot-numbers class-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-effective-slots)
     (slots (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-p-slots)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-p-slots class-description) slots))

;;; ---------------------------------------------------------------------------
(defmethod class-slots
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-p-slots}; see also
 \\fcite{structure-description}."

  (structure-description-p-slots class-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-time-stamp)
     (time-stamp (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-p-time-stamp)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-p-time-stamp class-description)
        time-stamp))

;;; ---------------------------------------------------------------------------
(defmethod class-description-time-stamp
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-p-time-stamp}; see also
 \\fcite{structure-description}."

  (structure-description-p-time-stamp class-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-version-number)
     (version-number (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-p-version-number)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-p-version-number class-description)
        version-number))

;;; ---------------------------------------------------------------------------
(defmethod class-description-version-number
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-p-version-number}; see also
 \\fcite{structure-description}."

  (structure-description-p-version-number class-description))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-name->slot-cache)
     (cache (class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf structure-description-t-name->slot-cache)}; see also
 \\fcite{structure-description}."

  (setf (structure-description-t-name->slot-cache class-description)
        cache))

;;; ---------------------------------------------------------------------------
(defmethod class-description-name->slot-cache
     ((class-description structure-description))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf structure-description-t-name->slot-cache}; see also
 \\fcite{structure-description}."

  (structure-description-t-name->slot-cache class-description))

;;; ---------------------------------------------------------------------------
;;; Persistent structure description
;;; ---------------------------------------------------------------------------

(defun p-allocate-structure-description
     (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 structure-description
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{structure-description};
 \\fcite{p-allocate}."

  (let ((p-objid (p-allocate p-heap
                             +structure-type-tag+
                             +structure-description-size+)))
    (with-transaction (p-heap)
      (setf (p-index p-heap p-objid +structure-location-description+)
	*structure-description-objid*))
    p-objid))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-p
     (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of class
 structure-description,
 \\lispnil\\ otherwise.
\\Seealsolabel
 \\Fcite{structure-description}."

  (assert-open-session-p p-heap)
  (and (= (p-type-tag-of p-objid p-heap) +structure-type-tag+)
       (with-transaction (p-heap)
	 (= (p-index p-heap p-objid +structure-location-description+)
	    *structure-description-objid*))))

;;; ---------------------------------------------------------------------------
#+(and :lisp-doc (not :document-api))
(:defdoc
 "p-structure-description- ..."
 "Structure Description Accessors"
 "
\\Purposelabel
 All following functions
 {\\bf p-structure-description-\\textsl{\\lt{}slot-name\\gt}} and
 {\\bf (setf p-structure-description-\\textsl{\\lt{}slot-name\\gt}\\/)}
 with \\textsl{\\lt{}slot-name\\gt} being one of the slot names of
 \\fcite{structure-description}\\ without the `p-' prefix
 access directly a slot of a persistent structure instance
 of \\fcite{structure-description}\\ in the \\sh.

 The accessor functions defined here are merely used in the
 bootstrap phase. After the bootstrap, the persistent objects of
 \\fcite{structure-description}\\ are created and accessed as all
 persistent structure objects by employing their
 structure-descriptions, i.e.\\ all information needed to create
 a persistent structure object or to access a persistent structure
 object's slot is contained in its structure-description.
\\Seealsolabel
 \\Fcite{structure-description};
 section \\fcite{bootstrap ...}.")

;;; ---------------------------------------------------------------------------
(defun p-structure-description-name
    (p-objid &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-name}
 of the persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (p-objid-to-t-slot p-objid +structure-description-location-name+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-description-p-name object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description-name)
     (t-name p-objid &optional (depth *default-depth*)
	     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-name}}
      {a symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-name}
 of the persistent structure-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-name}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (t-slot-to-p-objid t-name depth p-heap p-objid
		     +structure-description-location-name+
		     #'(lambda (new-value object)
			 (setf (structure-description-p-name object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-version-number
    (p-objid &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-version-number}
 of the persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-description-location-version-number+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-description-p-version-number object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description-version-number)
     (t-version-number p-objid &optional (depth *default-depth*)
                       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-version-number}}
      {a fixnum}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-version-number}
 of the persistent structure-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-version-number}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (t-slot-to-p-objid t-version-number depth p-heap p-objid
		     +structure-description-location-version-number+
		     #'(lambda (new-value object)
			 (setf (structure-description-p-version-number object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-time-stamp
    (p-objid &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-time-stamp}
 of the persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (p-objid-to-t-slot p-objid +structure-description-location-time-stamp+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-description-p-time-stamp object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description-time-stamp)
     (t-time-stamp p-objid &optional (depth *default-depth*)
		   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-time-stamp}}
      {a time stamp in \\cl\\ Universal Time divided by 60,
       i.e.\\ the time is in minutes, not in seconds.}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-time-stamp}
 of the persistent structure-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-time-stamp}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (t-slot-to-p-objid t-time-stamp depth p-heap p-objid
		     +structure-description-location-time-stamp+
		     #'(lambda (new-value object)
			 (setf (structure-description-p-time-stamp object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-schema-evolution
    (p-objid &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-schema-evolution}
 of the persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-description-location-schema-evolution+
		     depth p-heap 
		     #'(lambda (new-value object)
			 (setf (structure-description-p-schema-evolution
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description-schema-evolution)
     (t-schema-evolution p-objid &optional (depth *default-depth*)
                         (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-schema-evolution}}
      {a keyword symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-schema-evolution}
 of the persistent structure-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-schema-evolution}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (t-slot-to-p-objid t-schema-evolution depth p-heap p-objid
		     +structure-description-location-schema-evolution+
		     #'(lambda (new-value object)
			 (setf (structure-description-p-schema-evolution
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-next-generation
     (p-objid &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-next-generation}
 of the persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-description-location-next-generation+
		     depth p-heap 
		     #'(lambda (new-value object)
			 (setf (structure-description-p-next-generation object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description-next-generation)
     (t-next-generation p-objid &optional (depth *default-depth*)
                        (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-next-generation}}
      {either \\lispnil\\ or a structure-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-next-generation}
 of the persistent structure-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-next-generation}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (t-slot-to-p-objid t-next-generation depth p-heap p-objid
		     +structure-description-location-next-generation+
		     #'(lambda (new-value object)
			 (setf (structure-description-p-next-generation object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-constructor
    (p-objid &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-constructor}
 of the persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-description-location-constructor+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-description-p-constructor object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description-constructor)
     (t-constructor p-objid &optional (depth *default-depth*)
				      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-constructor}}
      {a symbol bound to a function which generates an instance of
       the class described by the persistent structure description
       referenced by \\funarg{p-objid}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-constructor}
 of the persistent structure-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-constructor}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (t-slot-to-p-objid t-constructor depth p-heap p-objid
		     +structure-description-location-constructor+
		     #'(lambda (new-value object)
			 (setf (structure-description-p-constructor object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-dependent
    (p-objid &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-dependent}
 of the persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-description-location-dependent+
		     depth p-heap 
		     #'(lambda (new-value object)
			 (setf (structure-description-p-dependent object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description-dependent)
     (t-dependent p-objid
      &optional (depth *default-depth*)
		(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-dependent}}
      {a symbol bound to a function which generates an instance of
       the class described by the persistent structure description
       referenced by \\funarg{p-objid}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-dependent}
 of the persistent structure-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-dependent}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (t-slot-to-p-objid t-dependent depth p-heap p-objid
		     +structure-description-location-dependent+
		     #'(lambda (new-value object)
			 (setf (structure-description-p-dependent object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-persistent-slot-numbers
     (p-objid &optional (depth *default-depth*)
              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-persistent-slot-numbers}
 of the persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-description-location-persistent-slot-numbers+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-description-p-persistent-slot-numbers
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description-persistent-slot-numbers)
     (n-slots p-objid &optional (depth *default-depth*)
              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{n-slots}}
      {a fixnum}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-persistent-slot-numbers}
 of the persistent structure-description referenced by
 \\funarg{p-objid}\\ to \\funarg{n-slots}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (t-slot-to-p-objid n-slots depth p-heap p-objid
		     +structure-description-location-persistent-slot-numbers+
		     #'(lambda (new-value object)
			 (setf (structure-description-p-persistent-slot-numbers
				object) new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-slot-numbers
     (p-objid &optional (depth *default-depth*)
	      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-slot-numbers}
 of the persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-description-location-slot-numbers+
		     depth p-heap
		     #'(lambda (new-value object)
			 (setf (structure-description-p-slot-numbers object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description-slot-numbers)
     (n-slots p-objid &optional (depth *default-depth*)
	      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{n-slots}}
      {a fixnum}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-slot-numbers}
 of the persistent structure-description referenced by
 \\funarg{p-objid}\\ to \\funarg{n-slots}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (t-slot-to-p-objid n-slots depth p-heap p-objid
		     +structure-description-location-slot-numbers+
		     #'(lambda (new-value object)
			 (setf (structure-description-p-slot-numbers object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-slots
     (p-objid &optional (depth *default-depth*)
	      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-slots}
 of the persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (p-objid-to-t-slot p-objid
		     +structure-description-location-slot-descriptions+
		     depth p-heap 
		     #'(lambda (new-value object)
			 (setf (structure-description-p-slots object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description-slots)
     (t-slots p-objid &optional (depth *default-depth*)
	      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-slots}}
      {a list of structure slot descriptions}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-slots}
 of the persistent structure-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-slots}.
\\Seealsolabel
 Section \\fcite{p-structure-description- ...}."

  (t-slot-to-p-objid t-slots depth p-heap p-objid
		     +structure-description-location-slot-descriptions+
		     #'(lambda (new-value object)
			 (setf (structure-description-p-slots object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defconstant +slot-load-on-demand-class+ (find-class 'slot-load-on-demand)
  #+:lisp-doc "The \\clsmo\\ of \\fcite{slot-load-on-demand}.")

;;; ---------------------------------------------------------------------------
(defun load-structure-slot
  (struct-descr slot-descr t-object p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{struct-descr}}
      {a structure-description}
 \\isa{\\funarg{slot-descr}}
      {a structure slot description}
 \\isa{\\funarg{t-object}}
      {a transient structure object}
\\Purposelabel
 Load the transient representation of the persistent object referenced
 by \\funarg{p-objid}\\ if \\funarg{p-objid}'s transient
 representation is of \\fcite{slot-load-on-demand}.

 This function is called by the slot load function which was replaced
 for the system-generated structure slot read function, see
 \\fcite{establish-all-slot-loaders}. The
 \\funarg{p-objid}\\ argument is the value loaded in the slot
 denoted by the \\funarg{slot-descr}\\ in structure class given by the
 \\funarg{struct-descr}\\ argument of the structure object
 \\funarg{t-object}. The idea is that at structure object load-time
 for a structure slot which should be loaded on demand there
 is not put the `real' (i.e.\\ fully loaded persistent object
 contained in the slot) slot value into the structure object's
 slot but an instance of
 \\fcite{slot-load-on-demand}\\ which contains itself only the
 \\objid\\ of the slot-value. The system-generated structure slot
 reader for a structure slot was replaced with a \\plob-generated
 structure slot reader function which calls this function in turn;
 when this reader function finds an instance of
 \\fcite{slot-load-on-demand}\\ as the structure slot's value, it
 replaces the structure slot's value by the `real' (i.e.\\ now fully
 loaded persistent object contained in the slot) slot value.
\\Seealsolabel
 \\Fcite{structure-description};
 \\fcite{structure-slot-description}."

  (declare (inline load-structure-slot))
  (if (eq (class-of p-objid) +slot-load-on-demand-class+)
      (let ((loaded-object
	     (p-objid-to-t-object (slot-load-on-demand-objid p-objid)
				  (p-type-tag-of p-objid)
				  depth p-heap)))
	(unless (or (eq depth :objid) (eq depth :object))
	  (call-structure-slot-description-writer struct-descr slot-descr
						  p-heap t-object
						  loaded-object))
	loaded-object)
    p-objid))

;;; ---------------------------------------------------------------------------
(defun make-structure-slot-loader
    (struct-descr slot-descr old-slot-reader)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{struct-descr}}
      {a structure-description}
 \\isa{\\funarg{slot-descr}}
      {a structure slot description}
 \\isa{\\funarg{old-slot-reader}}
      {the system-generated structure slot reader function
       for structure slot \\funarg{slot-descr}\\ of instances
       of structure class \\funarg{struct-descr}}
\\Purposelabel
 Returns a function object suitable as a structure slot reader
 with loading of the structure slot's value on demand
 for structure slot \\funarg{slot-descr}\\ of instances of structure
 class \\funarg{struct-descr}.
\\Seealsolabel
 \\Fcite{load-structure-slot};
 \\fcite{establish-all-slot-loaders};
 \\fcite{structure-description}."
  #'(lambda (object &optional (load-depth *default-depth*)
                    (load-p-heap *default-persistent-heap*))
      (load-structure-slot struct-descr slot-descr object
		           (funcall old-slot-reader object)
		           load-depth
		           load-p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-structure-description-into
  (t-into-descr p-objid
   &optional (depth *default-depth*)
	     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-into-descr}}
      {a structure-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Read the persistent structure-description referenced by
 \\funarg{p-objid}\\ destructively into \\funarg{t-into-descr}.
\\Seealsolabel
 \\Fcite{p-structure-description}."

  (setf (structure-description-objid t-into-descr) p-objid)
  (with-transaction (p-heap)
    (with-read-lock (p-heap p-objid depth +structure-type-tag+ nil)
      (setf (class-description-name t-into-descr)
	(p-structure-description-name p-objid depth p-heap))
      (setf (class-description-version-number t-into-descr)
	(p-structure-description-version-number p-objid depth p-heap))
      (setf (class-description-time-stamp t-into-descr)
	(p-structure-description-time-stamp p-objid depth p-heap))
      (setf (class-description-schema-evolution t-into-descr)
	(p-structure-description-schema-evolution p-objid depth p-heap))
      (setf (class-description-next-generation t-into-descr)
	(p-structure-description-next-generation p-objid depth p-heap))
      (setf (structure-description-p-constructor t-into-descr)
	(p-structure-description-constructor p-objid depth p-heap))
      (setf (structure-description-p-dependent t-into-descr)
	(p-structure-description-dependent p-objid depth p-heap))
      (setf (class-description-slot-numbers t-into-descr)
	(p-structure-description-slot-numbers p-objid depth p-heap))
      (setf (class-description-persistent-slot-numbers t-into-descr)
	(p-structure-description-persistent-slot-numbers p-objid depth p-heap))
      (setf (class-description-effective-slots t-into-descr)
	(p-structure-description-slots p-objid depth p-heap))))
  t-into-descr)

;;; ---------------------------------------------------------------------------
(defvar *class-and-slot->reader-table* (make-hash-table :test #'equal)
  #+:lisp-doc "
\\Purposelabel
 A hash table mapping a cons of a structure class name and a slot
 name to a system-generated structure slot reader function.
 Used in \\fcite{establish-all-slot-loaders}.
\\Seealsolabel
 \\Fcite{*reader->class-and-slot-table*}.")

;;; ---------------------------------------------------------------------------
(defvar *reader->class-and-slot-table* (make-hash-table :test #'eq)
  #+:lisp-doc "The reverse
mapping for \\fcite{*class-and-slot->reader-table*}.
Used in \\fcite{establish-all-slot-loaders}.")

;;; ---------------------------------------------------------------------------
(defun establish-all-slot-loaders
    (struct-descr
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Replaces each system-generated structure slot reader function by a 
 \\plob-generated slot reader function for each slot with an extent not
 \\lisp{equal}\\ to \\lisp{:no-demand-load}.
\\Remarkslabel
 The names of the
 system-generated slot reader functions are derived from
 \\funarg{struct-descr}.
\\Seealsolabel
 \\Fcite{load-structure-slot};
 \\fcite{structure-description}."

  (let ((class-name (class-name struct-descr))
        (slots (class-slots struct-descr)))
    (when slots
      (dotimes (i (class-description-slot-numbers struct-descr))
        (let* ((slot (svref slots i))
	       (reader-symbol
                (structure-slot-description-p-reader slot))
	       (writer-symbol
                (structure-slot-description-t-writer slot))
	       (slot-name (slot-definition-name slot))
	       (slot-extent (slot-description-extent slot))
               (key (list class-name slot-name))
	       (old-reader (gethash key
				    *class-and-slot->reader-table*))
               (reader (when reader-symbol
			 (symbol-function reader-symbol))))
          (when writer-symbol
	    (setf (structure-slot-description-t-writer slot) nil)
            (fmakunbound writer-symbol))
	  (when reader-symbol
	    (cond
	     ((and (not (eq slot-extent :transient))
		   (not (eq slot-extent :cached)))
	      ;; Insert a slot-loader:
	      (unless (and old-reader
                           (equal (gethash reader
                                           *reader->class-and-slot-table*)
                                  key))
	        (remhash key *class-and-slot->reader-table*)
	        (remhash reader *reader->class-and-slot-table*)
                (setf old-reader reader)
	        (setf (gethash key *class-and-slot->reader-table*)
		      reader))
              (let ((slot-reader
		     (make-structure-slot-loader
		      struct-descr slot old-reader)))
	        (setf (symbol-function reader-symbol) slot-reader)
                ;; Please note that now
                ;; (assert (not (eq slot-reader (symbol-function
		;;				 reader-symbol))))
                ;; holds; the symbol-function is not set to the
		;; closure slot-reader but to a new function.
                (setf (gethash (symbol-function reader-symbol)
                               *reader->class-and-slot-table*)
                      key)))
	     (old-reader
	      ;; Remove an established slot-loader:
	      (setf (symbol-function reader-symbol) old-reader)
	      (remhash key *class-and-slot->reader-table*)
	      (remhash reader *reader->class-and-slot-table*))))))))
  struct-descr)

;;; ---------------------------------------------------------------------------
(defmethod class-description-equal-p (t-class-descr p-class-descr
				      &optional verbose)
  (block nil
    (when (eq t-class-descr p-class-descr)
      (return t))
    (unless (eq (class-constructor t-class-descr)
		(class-constructor p-class-descr))
      (return (values nil 
		      (when verbose
			(format nil "constructor; tr.: ~A / pe.: ~A"
				(class-constructor t-class-descr)
				(class-constructor p-class-descr))))))
    (unless (and (eq (class-dependent t-class-descr)
                     (class-dependent p-class-descr)))
      (return (values nil 
		      (when verbose
			(format nil "dependent; tr.: ~A / pe.:~A"
				(class-dependent t-class-descr)
				(class-dependent p-class-descr))))))
    (let ((t-total-number-of-slots
	   (class-description-slot-numbers t-class-descr))
          (t-number-of-persistent-slots
	   (class-description-persistent-slot-numbers t-class-descr))
          (t-slots
	   (class-slots t-class-descr))
	  (p-total-number-of-slots
	   (class-description-slot-numbers p-class-descr))
          (p-number-of-persistent-slots
	   (class-description-persistent-slot-numbers p-class-descr))
          (p-slots
	   (class-slots p-class-descr)))
      (unless (= t-total-number-of-slots p-total-number-of-slots)
        (return (values nil 
			(when verbose
			  (format nil
				  "total number of slots; tr.: ~A / pe.: ~A"
				  t-total-number-of-slots
				  p-total-number-of-slots)))))
      (unless (= t-number-of-persistent-slots
		 p-number-of-persistent-slots)
        (return (values nil
			(when verbose
			  (format
			   nil
			   "number of persistent slots; tr.: ~A / pe.: ~A"
			   t-number-of-persistent-slots
			   p-number-of-persistent-slots)))))
      (when (= t-number-of-persistent-slots 0)
	(return t))
      (dotimes (i t-total-number-of-slots t)
	;; Compare the slot descriptions:
	(multiple-value-bind (equal-p reason)
	    (slot-description-equal-p (svref t-slots i)
				      (svref p-slots i)
				      verbose)
	  (unless equal-p
	    (return (values nil reason))))))))

;;; ---------------------------------------------------------------------------
(defun make-extra-class-options (class-descr)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{class-descr}}
      {a class description}
\\Purposelabel
 Returns a human-readable form of the extra class options
 specified for \\funarg{class-descr}."

  (let* ((version-number (class-description-version-number
			  class-descr))
	 (major-version (floor version-number 100))
	 (minor-version (mod version-number 100))
	 (second 0) (minute 0) (hour 0) (date 0) (month 0) (year 0))
    (declare (ignore second))
    (multiple-value-setq (second minute hour date month year)
      (decode-universal-time
       (* (class-description-time-stamp class-descr)
	  60)))
    `((:version
       ,(format nil "~D.~2,'0D" major-version minor-version))
      (:date
       ,(format nil "~D.~2,'0D.~D ~D:~2,'0D"
		date month year hour minute))
      ,@(when (class-dependent class-descr)
	  `((:dependent ,(class-dependent class-descr))))
      ,@(when (class-extent class-descr)
	  `((:extent ,(class-extent class-descr))))
      ,@(when (schema-evolution class-descr)
	  `((:schema-evolution
	    ,(schema-evolution class-descr)))))))

;;; ---------------------------------------------------------------------------
(defmethod generate-description
     ((class-descr structure-description)
      &optional add-extra-slot-options add-extra-class-options)
  (let ((expr ())
        (slots (class-slots class-descr)))
    (loop for i
	  from (1- (class-description-slot-numbers class-descr))
	  downto 0
          as slot-descr = (svref slots i)
          as slot-name = (slot-definition-name slot-descr)
          ;; 1998/11/18 HK: Hack: Omit slots belonging to
          ;; persistent-structure, since these will be added by the
          ;; (:include persistent-structure) below:
          unless (member slot-name +structure-persistent-slot-names+)
	  do
	  (let ((one-slot-eval ()))
            (when add-extra-slot-options
	      (push (slot-description-extent slot-descr) one-slot-eval)
	      (push :extent one-slot-eval)
	      (push (slot-description-deferred slot-descr) one-slot-eval)
	      (push :deferred one-slot-eval))
	    (push (slot-definition-type slot-descr) one-slot-eval)
	    (push :type one-slot-eval)
	    (push (structure-slot-description-p-init slot-descr)
		  one-slot-eval)
	    (push slot-name one-slot-eval)
	    (push one-slot-eval expr)))
    `(defstruct (,(class-name class-descr)
                 ;; 1998/11/18 HK: Hack: Include persistent-structure
                 ;; as a `superclass'
                 (:include persistent-structure)
                 (:constructor
                  ,(structure-description-p-constructor class-descr))
                 ,@(when add-extra-class-options
                     (make-extra-class-options class-descr)))
       ,@expr)))

;;; ---------------------------------------------------------------------------
(defmethod compile-description ((class-descr structure-description))
  (when (/= (class-description-slot-numbers class-descr)
	    (length (class-slots class-descr)))
    (error "Request to compile incomplete structure description ~A."
           class-descr))
  (when (and *verbose* (>= *verbose* 2))
    (format t ";;;;; Compiling ~A~%" class-descr))
  (funcall (compile-silent nil
			   `(lambda () ,(generate-description class-descr)))))

;;; ---------------------------------------------------------------------------
(defun handle-class-mismatch
     (t-class-descr p-class-descr reason
      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-class-descr}\\ resp.\\ \\funarg{p-class-descr}}
      {a class description}
\\Valueslabel
 Returns \\nonnil\\ iff the transient class definition
 has been stored to the \\sh,
 \\lispnil\\ otherwise.
\\Purposelabel
 Handles a mismatch between the class description
 of the class as found in the transient system
 \\funarg{t-class-descr}\\ and the class description
 found in the \\sh\\ \\funarg{p-class-descr}.

 The user is asked what \\plob\\ should do next;
 either \\funarg{t-class-descr}\\ replaces the current class
 description stored in the \\sh\\ (this is the normal way of
 schema evolution) or the class description in the \\sh\\ is
 replaced by \\funarg{t-class-descr}\\ (this would be the not
 so normal way of `schema degration').
\\Remarkslabel
 Perhaps \\plob\\ should always choose the first way by default
 without asking for user-intervention; the error prompt raised
 here is a kind of `safety valve' to escape from an unwanted
 schema evolution.
\\Seealsolabel
 \\Fcite{(setf schema-evolution)}."

    (if (and *verbose* (>= *verbose* 1))
	(let* ((name (class-name t-class-descr))
	       (the-class (find-class name)))
	  (loop
	    (restart-case
		(error "The persistent definition for class
~S does not match its transient counterpart.~%       Reason: ~A"
		       name reason)
	      (continue
		  ()
		  :report
		    "Store the transient definition to the persistent store."
		(update-class t-class-descr p-heap)
		(return nil))
	      (clobber-transient-definition
		  ()
		  :report
		    "Replace the transient definition by its persistent counterpart."
		(compile-description p-class-descr)
		(setf (mismatch-p the-class) nil)
		(return t))
	      (ignore
		  ()
		  :report
		    "Ignore definition mismatch (might result in LISP runtime errors)."
		(return nil))
	      (show-transient-description
		  ()
		  :report
		    "Show the transient class definition."
		(write-string "Transient definition" *query-io*)
		(pprint (generate-description t-class-descr t) *query-io*)
		(terpri *query-io*))
	      (show-persistent-description
		  ()
		  :report
		    "Show the persistent class definition."
		(write-string "Persistent definition" *query-io*)
		(pprint (generate-description p-class-descr t t) *query-io*)
		(terpri *query-io*)))))
      (update-class t-class-descr p-heap)))

;;; ---------------------------------------------------------------------------
(defun ensure-structure-description (name &optional (depth *default-depth*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{name}}
      {a symbol naming a structure class}
\\Valueslabel
 Returns two values:
 \\begin{enumerate}

 \\item The actual structure-description of the structure class
  named \\funarg{name}\\ is returned as the first value.

 \\item If the structure-description was subject to schema evolution,
  \\nonnil\\ is returned as the second value; \\lispnil\\ otherwise.

 \\end{enumerate}
\\Purposelabel
 Establish the structure class named \\funarg{name}:
 \\begin{itemize}

 \\item When there was no structure-description named
  \\funarg{name}\\ found in the \\sh\\ class table, store the
  structure-description of the structure named
  \\funarg{name}\\ to the \\sh.

 \\item When a structure-description named
  \\funarg{name}\\ was found in the \\sh\\ class table, check if there
  is a mismatch between the persistent structure-description and the
  current transient version of the structure definition.
  If a mismatch is detected, resolve the mismatch by calling
  \\fcite{handle-class-mismatch}. Do nothing if no mismatch
  was detected.

 \\end{itemize}
\\Seealsolabel
 \\Fcite{p-find-class};
 \\fcite{handle-class-mismatch};
 \\fcite{ensure-class-description}."

  (if (eq name 'structure-description)
      (case depth
        (:objid
         *structure-description-objid*)
        (:object
         (make-persistent-object *structure-description* +structure-type-tag+))
        (t
         *structure-description*))
    (let* ((*default-depth* depth)
	   (mismatch-p nil)
	   (reason nil)
           (establish-slot-loaders-p mismatch-p)
	   (t-struct-descr nil)
           (p-objid-struct-descr
	    (p-find-class-objid name *default-persistent-heap*))
           (p-struct-descr (when p-objid-struct-descr
                             (p-structure-description
			      p-objid-struct-descr
			      :cached
			      *default-persistent-heap*)))
           (the-class (find-class name nil)))
      (declare (special *default-depth*))
      (if p-objid-struct-descr
          (if the-class
	      (when (mismatch-p the-class)
	        (setf establish-slot-loaders-p t)
	        (unless t-struct-descr
		  (setf t-struct-descr (fill-description the-class)))
	        (multiple-value-setq (mismatch-p reason)
		    (class-description-equal-p t-struct-descr
					       p-struct-descr t))
	        (setf mismatch-p (not mismatch-p))
	        (when (and mismatch-p
                           (eq (class-description-schema-evolution
                                p-struct-descr)
                               :no-evolution))
		  (when (and *verbose* (>= *verbose* 1))
		    (warn "Structure ~S is marked with :no-evolution ~
                         but its transient definition was modified.~%       ~
                         Reason: ~A"
			  name reason))
                  (setf mismatch-p nil)
                  (setf establish-slot-loaders-p nil)))
            (progn
              ;; There is no class definition up to now; so there can also
              ;; be no mismatch:
	      (setf establish-slot-loaders-p t)
	      (setf t-struct-descr p-struct-descr)
              (compile-description p-struct-descr)
              (setf the-class (find-class name))))
        (progn
          ;; There is no persistent structure description up to now;
          ;; so generate, fill ...
	  (setf establish-slot-loaders-p t)
          (unless the-class
            (error "Could not locate class named ~A"
                   (pretty-print-symbol name)))
          (setf p-struct-descr (fill-description the-class))
	  (setf t-struct-descr p-struct-descr)
          ;; ... and save it:
          (update-class p-struct-descr *default-persistent-heap*)))
      (setf (mismatch-p the-class) mismatch-p)
      (when mismatch-p
        (unless t-struct-descr
	  (setf t-struct-descr (fill-description the-class)))
        (if (handle-class-mismatch t-struct-descr p-struct-descr reason
				   *default-persistent-heap*)
	    (setf establish-slot-loaders-p t)
	  (setf p-struct-descr nil)))
      (when establish-slot-loaders-p
        (unless t-struct-descr
	  (setf t-struct-descr (fill-description the-class)))
        (establish-all-slot-loaders t-struct-descr depth
				    *default-persistent-heap*))
      (unless p-struct-descr
        (setf p-struct-descr
	      (p-structure-description
	       (p-find-class-objid name *default-persistent-heap*)
	       :cached *default-persistent-heap*)))
      (values p-struct-descr (or mismatch-p establish-slot-loaders-p)))))

;;; ---------------------------------------------------------------------------
(defun p-structure-description
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 structure-description
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-structure-description)}."

  (let ((t-object (is-registered-objid p-objid)))
    (unless t-object
      (setf t-object (make-structure-description))
      (register-to-cache p-objid t-object)
      (p-structure-description-into t-object p-objid depth p-heap))
    t-object))

;;; ---------------------------------------------------------------------------
(defun store-structure-description (t-descr p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a structure-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient structure-description in
 \\funarg{t-descr}\\ to the
 persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-structure-description}."

  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-descr))
      (unless p-objid
	(setf p-objid (p-allocate-class-description p-heap))
	(setf force-write t)))
  (setf (structure-description-objid t-descr) p-objid)
  (with-transaction (p-heap)
    (with-write-lock (p-heap p-objid t-descr depth
			     +structure-type-tag+ force-write)
      (setf (p-structure-description-name p-objid depth p-heap)
	(class-name t-descr))
      (setf (p-structure-description-version-number p-objid depth p-heap)
	(class-description-version-number t-descr))
      (setf (p-structure-description-time-stamp p-objid depth p-heap)
	(class-description-time-stamp t-descr))
      (setf (p-structure-description-schema-evolution p-objid depth p-heap)
	(class-description-schema-evolution t-descr))
      (setf (p-structure-description-next-generation p-objid depth p-heap)
	(class-description-next-generation t-descr))
      (setf (p-structure-description-constructor p-objid depth p-heap)
	(structure-description-p-constructor t-descr))
      (setf (p-structure-description-dependent p-objid depth p-heap)
	(structure-description-p-dependent t-descr))
      (setf (p-structure-description-slot-numbers p-objid depth p-heap)
	(class-description-slot-numbers t-descr))
      (setf (p-structure-description-persistent-slot-numbers
	     p-objid depth p-heap)
	(class-description-persistent-slot-numbers t-descr))
      (setf (p-structure-description-slots p-objid depth p-heap)
	(class-slots t-descr)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-description)
    (t-descr &optional p-objid (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a structure-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient structure-description in
 \\funarg{t-descr}\\ to the
 persistent structure-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-structure-description}."

  (values t-descr (store-structure-description t-descr p-objid depth p-heap)))
  
;;; ---------------------------------------------------------------------------
(defmethod ensure-allocated-object
     ((t-object structure-description)
      &optional (p-heap *default-persistent-heap*))
  (let ((p-objid (persistent-object-objid t-object)))
    (unless p-objid
      (setf p-objid (p-allocate-structure-description p-heap))
      (setf (persistent-object-objid t-object) p-objid))
    p-objid))

;;; ---------------------------------------------------------------------------
(defun register-class
     (class-descr &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{class-descr}}
      {a class description}
\\Purposelabel
 Registers \\funarg{class-descr}\\ to the \\plob\\ class table
 contained in the \\fcite{*symbol->class-table*}."

  ;; 1996/11/19 HK: Commented out:
  ;; (register-to-cache class-descr class-descr)
  (let* ((name (class-name class-descr))
	 (p-name (t-object-to-p-objid name :cached p-heap)))
    (setf (getbtree-by-objid p-name
                             *symbol->class-table*
                             :objid
                             p-heap)
	  (ensure-allocated-object class-descr))))

;;; ---------------------------------------------------------------------------
(defmethod update-class
     (class-descr &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Updates \\funarg{class-descr}\\ in the \\plob\\ class table
 contained in the \\fcite{*symbol->class-table*}."

  (let* ((*default-depth*
	 ;; :deep ;; 1998/09/18 HK
	 :cached
	 )
	 (name (class-name class-descr))
	 (old-class-descr (p-find-class-description name p-heap))
         (p-objid (ensure-allocated-object class-descr p-heap)))
    (unless p-objid
      (error "Trying to update the non-allocated class ~A." name))
    (register-class class-descr p-heap)
    (cond
     (old-class-descr
      (let ((new-version-number (1+ (class-description-version-number
                                     old-class-descr))))
        (setf (class-description-next-generation old-class-descr)
              class-descr)
	(setf (class-description-version-number class-descr)
              new-version-number)
        (t-object-to-p-objid class-descr *default-depth* p-heap)
        (when (and *verbose* (>= *verbose* 2))
          (format t ";;;;; Updated ~A.~%" class-descr))
	(t-object-to-p-objid old-class-descr *default-depth* p-heap)))
     (t
      (t-object-to-p-objid class-descr *default-depth* p-heap)
      (when (and *verbose* (>= *verbose* 2))
	(format t ";;;;; Stored ~A.~%" class-descr))))
    (setf (mismatch-p (find-class name)) nil)
    p-objid))

;;; ---------------------------------------------------------------------------
(defmethod update-class :after
	   ((class-descr structure-description)
	    &optional (p-heap *default-persistent-heap*))
  (declare (ignore p-heap))
  (setf (p-dependent (p-structure-description-slots class-descr :objid))
	:read))

;;; ---------------------------------------------------------------------------
(defmethod reinitialize-instance :after ((the-class structure-class)
					 &rest initargs)
  #+:lisp-doc "Marks \\funarg{the-class}\\ as being (perhaps) mismatched;
 see \\fcite{(setf mismatch-p)}."
  (declare (ignore initargs))
  (setf (mismatch-p the-class) t))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
