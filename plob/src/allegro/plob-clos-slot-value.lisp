;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-clos-slot-value.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	14.4..94
;;;; Description	PLOB functions for CLOS instances slot access
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
(defconstant +cant-setf-transient-slot-error-prompt+
  "Cannot setf transient slot ~S for ~A of class ~S to value ~A."
  #+:lisp-doc "Prompt which is shown in the error message concerning non-setf'able slots.")

;;; ---------------------------------------------------------------------------
(defconstant +cant-load-transient-slot-error-prompt+
  "Cannot load transient slot ~S for ~A of class ~S."
  #+:lisp-doc "Prompt which is shown in the error message concerning non-loadable slots.")

;;; ---------------------------------------------------------------------------
(defconstant +cant-find-slot-error-prompt+
  "Cannot find slot ~S for ~A of class ~S."
  #+:lisp-doc "Prompt which is shown in the error message concerning non-found slots.")

;;; ---------------------------------------------------------------------------
(defconstant +cant-boundp-transient-slot-error-prompt+
  "Cannot boundp transient slot ~S for ~A of class ~S."
  #+:lisp-doc "Prompt which is shown in the error message concerning
 non-boundp'able slots.")

;;; ---------------------------------------------------------------------------
(defconstant +cant-makunbound-transient-slot-error-prompt+
  "Cannot makunbound transient slot ~S for ~A of class ~S."
  #+:lisp-doc "Prompt which is shown in the error message concerning
 non-makunbound'able slots.")

;;; ---------------------------------------------------------------------------
(defun get-instance-vector-and-location (the-class object slot-description)
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isaclosobject{\\funarg{object}}
 \\isa{\\funarg{slot-description}}
      {a slot-description}
\\Valueslabel
  Four values are returned. The kind of values returned depend
  upon the allocation of the slot described by
  \\funarg{slot-description}:
  \\begin{itemize}
  \\item The slot described by \\funarg{slot-description}\\ has
    an \\lisp{:allocation :class}:
   \\begin{enumerate}

   \\item The \\objid\\ of the persistent cons cell which is used
    for holding the slot value is returned as first value.

   \\item The position in the cons cell which contains the slot
    value is returned; this is the position {\\bf +cons-location-cdr+}
    since the value is stored in the cdr of the persistent cons cell.

   \\item The \\typetag\\ for type cons is returned as third value,
    i.e.\\ the value of the constant {\\bf +cons-type-tag+}.

   \\item The slot allocation keyword {\\bf :class} is returned as
    fourth value.

   \\end{enumerate}

  \\item The slot described by \\funarg{slot-description}\\ has
   not an \\lisp{:allocation :class}, i.e.\\ is a `normal'
   instance slot:

   \\begin{enumerate}

   \\item The \\objid\\ of the persistent vector which is used
    for holding the persistent \\clos\\ instance slot values is
    returned as first value.

   \\item The position in the vector which contains the slot
    value is returned.

   \\item The \\typetag\\ for type vector is returned as third
    value, i.e.\\ the value of the constant
    {\\bf +vector-type-tag+}.

   \\item The slot allocation keyword {\\bf :instance} is
    returned as fourth value.

   \\end{enumerate}
  \\end{itemize}
  For slot representation see also slot {\\bf p-location}
  of \\fcite{effective-slot-description}.
\\Purposelabel
 This function is used for preparing the slot access to a
 persistent \\clos\\ instance. Returned are all informations
 necessary for a controlled access to the slot of a
 persistent \\clos\\ instance."

  (let ((class-description (class-description-of the-class))
	(p-objid (persistent-object-objid object)))
    (unless p-objid
      ;; The object has no objid, so it is not yet allocated;
      ;; do so now:
      (setf p-objid (p-allocate-instance class-description))
      (register-to-cache p-objid object)
      (setf (persistent-object-objid object) p-objid))
    (let* ((p-objid-vector +null-objid+) (slot-location 0)
           (expect-type-tag +null-type-tag+)
           (slot-allocation (slot-definition-allocation slot-description)))
      (declare (type fixnum p-objid-vector slot-location expect-type-tag))
      (ecase slot-allocation
        (:structure
	 (setf p-objid-vector p-objid)
	 (setf expect-type-tag +structure-type-tag+)
         (setf slot-location (slot-definition-location slot-description)))
        (:instance
	 (setf p-objid-vector
	       (gethash p-objid *instance->data-vector-cache*))
	 (unless p-objid-vector
	   (setf p-objid-vector
		 (p-instance-data-vector p-objid :objid
					 *default-persistent-heap*))
	   (setf (gethash p-objid *instance->data-vector-cache*)
		 p-objid-vector))
	 (setf expect-type-tag +vector-type-tag+)
	 (setf slot-location
	       (slot-definition-location slot-description)))
        (:class
	 (setf slot-location (slot-description-location slot-description))
	 (setf p-objid-vector (is-registered-object slot-location))
	 (unless p-objid-vector
	   (setf p-objid-vector
		 (p-slot-description-location slot-description :objid
					      *default-persistent-heap*))
	   (let ((registered-location
		  (is-registered-objid p-objid-vector)))
	     (if registered-location
		 (setf (slot-description-location slot-description)
		       registered-location)
	       (register-to-cache p-objid-vector slot-location))))
	 (setf expect-type-tag +cons-type-tag+)
	 (setf slot-location +cons-location-cdr+)))
      (values p-objid-vector slot-location expect-type-tag slot-allocation))))

;;; ---------------------------------------------------------------------------
;;; HK 9.8.94:
;;; To Do: Call slot-missing instead signalling an error:
;;; ---------------------------------------------------------------------------
(defun find-effective-slot-description-by-objid
     (slot-name p-objid
		&optional (depth :cached) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{slot-name}}
      {a symbol naming an effective slot of the class of \\funarg{p-objid}}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 Returns three values:
 \\begin{enumerate}

 \\item The \\clsmo\\ of the class of the persistent \\clos\\ instance
   referenced by \\funarg{p-objid}\\ is returned as first value.

 \\item The effective-slot-description object of the slot
   named \\funarg{slot-name}\\ in the class of the persistent
   \\clos\\ instance referenced by \\funarg{p-objid}\\ is
   returned as second value.

  \\item If a transient object associated to
   \\funarg{p-objid}\\ is found in the cache, this transient object
   is returned as third value, \\lispnil\\ otherwise.

 \\end{enumerate}
\\Purposelabel
 Search the slot named \\funarg{slot-name}\\ in the class of the
 persistent \\clos\\ instance referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{find-effective-slot};
 \\fcite{find-effective-slot-description};
 \\fcite{find-slot-description}."

  (declare (inline find-effective-slot-description-by-objid)
	   (ignore depth))
  (let* ((t-object (is-registered-objid p-objid))
	 (real-class (if t-object
			 (class-of t-object)
		       (find-class 
                        (if (p-instancep p-objid p-heap)
                            (p-class-description-name
			     (p-instance-class-wrapper p-objid :objid p-heap)
			     :cached p-heap)
                          (p-structure-description-name
                           (p-structure-descr p-objid :objid p-heap)
			   :cached p-heap)))))
         (effective-slot-description
	  (when real-class
	    (find-effective-slot-description slot-name real-class))))
    (if effective-slot-description
	(values real-class effective-slot-description t-object)
      (error +cant-find-slot-error-prompt+
	     slot-name
	     (sh-pprint-objid (persistent-object-objid p-heap)
			      (persistent-object-objid p-objid)
			      (p-type-tag-of p-objid))
	     (if real-class real-class :dont-know)))))

;;; ---------------------------------------------------------------------------
;;; slot-value
;;; ---------------------------------------------------------------------------
(defun load-slot-value-using-class
     (the-class the-object slot-description
		&optional (depth *default-depth*)
                (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isaclosobject{\\funarg{the-object}}
 \\isa{\\funarg{slot-description}}
      {an effective slot-description}
\\Purposelabel
 Load the value of the slot described by
 \\funarg{slot-description}\\ of the persistent
 \\clos\\ instance \\funarg{the-object}\\ of class
 \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{persistent-slot-value-using-class};
 \\fcite{slot-value-using-class (standard-class t %
effective-slot-description)}."

  (declare (inline load-slot-value-using-class))
  (multiple-value-bind (p-objid-vector slot-location expecting-type-tag)
      (get-instance-vector-and-location
       the-class the-object slot-description)
    (with-transaction (p-heap)
      (multiple-value-bind (sub-objid sub-type-tag)
	  (p-index p-heap p-objid-vector slot-location expecting-type-tag)
	(when (and (p-markerp sub-type-tag)
		   (eql sub-type-tag +unbound-type-tag+))
	  (error "In ~A of class ~S, slot ~S is unbound."
		 the-object the-class slot-description))
	(when (and *verbose* (>= *verbose* 7))
	  (format t "; Loading slot ~A with depth ~A of~%; ~A~%"
		  (slot-definition-name slot-description)
		  depth the-object))
	(values
	 (p-objid-to-t-object sub-objid sub-type-tag depth p-heap)
	 sub-objid
	 sub-type-tag)))))

;;; ---------------------------------------------------------------------------
(defun persistent-slot-value-using-class
     (the-class p-objid slot-name next-method
		&optional (depth *default-depth*)
		(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{slot-name}}
      {a symbol naming an effective slot of \\funarg{the-class}}
\\Purposelabel
 Load the slot named \\funarg{slot-name}\\ of the persistent
 \\clos\\ instance referenced by \\funarg{p-objid}\\ of class
 \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{load-slot-value-using-class};
 %% \\fcite{slot-value-using-class (built-in-class integer t)};
 \\fcite{slot-value-using-class (structure-class persistent-object t)}."

  (declare (inline persistent-slot-value-using-class))
  (multiple-value-bind (real-class slot-description)
      (find-effective-slot-description-by-objid slot-name p-objid)
    (if (eq (slot-description-extent slot-description) :transient)
	(if (and (eq real-class the-class) next-method)
	    (funcall next-method)
	    (error +cant-load-transient-slot-error-prompt+
		   slot-name p-objid the-class))
      (load-slot-value-using-class real-class p-objid slot-description
                                   depth p-heap))))

;;; ---------------------------------------------------------------------------
(defun load-slot-value-using-class-on-demand-with-extent
     (the-class the-object slot-description slot-extent transient-slot-reader
		&optional (depth *default-depth*)
                (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isaclosobject{\\funarg{the-object}}
 \\isa{\\funarg{slot-description}}
      {an effective slot-description}
\\Purposelabel
 Load the value of the slot described by
 \\funarg{slot-description}\\ of the persistent
 \\clos\\ instance \\funarg{the-object}\\ of class
 \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{persistent-slot-value-using-class};
 \\fcite{slot-value-using-class (standard-class t %
effective-slot-description)}."

  (declare (inline load-slot-value-using-class-on-demand-with-extent))
  (if (eq slot-extent :persistent)
      (load-slot-value-using-class the-class the-object slot-description
				   depth p-heap)
    (let ((the-slot-value (funcall transient-slot-reader)))
      (if (or (eq slot-extent :transient)
	      (with-direct-representation-p slot-extent))
	    the-slot-value
	(let ((direct-representation-p (with-direct-representation-p depth))
	      (slot-load-on-demand-p
	       (eq (class-of the-slot-value) +slot-load-on-demand-class+)))
	  (cond
	   ((and (null direct-representation-p)
		 (null slot-load-on-demand-p))
	    the-slot-value)
	   ((and (null direct-representation-p)
		 slot-load-on-demand-p)
	    (with-transaction (p-heap)
	      (let ((*default-depth* depth))
		(setf the-slot-value
		  (p-objid-to-t-object
		   (slot-load-on-demand-objid the-slot-value)
		   (slot-load-on-demand-type-tag the-slot-value)
		   *default-depth* p-heap))))
	    (let ((*transient-slot-value* t))
	      (setf (slot-value the-object
				(slot-definition-name slot-description))
		the-slot-value))
	    the-slot-value)
	   ((and direct-representation-p
		 (null slot-load-on-demand-p))
	    (multiple-value-bind (the-slot-object sub-objid sub-type-tag)
		(load-slot-value-using-class
		 the-class the-object slot-description
		 :object p-heap)
	      (let ((*transient-slot-value* t))
		(setf (slot-value the-object
				  (slot-definition-name slot-description))
		  (make-slot-load-on-demand sub-objid sub-type-tag)))
	      (if (eq depth :objid) sub-objid the-slot-object)))
	   (t ;; (and direct-representation-p slot-load-on-demand-p)
	    (if ( eq depth :objid)
		(persistent-object-objid the-slot-value)
	      (make-persistent-object
	       (persistent-object-objid the-slot-value)
	       (p-type-tag-of the-slot-value))))))))))

;;; ---------------------------------------------------------------------------
(defun load-slot-value-using-class-on-demand
     (the-class the-object slot-description transient-slot-reader
		&optional (depth *default-depth*)
                (p-heap *default-persistent-heap*))
  (load-slot-value-using-class-on-demand-with-extent
   the-class the-object slot-description
   (slot-description-extent slot-description)
   transient-slot-reader depth p-heap))

;;; ---------------------------------------------------------------------------
#|
(defmethod slot-value-using-class
  ((the-class built-in-class) (p-objid integer) slot-name)
  #+:lisp-doc "A method for low-level slot reading:
 The persistent \\clos\\ instance is referenced directly by the
 numeric \\funarg{p-objid}.
\\Remarkslabel
 It was no good idea to specify a method on
 \\class{built-in-class}\\ \\class{integer}, since this crashed
 \\allegro"
  (persistent-slot-value-using-class the-class p-objid slot-name
				     #'call-next-method))
|#

;;; ---------------------------------------------------------------------------
(defmethod slot-value-using-class
    ((the-class structure-class) (p-objid persistent-object)
     (slot-name symbol))
  #+:lisp-doc "Trapped to
 \\fcite{slot-value-using-class (structure-class t %
structure-slot-description)}."
  (multiple-value-bind (real-class slot-description)
      (find-effective-slot-description-by-objid slot-name p-objid)
    (slot-value-using-class real-class p-objid slot-description)))

;;; ---------------------------------------------------------------------------
(defmethod slot-value-using-class
    ((the-class structure-class) object
     (slot-description structure-slot-description))
  (cond
   ((not (eq (slot-description-extent slot-description) :transient))
    (with-transaction (*default-persistent-heap*)
      (multiple-value-bind
	  (p-objid-vector slot-location expecting-type-tag)
	  (get-instance-vector-and-location the-class object
					    slot-description)
	(p-objid-to-t-slot-in-transaction
	 p-objid-vector slot-location *default-depth*
	 *default-persistent-heap* nil expecting-type-tag))))
   ((eq the-class (class-of object))
    (call-next-method the-class object
		      (slot-definition-name slot-description)))
   (t
    (error +cant-load-transient-slot-error-prompt+
	   slot-description object the-class))))

;;; ---------------------------------------------------------------------------
(defmethod slot-value-using-class
     ((the-class persistent-metaclass) object (slot-name symbol))
  #+:lisp-doc "
 Trapped to
 \\fcite{slot-value-using-class (standard-class t %
effective-slot-description)}."
  (let ((effective-slot-description
         (find-effective-slot-description slot-name the-class)))
    (if effective-slot-description
        (slot-value-using-class the-class object effective-slot-description)
      (call-next-method))))

;;; ---------------------------------------------------------------------------
(defmethod slot-value-using-class
     ((the-class standard-class) object
      (slot-description effective-slot-description))
  (load-slot-value-using-class-on-demand
   the-class object slot-description
   #'(lambda ()
       (call-next-method the-class object
			 (slot-definition-name slot-description)))))

;;; ---------------------------------------------------------------------------
#+:allegro
(defmethod slot-value-using-class
    ((the-class persistent-metaclass) object
     (slot-description effective-slot-description))
  #+:lisp-doc "
\\Remarkslabel
  In \\allegro, this method specialized to
  \\fcite{persistent-metaclass}\\ must be defined; the method
  \\fcite{slot-value-using-class (standard-class t effective-slot-description)}\\ signals a Bus error."
  (load-slot-value-using-class-on-demand
   the-class object slot-description
   #'(lambda ()
       (call-next-method the-class object
			 (slot-definition-name slot-description)))))

;;; ---------------------------------------------------------------------------
(defconstant +effective-slot-description-class+
  (find-class 'effective-slot-description)
  #+:lisp-doc "The \\clsmo\\ of \\fcite{effective-slot-description}.")

;;; ---------------------------------------------------------------------------
(defmethod slot-value-using-class
    ((the-class persistent-metaclass) object
     (slot-definition persistent-effective-slot-definition))
  (cond
   (*in-bootstrap-p*
    (call-next-method))
   ((eq the-class +effective-slot-description-class+)
    (load-slot-value-using-class-on-demand-with-extent
     the-class object slot-definition :cached #'call-next-method :cached))
   (t
    (let ((slot-description
	   (persistent-slot-definition-description slot-definition)))
      (unless slot-description
	(setf slot-description (find-effective-slot-description
				slot-definition the-class))
	(setf (persistent-slot-definition-description slot-definition)
	  slot-description))
      (if slot-description
	  (load-slot-value-using-class-on-demand
	   the-class object slot-description #'call-next-method)
	(call-next-method))))))

;;; ---------------------------------------------------------------------------
;;; (setf slot-value)
;;; ---------------------------------------------------------------------------
(defun store-slot-value-using-class
    (new-value the-class the-object slot-description
     call-next-method
     &optional (depth *default-depth*)
	       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
  Workhorse for \\textbf{slot-value-using-class}.
"
  (declare (inline store-slot-value-using-class))
  (let* ((real-class-p (eq (class-of the-object) the-class))
	 (slot-extent (slot-description-extent slot-description))
	 (slot-write-through-p (slot-write-through-extent-p slot-extent))
	 (old-objid nil) (old-type-tag nil)
	 (sub-objid nil) (sub-type-tag nil)
	 (index (slot-description-index slot-description))
	 (dummy nil))
    (declare
     #-:lispworks4 ;; and hopefully not later
     (dynamic-extent slot-extent slot-write-through-p
		     old-objid old-type-tag sub-objid sub-type-tag
		     index dummy)
     (ignore dummy))
    (with-transaction (p-heap)
      (cond
       (slot-write-through-p
	(multiple-value-bind
	    (p-objid-vector slot-location expecting-type-tag slot-allocation)
	    (get-instance-vector-and-location the-class the-object
					      slot-description)
	  (when index
	    (multiple-value-setq (dummy old-objid old-type-tag)
	      (p-objid-to-t-slot-in-transaction
	       p-objid-vector slot-location :objid p-heap nil
	       expecting-type-tag)))
	  (when (and *verbose* (>= *verbose* 7))
	    (format t "; Storing slot ~A with depth ~A of~%; ~A~%"
		    (slot-definition-name slot-description)
		    depth the-object)
	    ;; 1998/12/03 HK: Debug:
	    (when (eq (class-name (class-of new-value))
		      'persistent-btree)
	      (format t "; Storing btree ~A~%"
		      new-value depth))
	    )
	  (multiple-value-setq (dummy sub-objid sub-type-tag)
	    (t-slot-to-p-objid-in-transaction
	     new-value depth p-heap p-objid-vector slot-location
	     nil expecting-type-tag))
	  ;; 1998/12/03 HK: Debug:
	  (when (and *verbose* (>= *verbose* 7))
	    (format t "; Slot ~A contains objid ~A, type-tag ~A~%"
		    (slot-definition-name slot-description)
		    sub-objid sub-type-tag))
	  (when (eq slot-allocation :class)
	    ;; Change also the transient value:
	    (setf (cdr (slot-definition-location slot-description))
	      new-value)
	    ;; For LispWorks, the transient value of a
	    ;; :class slot is set too:
	    #+:lispworks
	    (progn
	      (funcall call-next-method new-value the-class the-object
		       (slot-definition-name slot-description))
	      (setf slot-write-through-p nil)))))
       (real-class-p
	(setf slot-write-through-p t))
       (t
	(error +cant-setf-transient-slot-error-prompt+
	       slot-description the-object the-class new-value)))
      (when index
	(unless sub-objid
	  (multiple-value-setq (dummy sub-objid sub-type-tag)
	    (t-object-to-p-objid new-value depth p-heap)))
	(let ((p-objid (persistent-object-objid the-object))
	      (index-objid (getindex-by-tag sub-objid sub-type-tag
					    index :objid
					    p-heap)))
	  #-:lispworks4 ;; and hopefully not later
	  (declare (dynamic-extent p-objid index-objid))
	  (unless (or (null index-objid)
		      (eql index-objid p-objid))
	    (error "Trying to set duplicate slot value ~S for index ~A on slot ~A of object ~A; object which captures the value already is ~A."
		   new-value index slot-description
		   (make-persistent-object p-objid)
		   (make-persistent-object index-objid)))
	  ;; Remove the old entry from the index:
	  (remindex-by-tag old-objid old-type-tag
			   index p-heap)
	  ;; Insert the new entry into the index:
	  (setf (getindex-by-tag sub-objid sub-type-tag
				 index :objid p-heap)
	    p-objid))))
    (when (and slot-write-through-p real-class-p
	       (member slot-extent +plob-slot-represented-extents+))
      (let ((slot-name (slot-definition-name slot-description)))
	(case slot-extent
	  (:objid
	   ;; Put the numerical objid into the slot:
	   (when (p-immediatep sub-type-tag)
	     ;; Immediates can't be represented in an
	     ;; objid, so set the slot to NIL instead:
	     (when (and *verbose* (>= *verbose* 1))
	       (warn +cant-set-slot-to-immediate+
		     slot-name the-object sub-objid))
	     (setf sub-objid nil))
	   (setf new-value sub-objid))
	  (:object
	   (setf new-value (make-persistent-object sub-objid sub-type-tag))))
	(funcall call-next-method new-value the-class the-object slot-name))))
  new-value)

;;; ---------------------------------------------------------------------------
#|
(defmethod (setf slot-value-using-class)
  (new-value (the-class built-in-class) (p-objid integer) slot-name)
  #+:lisp-doc "Trapped to
 \\fcite{(setf slot-value-using-class) (t standard-class t %
effective-slot-description)}.
\\Remarkslabel
 It was no good idea to specify a method on
 \\class{built-in-class}\\ \\class{integer}, since this crashed
 \\allegro"
  (multiple-value-bind (real-class slot-description)
      (find-effective-slot-description-by-objid slot-name p-objid)
    (setf (slot-value-using-class real-class p-objid slot-description)
          new-value)))
|#

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-value-using-class)
     (new-value (the-class structure-class) (p-objid persistent-object)
                (slot-name symbol))
  #+:lisp-doc "Trapped to
 \\fcite{(setf slot-value-using-class) (t structure-class t %
structure-slot-description)}."
  (multiple-value-bind (real-class slot-description)
      (find-effective-slot-description-by-objid slot-name p-objid)
    (setf (slot-value-using-class real-class p-objid slot-description)
          new-value)))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-value-using-class)
     (new-value (the-class persistent-metaclass) object (slot-name symbol))
  #+:lisp-doc "Trapped to
 \\fcite{(setf slot-value-using-class) (t standard-class t %
effective-slot-description)}."
  ;; 1998/04/09 HK: Debug:
  (when (and *verbose* (>= *verbose* 8))
    (format t "; (setf slot-value-using-class) (persistent-metaclass t symbol)~%"))
  (if *transient-slot-value*
      (call-next-method)
    (let ((effective-slot-description
           (find-effective-slot-description slot-name the-class)))
      (if effective-slot-description
          (setf (slot-value-using-class the-class object
                                        effective-slot-description)
                new-value)
	  (call-next-method)))))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-value-using-class)
    (new-value (the-class structure-class) object
     (slot-description structure-slot-description))
  (cond
   ((not (eq (slot-description-extent slot-description) :transient))
    (with-transaction (*default-persistent-heap*)
      (multiple-value-bind
	  (p-objid-vector slot-location expecting-type-tag)
	  (get-instance-vector-and-location the-class object
					    slot-description)
	(t-slot-to-p-objid-in-transaction
	 new-value *default-depth* *default-persistent-heap*
	 p-objid-vector slot-location nil expecting-type-tag))))
   ((eq the-class (class-of object))
    (call-next-method new-value the-class object
		      (slot-definition-name slot-description)))
   (t
    (error +cant-setf-transient-slot-error-prompt+
	   slot-description object the-class new-value)))
  new-value)

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-value-using-class)
     (new-value (the-class standard-class) object
                (slot-description effective-slot-description))
  ;; 1998/04/09 HK: Debug:
  (when (and *verbose* (>= *verbose* 8))
    (format t "; (setf slot-value-using-class) (standard-class t effective-slot-description)~%"))
  (if *transient-slot-value*
      (call-next-method new-value the-class object
			(slot-definition-name slot-description))
    (store-slot-value-using-class new-value the-class object slot-description
				  #'call-next-method)))

;;; ---------------------------------------------------------------------------
#+:allegro
(defmethod (setf slot-value-using-class)
    (new-value (the-class persistent-metaclass) object
     (slot-description effective-slot-description))
  #+:lisp-doc "
\\Remarkslabel
  In \\allegro, this method specialized to
  \\fcite{persistent-metaclass}\\ must be defined; the method
  \\fcite{(setf slot-value-using-class) (t standard-class t effective-slot-description)}\\ signals a Bus error."
  (if *transient-slot-value*
      (call-next-method new-value the-class object
			(slot-definition-name slot-description))
    (store-slot-value-using-class new-value the-class object slot-description
				  #'call-next-method)))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-value-using-class)
    (new-value (the-class persistent-metaclass) object
     (slot-definition persistent-effective-slot-definition))
  (if (or *in-bootstrap-p* *transient-slot-value*)
      (call-next-method)
    (let ((slot-description
	   (persistent-slot-definition-description slot-definition)))
      (unless slot-description
	(setf slot-description (find-effective-slot-description
				slot-definition the-class))
	(setf (persistent-slot-definition-description slot-definition)
	  slot-description))
      (if slot-description
	  (store-slot-value-using-class new-value the-class
					object slot-description
					#'call-next-method)
	(call-next-method)))))

;;; ---------------------------------------------------------------------------
;;; boundp
;;; ---------------------------------------------------------------------------
(defun load-slot-boundp-using-class 
     (the-class the-object slot-description
		&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isaclosobject{\\funarg{the-object}}
 \\isa{\\funarg{slot-description}}
      {an effective slot-description}
\\Purposelabel
 Check if the slot described by \\funarg{slot-description}\\ of
 the persistent \\clos\\ instance \\funarg{the-object}\\ of class
 \\funarg{the-class}\\ is bound.
\\Seealsolabel
 \\Fcite{persistent-slot-boundp-using-class};
 \\fcite{slot-boundp-using-class (standard-class t %
effective-slot-description)}."

  (declare (inline load-slot-boundp-using-class ))
  (with-transaction (p-heap)
    (multiple-value-bind (p-objid-vector slot-location expecting-type-tag)
	(get-instance-vector-and-location
	 the-class the-object slot-description)
      (not (eql (p-marker *default-persistent-heap*
			  p-objid-vector slot-location expecting-type-tag)
		+unbound-type-tag+)))))

;;; ---------------------------------------------------------------------------
(defun persistent-slot-boundp-using-class
     (the-class p-objid slot-name next-method
		&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{slot-name}}
      {a symbol naming an effective slot of \\funarg{the-class}}
\\Purposelabel
 Check if the slot named \\funarg{slot-name}\\ of the persistent
 \\clos\\ instance referenced by \\funarg{p-objid}\\ of class
 \\funarg{the-class}\\ is bound.
\\Seealsolabel
 \\Fcite{load-slot-boundp-using-class};
 %% \\fcite{slot-boundp-using-class (built-in-class integer t)};
 \\fcite{slot-boundp-using-class (structure-class persistent-object t)}."

  (declare (inline persistent-slot-boundp-using-class))
  (multiple-value-bind (real-class slot-description)
      (find-effective-slot-description-by-objid slot-name p-objid)
    (if (eq (slot-description-extent slot-description) :transient)
	(if (and (eq real-class the-class) next-method)
	  (funcall next-method)
	  (error +cant-load-transient-slot-error-prompt+
		 slot-name p-objid the-class))
      (load-slot-boundp-using-class real-class p-objid slot-description
                                    p-heap))))

;;; ---------------------------------------------------------------------------
#|
(defmethod slot-boundp-using-class
     ((the-class built-in-class) (p-objid integer) slot-name)
  #+:lisp-doc "A method for low-level slot boundp:
 The persistent \\clos\\ instance is referenced directly by the
 numeric \\funarg{p-objid}.
\\Remarkslabel
 It was no good idea to specify a method on
 \\class{built-in-class}\\ \\class{integer}, since this crashed
 \\allegro"
  (persistent-slot-boundp-using-class the-class p-objid slot-name
				      #'call-next-method))
|#

;;; ---------------------------------------------------------------------------
(defmethod slot-boundp-using-class
     ((the-class structure-class) (p-objid persistent-object) slot-name)
  #+:lisp-doc "A method for low-level slot boundp:
 The persistent \\clos\\ instance is referenced by its
 \\objid\\ contained in \\funarg{p-objid}."
  (persistent-slot-boundp-using-class the-class p-objid slot-name
				      #'call-next-method))

;;; ---------------------------------------------------------------------------
(defmethod slot-boundp-using-class
     ((the-class persistent-metaclass) object (slot-name symbol))
  #+:lisp-doc "Trapped to
 \\fcite{slot-boundp-using-class (standard-class t %
effective-slot-description)}."
  (if *transient-slot-value*
      (call-next-method)
    (let ((effective-slot-description
           (find-effective-slot-description slot-name the-class)))
      (if effective-slot-description
          (slot-boundp-using-class the-class object effective-slot-description)
        (call-next-method)))))

;;; ---------------------------------------------------------------------------
(defmethod slot-boundp-using-class
     ((the-class standard-class) object
      (slot-description effective-slot-description))
  (if *transient-slot-value*
      (call-next-method)
    (if (eq (slot-description-extent slot-description) :persistent)
        (load-slot-boundp-using-class the-class object
				      slot-description)
      (let ((*transient-slot-value* t))
        (call-next-method the-class object
		          (slot-definition-name slot-description))))))

;;; ---------------------------------------------------------------------------
#+:allegro
(defmethod slot-boundp-using-class
     ((the-class persistent-metaclass) object
      (slot-description effective-slot-description))
  #+:lisp-doc "
\\Remarkslabel
  In \\allegro, this method specialized to
  \\fcite{persistent-metaclass}\\ must be defined; the method
  \\fcite{slot-boundp-using-class (standard-class t %
effective-slot-description)}\\ signals a Bus error."
  (if (eq (slot-description-extent slot-description) :persistent)
      (load-slot-boundp-using-class the-class object
				    slot-description)
    (call-next-method the-class object
		      (slot-definition-name slot-description))))

;;; ---------------------------------------------------------------------------
(defmethod slot-boundp-using-class
    ((the-class persistent-metaclass) object
     (slot-definition persistent-effective-slot-definition))
  (if *transient-slot-value*
      (call-next-method)
    (let ((slot-description
	   (persistent-slot-definition-description slot-definition)))
      (unless slot-description
	      (setf slot-description (find-effective-slot-description
				      slot-definition the-class))
	      (setf (persistent-slot-definition-description slot-definition)
		    slot-description))
      (if (and slot-description
	       (eq (slot-description-extent slot-description) :persistent))
	  (load-slot-boundp-using-class the-class object
					slot-description)
	(call-next-method the-class object
			  (slot-definition-name slot-description))))))

;;; ---------------------------------------------------------------------------
;;; makunbound
;;; ---------------------------------------------------------------------------
(defun store-slot-unbound-using-class 
     (the-class the-object slot-description
      call-next-method
      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
  Workhorse for \\textbf{slot-makunbound-using-class}.
"
  (declare (inline store-slot-unbound-using-class))
  (let* ((real-class-p (eq (class-of the-object) the-class))
	 (slot-extent (slot-description-extent slot-description))
	 (slot-write-through-p (slot-write-through-extent-p slot-extent))
	 (old-objid nil) (old-type-tag nil)
	 (index (slot-description-index slot-description)))
    #-:lispworks4 ;; and hopefully not later
    (declare (dynamic-extent real-class-p slot-extent slot-write-through-p
			     old-objid old-type-tag index))
    (with-transaction (p-heap)
      (cond
       (slot-write-through-p
	(multiple-value-bind
	    (p-objid-vector slot-location expecting-type-tag slot-allocation)
	    (get-instance-vector-and-location
	     the-class the-object slot-description)
	  (when index
	    (multiple-value-setq (old-objid old-objid old-type-tag)
	      (p-objid-to-t-slot-in-transaction
	       p-objid-vector slot-location :objid p-heap nil
	       expecting-type-tag)))
	  (when (and *verbose* (>= *verbose* 7))
	    (format t "; Making slot ~A of ~A unbound.~%"
		    (slot-definition-name slot-description)
		    the-object))
	  (setf (p-marker p-heap p-objid-vector
			  slot-location expecting-type-tag)
	    +unbound-type-tag+)
	  (when (eq slot-allocation :class)
	    ;; For LispWorks, the transient value of a
	    ;; :class slot is set too:
	    #+:lispworks
	    (progn
	      (let ((*transient-slot-value* t))
	        (funcall call-next-method the-class the-object
		         (slot-definition-name slot-description)))
	      (setf slot-write-through-p nil)))))
       (real-class-p
	(setf slot-write-through-p t))
       (t
	(error +cant-makunbound-transient-slot-error-prompt+
	       slot-description the-object the-class)))
      (when (and old-objid index)
	;; Remove the old entry from the index:
	(remindex-by-tag old-objid old-type-tag index p-heap)))
    (when (and slot-write-through-p real-class-p
	       (member slot-extent +plob-slot-represented-extents+))
      (let ((*transient-slot-value* t))
        (funcall call-next-method
	         the-class the-object
                 (slot-definition-name slot-description)))))
  the-object)

;;; ---------------------------------------------------------------------------
#|
(defmethod slot-makunbound-using-class
     ((the-class built-in-class) (p-objid integer) slot-name)
  #+:lisp-doc "Trapped to
 \\fcite{slot-makunbound-using-class (standard-class t %
effective-slot-description)}.
\\Remarkslabel
 It was no good idea to specify a method on
 \\class{built-in-class}\\ \\class{integer}, since this crashed
 \\allegro"
  (multiple-value-bind (real-class slot-description)
      (find-effective-slot-description-by-objid slot-name p-objid)
    (slot-makunbound-using-class real-class p-objid slot-description)))
|#

;;; ---------------------------------------------------------------------------
(defmethod slot-makunbound-using-class
     ((the-class structure-class) (p-objid persistent-object)
      (slot-name symbol))
  #+:lisp-doc "Trapped to
 \\fcite{slot-makunbound-using-class (structure-class t %
structure-slot-description)}."
  (multiple-value-bind (real-class slot-description)
      (find-effective-slot-description-by-objid slot-name p-objid)
    (slot-makunbound-using-class real-class p-objid slot-description)))

;;; ---------------------------------------------------------------------------
(defmethod slot-makunbound-using-class
     ((the-class persistent-metaclass) object (slot-name symbol))
  #+:lisp-doc "Trapped to
 \\fcite{slot-makunbound-using-class (standard-class t %
effective-slot-description)}."
  (let ((effective-slot-description
         (find-effective-slot-description slot-name the-class)))
    (if effective-slot-description
        (slot-makunbound-using-class the-class object
                                     effective-slot-description)
      (call-next-method))))

;;; ---------------------------------------------------------------------------
(defmethod slot-makunbound-using-class
     ((the-class structure-class) object
      (slot-description structure-slot-description))
  (with-transaction (*default-persistent-heap*)
    (cond
     ((not (eq (slot-description-extent slot-description) :transient))
      (when (persistent-object-objid object)
	(multiple-value-bind (p-objid-vector slot-location expecting-type-tag)
	    (get-instance-vector-and-location
	     the-class object slot-description)
	  (setf (p-marker *default-persistent-heap*
			  p-objid-vector slot-location expecting-type-tag)
	    +unbound-type-tag+))))
     ((eq the-class (class-of object))
      ;; The above test makes sure that the-class is really the class of
      ;; object; this is important if this method is called by one of
      ;; the (setf slot-value-using-class ...) methods defined above:
      (call-next-method
       the-class object (slot-definition-name slot-description)))
     (t
      (error +cant-makunbound-transient-slot-error-prompt+
	     slot-description object the-class))))
  object)

;;; ---------------------------------------------------------------------------
(defmethod slot-makunbound-using-class
     ((the-class standard-class) object
      (slot-description effective-slot-description))
  (store-slot-unbound-using-class the-class object slot-description
				  #'call-next-method))

;;; ---------------------------------------------------------------------------
#+:allegro
(defmethod slot-makunbound-using-class
     ((the-class persistent-metaclass) object
      (slot-description effective-slot-description))
  #+:lisp-doc "
\\Remarkslabel
  In \\allegro, this method specialized to
  \\fcite{persistent-metaclass}\\ must be defined; the method
  \\fcite{slot-makunbound-using-class (standard-class t effective-slot-description)}\\ signals a Bus error."
  (store-slot-unbound-using-class the-class object slot-description
				  #'call-next-method))

;;; ---------------------------------------------------------------------------
(defmethod slot-makunbound-using-class
    ((the-class persistent-metaclass) object
     (slot-definition persistent-effective-slot-definition))
    (let ((slot-description
	   (persistent-slot-definition-description slot-definition)))
      (unless slot-description
	      (setf slot-description (find-effective-slot-description
				      slot-definition the-class))
	      (setf (persistent-slot-definition-description slot-definition)
		    slot-description))
      (store-slot-unbound-using-class the-class object slot-description
				      #'call-next-method)))

;;; ---------------------------------------------------------------------------
(defun (setf p-slot-value) (new-value instance slot-name
			    &optional (depth *default-depth*)
				      (p-heap *default-persistent-heap*))
  #+:lisp-doc "\\Purposelabel
 Trivial implementation of \\fcite{(setf slot-value)}.
 See \\fcite{p-slot-boundp}\\ on details
 why this function has been defined.
\\Seealsolabel
 \\Fcite{(setf slot-value)}, \\fcite{(setf slot-value-using-class)}."
  (let ((*default-depth* depth)
	(*default-persistent-heap* p-heap))
    (if (integerp slot-name)
	(t-slot-to-p-objid new-value depth p-heap instance slot-name nil)
      (setf (slot-value-using-class (class-of instance) instance slot-name)
	    new-value))))

;;; ---------------------------------------------------------------------------
(defun p-slot-value (instance slot-name
		     &optional (depth *default-depth*)
			       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Trivial implementation of \\fcite{slot-value}.
 See \\fcite{p-slot-boundp}\\ on details
 why this function has been defined.
\\Seealsolabel
 \\Fcite{slot-value}, \\fcite{slot-value-using-class}."
  (let ((*default-depth* depth)
	(*default-persistent-heap* p-heap))
    (if (integerp slot-name)
	(p-objid-to-t-slot instance slot-name depth p-heap nil)
	(slot-value-using-class (class-of instance) instance slot-name))))

;;; ---------------------------------------------------------------------------
(defun p-slot-boundp (instance slot-name
		      &optional (depth *default-depth*)
				(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Trivial implementation of \\fcite{slot-boundp}.  Following
 explanations are valid for \\fcite{(setf p-slot-value)},
 \\fcite{p-slot-value}\\ and \\fcite{p-slot-makunbound}, too.

 For \\lw, these \\textbf{p-slot-\\ldots} functions are not needed,
 since all of them do more or less what has been coded here.

 In \\allegro, all \\textbf{p-slot-\\ldots} functions first search for
 the \\sltmo\\ representing the slot and do only call the
 \\textbf{p-slot-\\ldots-using-class} generic function if a slot was
 found in the class at all; if the slot was not found, an error is
 signalled.\\footnote{\\allegro takes the definition of
 \\fcite{slot-missing}\\ literally, which says that the
 \\fcite{slot-missing}\\ is only called on instances with a class
 being an instance of \\fcite{standard-class}; the standard does not
 say anything about how \\fcite{slot-value}\\ and companions should
 proceed on other instances.  From a practical point of view, it would
 be better to call \\fcite{slot-missing}\\ for all missing slots.}
 Since the implementation of \\allegro's
 \\textbf{slot-\\ldots}\\ functions can't be changed, for
 non-\\clos\\ instances and for instances representing a reference to
 a persistent object, the \\textbf{p-slot-\\ldots} functions must be
 used. This is for example the case for objects returned from a call to
 \\fcite{store-object}.
\\Seealsolabel
 \\Fcite{slot-boundp}, \\fcite{slot-boundp-using-class}."

  (let ((*default-depth* depth)
	(*default-persistent-heap* p-heap))
    (slot-boundp-using-class (class-of instance) instance slot-name)))

;;; ---------------------------------------------------------------------------
(defun p-slot-makunbound (instance slot-name
			  &optional (depth *default-depth*)
				    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Trivial implementation of \\fcite{slot-makunbound}.
 See \\fcite{p-slot-boundp}\\ on details
 why this function has been defined.
\\Seealsolabel
 \\Fcite{slot-makunbound}, \\fcite{slot-makunbound-using-class}."
  (let ((*default-depth* depth)
	(*default-persistent-heap* p-heap))
    (slot-makunbound-using-class (class-of instance) instance slot-name)))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
