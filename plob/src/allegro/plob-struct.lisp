;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-struct.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	7.2.94
;;;; Description	PLOB allocate and accessor functions for
;;;;		LISP structures
;;;; The bibliography references used are:
;;;; [St90]	Guy L. Steele Jr.:
;;;;		Common LISP
;;;;		The Language
;;;;		Second Edition
;;;;		Digital Press, Bedford, Massachusetts, 1990
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
;;; Structure
;;; ---------------------------------------------------------------------------

(defun p-allocate-structure
     (structure-description &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{structure-description}}
      {a structure description}
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 structure described by \\funarg{structure-description}\\ allocated
 in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  ;; (assert-open-session-p p-heap)
  (sh-create-structure (persistent-object-objid p-heap)
		       (persistent-object-objid structure-description)))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-descr)
     (t-descr p-objid &optional (depth *default-depth*)
	      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a structure-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-descr}
 of the persistent structure referenced by
 \\funarg{p-objid}\\ to \\funarg{t-descr}.

 This slot defines the class of the persistent structure
 instance referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-structure-descr};
 \\fcite{persistent-structure}."

  (t-slot-to-p-objid t-descr depth p-heap p-objid
		     +structure-location-description+
		     #(setf persistent-structure-p-descr)
		      +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-descr
     (p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-descr}
 of the persistent structure referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-structure-descr)};
 \\fcite{persistent-structure}."

  (p-objid-to-t-slot p-objid +structure-location-description+ depth
		     p-heap nil +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-structure-slot-ref)
     (t-slot-value p-objid at-location &optional (depth *default-depth*)
		   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-slot-value}}
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
\\Valueslabel
 \\retarg{\\funarg{t-slot-value}}
\\Purposelabel
 Write to a persistent structure object's component; the
 slot of the persistent structure object referenced by
 \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
 set to \\funarg{t-slot-value}.
\\Seealsolabel
 \\Fcite{p-structure-slot-ref};
 \\fcite{persistent-structure}."

  (t-slot-to-p-objid t-slot-value depth p-heap p-objid at-location nil
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-structure-slot-ref
     (p-objid at-location &optional (depth *default-depth*)
              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
\\Purposelabel
 Read a persistent structure object's component; the
 slot value of the persistent structure object referenced by
 \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
 returned in its transient representation.
\\Seealsolabel
 \\Fcite{(setf p-structure-slot-ref)};
 \\fcite{persistent-structure}."

  (p-objid-to-t-slot p-objid at-location depth p-heap nil
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun store-structure-in-transaction
    (t-structure p-objid p-struct-descr depth p-heap)
  #+:lisp-doc "Store \\funarg{t-structure}\\ to the Stable Heap."
  (let ((number-of-persistent-slots
	 (class-description-persistent-slot-numbers p-struct-descr)))
    (when (> number-of-persistent-slots 0)
      (let ((slots (class-slots p-struct-descr))
            (number-of-slots (p-objid-size p-objid p-heap))
            (name nil))
        (with-objid-buffer
	    (objid-structure-buffer p-objid number-of-persistent-slots p-heap)
	  (map
	   nil
	   #'(lambda (s)
	       (let* ((slot-name (slot-definition-name s))
		      (slot-location (slot-definition-location s))
		      (slot-extent (slot-description-extent s))
		      (slot-deferred (slot-description-deferred s))
		      (transient-slot-p (eq slot-extent :transient))
		      (slot-reader (structure-slot-description-p-reader s)))
		 (cond
		  ((eq slot-name 'objid)
		   (unless transient-slot-p
		     (write-objid-buffer objid-structure-buffer
					 slot-location
					 p-objid
					 +short-objid-tag+))
                   (unless name
                     (setf name (class-name (class-of t-structure))))
		   (when (subtypep name 'persistent-object)
		     (setf (persistent-object-objid t-structure)
			   p-objid)))
		  ((eq slot-name 'p-descr)
		   (unless transient-slot-p
		     (write-objid-buffer objid-structure-buffer
					 slot-location
					 (structure-description-objid
					  p-struct-descr)
					 +short-objid-tag+))
                   (unless name
                     (setf name (class-name (class-of t-structure))))
		   (when (subtypep name 'persistent-structure)
		     (setf (persistent-structure-p-descr t-structure)
			   p-struct-descr)))
		  ((or (null slot-reader) transient-slot-p)
		   nil)
		  ((and (> slot-location number-of-slots)
			*verbose* (>= *verbose* 1))
		   (warn "Cannot store slot ~A of ~A."
			 s (make-persistent-object
			    (persistent-object-objid p-objid))))
		  (t
		   (multiple-value-bind (sub-objid sub-type-tag)
		       (t-object-to-p-objid (funcall slot-reader t-structure)
					    depth p-heap)
		     (write-objid-buffer objid-structure-buffer
					 slot-location
					 sub-objid
					 sub-type-tag))))))
	   slots)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun store-structure (t-structure p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-structure}}
      {a structure object}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-structure}}
\\Purposelabel
 Store the transient structure object in \\funarg{t-structure}\\ to
 the persistent structure object referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-structure}."

  (with-transaction (p-heap)
    (let* ((class-of-t-struct (class-of t-structure))
	   (name (class-name class-of-t-struct))
	   (p-struct-descr (ensure-structure-description name))
	   (number-of-persistent-slots
	    (class-description-persistent-slot-numbers p-struct-descr))
	   (to-store nil) (force-write nil))
      (unless p-objid
	(setf p-objid (persistent-object-objid t-structure)))
      ;; Do not allocate persistent storage for objects with no
      ;; persistent slots:
      (when (> number-of-persistent-slots 0)
	(unless p-objid
	  (setf p-objid (p-allocate-structure p-struct-descr p-heap))
	  (setf force-write t))
	(with-write-lock
	    (p-heap p-objid t-structure
		    (if (structure-description-p-dependent p-struct-descr)
			:nocache
		      depth)
		    +structure-type-tag+ force-write)
	  ;; Now the persistent object has a valid reference to its
	  ;; structure-description. Store the persistent slots:
	  (setf to-store t)
	  (store-structure-in-transaction t-structure p-objid
					  p-struct-descr depth p-heap)))
      (when (and *verbose* (>= *verbose* 5) to-store)
	(format t ";; Stored structure~%;; ~A~%" t-structure))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-structure)
     (t-structure &optional p-objid (depth *default-depth*)
			    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-structure}}
      {a structure object}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-structure}}
\\Purposelabel
 Store the transient structure object in \\funarg{t-structure}\\ to
 the persistent structure object referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-structure}."

  (values t-structure (store-structure t-structure p-objid depth p-heap)))
  
;;; ---------------------------------------------------------------------------
(defun load-instance-for-structure
    (struct-descr p-objid depth p-heap
     &optional t-into-object get-slot-description)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{struct-descr}}
      {a structure-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient structure instance representing the persistent
 structure object referenced by \\funarg{p-objid}.

 This function corresponds roughly to the
 \\fcite{make-instance (standard-class)}.
\\Seealsolabel
 \\Fcite{load-instance-for-redefined-structure};
 \\fcite{make-instance (standard-class)}."

  (flet ((structure-slot-sub-object
          (objid-structure-buffer at-location)
          (multiple-value-bind (p-slot-objid p-slot-type-tag)
              (read-objid-buffer objid-structure-buffer at-location)
            (if (p-immediatep p-slot-type-tag)
		(values (p-objid-to-t-object p-slot-objid p-slot-type-tag
					     :immediate p-heap)
			p-slot-type-tag
			:immediate)
              (multiple-value-bind (t-slot-object t-slot-object-p)
		  (is-registered-objid p-slot-objid)
	        (if t-slot-object-p
	            (values t-slot-object p-slot-type-tag :immediate)
	          (values p-slot-objid p-slot-type-tag
			  :cached-demand-load)))))))

    (let ((the-class (find-class (class-name struct-descr)))
          (numeric-objid (persistent-object-objid p-objid))
	  (slots (class-slots struct-descr))
          (number-of-persistent-slots
	   (class-description-persistent-slot-numbers struct-descr))
	  (create-argument-list nil)
	  (t-set-slot-list nil) (p-set-slot-list nil))
      (when (> number-of-persistent-slots 0)
	(with-objid-buffer
	    (objid-structure-buffer
	     p-objid number-of-persistent-slots p-heap)
          (loop for i
	    from (1- (class-description-slot-numbers struct-descr))
	    downto 0
	    do
	    (let* ((slot-descr (svref slots i))
		   (user-slot-descr
		    (if get-slot-description
			(funcall get-slot-description slot-descr struct-descr)
		      slot-descr)))
	      (cond
	       (user-slot-descr
	        (let* ((slot-name (slot-definition-name slot-descr))
                       (slot-initarg (slot-definition-initargs slot-descr))
		       (slot-reader
			(structure-slot-reader the-class slot-name))
		       (slot-extent
			(slot-description-extent slot-descr))
		       (slot-deferred
			(slot-description-deferred slot-descr)))
                  (unless slot-initarg
                    (setf slot-initarg (intern slot-name :keyword)))
		  (unless slot-reader
		    (setf slot-reader
		      (structure-slot-description-p-reader slot-descr)))
		  (cond
		   ((null slot-reader) nil)
		   ((eq slot-name 'objid)
		    (push numeric-objid create-argument-list)
		    (push slot-initarg create-argument-list))
		   ((eq slot-name 'p-descr)
		    (push struct-descr create-argument-list)
		    (push slot-initarg create-argument-list))
		   ((not (eq slot-extent :transient))
		    (multiple-value-bind (t-slot-object p-slot-type-tag loaded)
		        (structure-slot-sub-object objid-structure-buffer
				                   (slot-definition-location
					            user-slot-descr))
		      (if t-into-object
			  ;; If an object already exists, it should be
			  ;; re-read; push all slots onto the
			  ;; set-slot-list:
			  (ecase loaded
			    (:immediate
			     (push (cons slot-descr t-slot-object)
				   t-set-slot-list))
			    (:cached-demand-load
			     (push (cons slot-descr t-slot-object)
				   p-set-slot-list)))
			(ecase loaded
			  (:immediate
			   (push t-slot-object create-argument-list)
			   (push slot-initarg create-argument-list))
			  (:cached-demand-load
			   (if (eq slot-extent :cached-demand-load)
			       (progn
				 (push (make-slot-load-on-demand
					t-slot-object p-slot-type-tag)
				       create-argument-list)
				 (push slot-initarg create-argument-list))
			     (push (cons slot-descr t-slot-object)
				   p-set-slot-list))))))))))
	       ((and *verbose* (>= *verbose* 1))
		(warn "Slot ~A not loadable for ~A."
		      slot-descr
		      (make-persistent-object numeric-objid))))))))
      (unless t-into-object
	(setf t-into-object
	  (apply (structure-description-p-constructor struct-descr)
		 create-argument-list))
	(unless (structure-description-p-dependent struct-descr)
	  (register-to-cache numeric-objid t-into-object)))
      (dolist (s t-set-slot-list)
	(let* ((slot-descr (car s))
	       (t-slot-value (cdr s)))
	  (call-structure-slot-description-writer
	   struct-descr slot-descr
	   p-heap t-into-object
	   t-slot-value)))
      (dolist (s p-set-slot-list)
	(let* ((slot-descr (car s))
	       (p-slot-objid (cdr s)))
	  (call-structure-slot-description-writer
	   struct-descr slot-descr
	   p-heap t-into-object
	   (p-objid-to-t-object
	    p-slot-objid (p-type-tag-of p-slot-objid p-heap) depth p-heap))))))
  t-into-object)

;;; ---------------------------------------------------------------------------
(defstruct (migrate-info
            (:constructor make-migrate-info (location value type)))
  #+:lisp-doc "
\\Purposelabel
 A structure used at schema evolution for migrating an 'old' to a
 'new' persistent structure object;
 used in \\fcite{load-instance-for-redefined-structure}.
 For each slot to migrate, an instance of \\class{migrate-info}\\ is
 created."

  (location -1
	    :type fixnum
	    #+:lisp-doc :documentation #+:lisp-doc "
 The new location of the slot's value in the object.")

  (value +null-objid+
	 :type fixnum
	 #+:lisp-doc :documentation #+:lisp-doc "
 Either the immediate value or the \\objid\\ of the slot's value.")

  (type +short-objid-tag+
	:type fixnum
	#+:lisp-doc :documentation #+:lisp-doc "
 The \\typetag\\ of the slot's value."))

;;; ---------------------------------------------------------------------------
(defun find-slot-description (slot-descr struct-descr)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{slot-descr}}
      {a slot description of a slot of the structure class described by
       \\funarg{struct-descr}}
 \\isa{\\funarg{struct-descr}}
      {a structure-description}
\\Purposelabel
 Searches the effective-slot-description of a slot having the same name
 as the slot described by \\funarg{slot-descr}\\ in
 \\funarg{struct-descr}.
\\Seealsolabel
 \\Fcite{find-effective-slot-description};
 \\fcite{find-effective-slot-description-by-objid};
 \\Fcite{find-effective-slot}."

  (find (slot-definition-name slot-descr)
	(class-slots struct-descr)
	:test #'eq
	:key #'slot-definition-name))

;;; ---------------------------------------------------------------------------
(defun load-instance-for-redefined-structure
    (old-struct-descr new-struct-descr p-objid depth p-heap
     &optional t-into-object)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{old-struct-descr}\\ resp.\\ \\funarg{new-struct-descr}}
      {a structure-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient structure instance representing the persistent
 structure object referenced by \\funarg{p-objid}\\ whose
 structure-description was redefined.

 The persistent structure object referenced by \\funarg{p-objid}\\ is
 migrated (or `schema-evoluted') from its current
 \\funarg{old-struct-descr}\\ (found in the persistent structure object's
 slot {\\bf p-descr}) to the global actual
 \\funarg{new-struct-descr}\\ (as now contained in the class table,
 i.e.\\ the structure-description as returned by
 \\fcite{p-find-class}). This migration includes updating the 
 description reference of the structure object to the global
 actual structure description.
\\Remarkslabel
 A `full' schema evolution from \\funarg{old-struct-descr}\\ to
 \\funarg{new-struct-descr}\\ is only possible if the number of
 slots was not increased; otherwise, the persistent structure
 object's description of \\funarg{p-objid}\\ is not changed.
 In this case the returned transient structure object contains
 the additional slots initalized to their slot default initializations.

 For redefining \\cl\\ structures created by
 \\lisp{defstruct}\\ see also
 \\stcite{473, X3J13 vote of January 1989 \\lt{}56\\gt}.
\\Seealsolabel
 \\Fcite{load-instance-for-structure};
 \\fcite{(setf schema-evolution)}."

  (labels ((set-evolution
            (struct-descr new-evolution)
	    (setf (class-description-schema-evolution struct-descr)
                  new-evolution)
            (setf (p-structure-description-schema-evolution struct-descr)
                  new-evolution)
            new-evolution)

           (migrate-object
            (old-struct-descr new-struct-descr p-objid p-heap)
	    (with-write-lock
		(p-heap p-objid nil :nocache +structure-type-tag+ t)
	     (let* ((number-of-old-slots (p-objid-size p-objid p-heap))
	            (number-of-new-slots
	             (class-description-persistent-slot-numbers
                      new-struct-descr))
                    (new-sh-vector (make-array number-of-new-slots))
		    (number-of-copied-slots 0))
	       ;; Scan through the object looking for slots also found in
	       ;; the new structure description:
               (with-objid-buffer
                   (objid-buffer p-objid number-of-old-slots p-heap)
	         (map
                  nil
		  #'(lambda (old-slot-descr)
		      (let ((new-slot-descr (find-slot-description
					     old-slot-descr
					     new-struct-descr)))
		        (cond
		         ((and new-slot-descr
			       (not (eq (slot-description-extent
				         new-slot-descr)
				        :transient)))
			  ;; Found 'old' slot in 'new'
			  ;; structure description and
			  ;; the 'new' slot is not :transient:
			  (multiple-value-bind (value type)
			      (read-objid-buffer objid-buffer
				                 (slot-definition-location
				                  old-slot-descr))
			    (setf (svref new-sh-vector
				         number-of-copied-slots)
				  (make-migrate-info
				   (slot-definition-location
				    new-slot-descr)
				   value
				   (if (p-immediatep type)
				       type
				     +short-objid-tag+))))
			  (incf number-of-copied-slots))
		         ((and *verbose* (>= *verbose* 1))
			  (warn "Slot ~A will be lost for ~A."
			        old-slot-descr
			        (make-persistent-object
			         (persistent-object-objid
				  p-objid)))))))
		  (class-slots old-struct-descr))
                 ;; Make all components of the sh-vector unbound:
	         (loop for i from 0 below number-of-old-slots
		       do
                       (write-objid-buffer objid-buffer i
			                   +unbound-type-tag+
			                   +unbound-type-tag+))
	         ;; Copy the found slots back into the persistent object:
	         (map nil
		      #'(lambda (info)
                          (when info
                            (write-objid-buffer objid-buffer
				                (migrate-info-location info)
			                        (migrate-info-value info)
				                (migrate-info-type  info))))
		      new-sh-vector))
	       ;; Change the structure description of the object:
	       (setf (p-structure-descr p-objid :objid p-heap)
		     (structure-description-objid new-struct-descr))))
            p-objid))

    (if (class-description-equal-p old-struct-descr new-struct-descr)
        (progn
          ;; That was easy, said man: the structure descriptions are equal,
          ;; so 'upgrade' the structure description of p-objid:
          (setf (p-structure-descr p-objid :objid p-heap)
                (structure-description-objid new-struct-descr))
          (values (load-instance-for-structure
		   new-struct-descr p-objid depth p-heap t-into-object)
                  p-objid))
      (let ((evolution
             (class-description-schema-evolution old-struct-descr))
	    (number-of-old-slots (p-objid-size p-objid p-heap))
	    (number-of-new-slots
	     (class-description-persistent-slot-numbers new-struct-descr)))
	(unless evolution
	  (setf evolution *default-structure-schema-evolution*)
	  (set-evolution old-struct-descr evolution))
        (if (>= number-of-old-slots number-of-new-slots)
	    ;; Do always a (destructive) write-back schema evolution:
	  (progn
	    (migrate-object old-struct-descr new-struct-descr
			    p-objid p-heap)
	    ;; Load the object:
	    (values (load-instance-for-structure
		     new-struct-descr p-objid depth p-heap t-into-object)
		    p-objid))
          (progn
            (when (eq evolution :write-back)
	      (when (and *verbose* (>= *verbose* 1))
		(cerror (format nil "Use schema evolution ~A instead."
				*default-structure-schema-evolution*)
			"Schema evolution ~A is not possible ~
		       for evolution from ~A to ~A because ~
		       there are missing ~D persistent slots."
			evolution old-struct-descr new-struct-descr
			(- number-of-new-slots number-of-old-slots)))
	      (setf evolution *default-structure-schema-evolution*)
              (set-evolution old-struct-descr evolution))
	    ;; Load the object:
	    (values (load-instance-for-structure
		     new-struct-descr p-objid depth p-heap t-into-object
		     #'(lambda (slot-descr struct-descr)
                         (declare (ignore struct-descr))
			 (find-slot-description slot-descr old-struct-descr)))
		    p-objid)))))))

;;; ---------------------------------------------------------------------------
(defun p-structure (p-objid
                    &optional (depth *default-depth*)
                    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 structure object
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-structure)}."

  (let* ((t-object (is-registered-objid p-objid))
	 (p-struct-descr-old
	  (p-structure-description
	   (p-structure-descr p-objid :objid p-heap) :cached p-heap))
	 (p-struct-descr-new
	  (ensure-structure-description (class-name p-struct-descr-old)))
	 (from-store nil))
    (with-transaction (p-heap)
      (with-read-lock (p-heap p-objid depth +structure-type-tag+
			      (null t-object))
	(setf from-store t)
	(let ((t-class-name (class-name p-struct-descr-old)))
	  (if (eq p-struct-descr-new p-struct-descr-old)
	      (setf t-object
		(load-instance-for-class
		 t-class-name p-struct-descr-old
		 p-objid depth p-heap t-object))
	    (progn
	      (unregister-by-objid p-objid)
	      (multiple-value-setq (t-object p-objid)
		(load-instance-for-redefined-class
		 t-class-name p-struct-descr-old p-struct-descr-new
		 p-objid depth p-heap t-object)))))))
    (when (and *verbose* (>= *verbose* 5) from-store)
      (format t ";; Loaded structure ~%;; ~A~%" t-object))
    t-object))

;;; ---------------------------------------------------------------------------
;;; Storing of structures
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid
    ((t-object persistent-immediate-object) depth p-heap)
  #+:lisp-doc "Return the \\objid\\ stored within \\funarg{t-object}.
 Since \\funarg{t-object}\\ is a reference to a non-loaded persistent
 object, its state is never stored."

  (declare (ignore depth p-heap))
  (values (persistent-immediate-object-objid t-object)
	  (persistent-immediate-object-type-tag t-object)))

(defmethod t-object-to-p-objid
    ((t-object slot-load-on-demand) depth p-heap)
  #+:lisp-doc "Return the \\objid\\ stored within \\funarg{t-object}.
 Since \\funarg{t-object}\\ is a reference to a non-loaded persistent
 object, its state is never stored."

  (declare (ignore depth p-heap))
  (values (slot-load-on-demand-objid t-object)
	  (slot-load-on-demand-type-tag t-object)))

(defmethod t-object-to-p-objid-using-class
     (t-object (t-class structure-class) depth p-heap)
  (store-structure t-object nil depth p-heap))

;;; ---------------------------------------------------------------------------
;;; Loading of structures
;;; ---------------------------------------------------------------------------

(defmethod load-instance-for-class (t-class-name
                                    (p-class-descr structure-description)
                                    p-objid depth p-heap
				    &optional t-into-object)
  (declare (ignore t-class-name))
  (load-instance-for-structure
   p-class-descr p-objid depth p-heap t-into-object))

;;; ---------------------------------------------------------------------------
(defmethod load-instance-for-redefined-class
     (t-class-name
      (p-class-descr-old structure-description)
      (p-class-descr-new structure-description)
      p-objid depth p-heap &optional t-into-object)
  (declare (ignore t-class-name))
  (load-instance-for-redefined-structure
   p-class-descr-old p-class-descr-new p-objid depth p-heap t-into-object))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +structure-type-tag+))
				depth p-heap)
  (p-structure p-objid depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
