;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-clos.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	2.3.94
;;;; Description	PLOB functions for CLOS instances
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
;;; CLOS instance
;;; ---------------------------------------------------------------------------

(defun p-allocate-instance (class-description
                            &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{class-description}}
      {a class description}
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 \\clos\\ instance described by \\funarg{class-description}\\ allocated
 in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  ;; (assert-open-session-p p-heap)
  (sh-create-instance (persistent-object-objid p-heap)
		      (persistent-object-objid class-description)))

;;; ---------------------------------------------------------------------------
(defun p-instancep (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of type
 persistent \\clos\\ instance,
 \\lispnil\\ otherwise. No further class checking is done by this
 function; it is only checked if the persistent object referenced
 by \\funarg{p-objid}\\ is a persistent \\clos\\ instance at all."

  (= (p-type-tag-of p-objid p-heap) +instance-type-tag+))

;;; --- CLOS instance class wrapper -------------------------------------------

(defun (setf p-instance-class-wrapper) (t-class-wrapper
                                        p-objid
                                        &optional (depth *default-depth*)
                                        (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a class-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the class-wrapper of the persistent \\clos\\ instance
 referenced by \\funarg{p-objid}\\ to \\funarg{t-class-wrapper}.

 The class-wrapper defines the class of the persistent
 \\clos\\ instance referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-instance-class-wrapper}."

  (t-slot-to-p-objid t-class-wrapper depth p-heap p-objid
		     +clos-location-class-wrapper+ nil +instance-type-tag+
		     +null-objid+
		     #'(lambda (sub-objid p-heap p-objid at-index
				expecting-type-tag expecting-class
				sub-type-tag)
			 (declare (ignore at-index expecting-type-tag
					  expecting-class sub-type-tag))
			 (sh-write-instance-wrapper
			  (persistent-object-objid p-heap)
			  (persistent-object-objid p-objid)
			  sub-objid))))

;;; ---------------------------------------------------------------------------
(defun p-instance-class-wrapper (p-objid
                                 &optional (depth *default-depth*)
	                         (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the class-wrapper of the persistent \\clos\\ instance
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-instance-class-wrapper)}."

  (p-objid-to-t-slot p-objid +clos-location-class-wrapper+
		     depth p-heap nil +instance-type-tag+))

;;; --- CLOS instance data vector ---------------------------------------------

(defun (setf p-instance-data-vector) (t-data-vector
				      p-objid
				      &optional (depth *default-depth*)
				      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-data-vector}}
      {a vector}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the data-vector of the persistent \\clos\\ instance
 referenced by \\funarg{p-objid}\\ to \\funarg{t-data-vector}.

 The data-vector holds the state of the persistent
 \\clos\\ instance referenced by \\funarg{p-objid}; its length
 is the number of persistent slots of the persistent
 \\clos\\ instance. The number of persistent slots is the value
 of the slot {\\bf p-persistent-slot-numbers} of the persistent
 \\clos\\ instance's class-description (see
 \\fcite{class-description}).
\\Seealsolabel
 \\Fcite{p-instance-data-vector}."

  (t-slot-to-p-objid t-data-vector depth p-heap p-objid
		     +clos-location-data-vector+ nil +instance-type-tag+
		     +null-objid+
		     #'(lambda (sub-objid p-heap p-objid at-index
				expecting-type-tag expecting-class
				sub-type-tag)
			 (declare (ignore at-index expecting-type-tag
					  expecting-class sub-type-tag))
			 (sh-write-instance-data
			  (persistent-object-objid p-heap)
			  (persistent-object-objid p-objid)
			  sub-objid))))

;;; ---------------------------------------------------------------------------
(defun p-instance-data-vector (p-objid
			       &optional (depth *default-depth*)
			       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the data-vector of the persistent \\clos\\ instance
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-instance-data-vector}."

  (p-objid-to-t-slot p-objid +clos-location-data-vector+
		     depth p-heap nil +instance-type-tag+))

;;; ---------------------------------------------------------------------------
(defmethod (setf persistent-object-objid)
     (new-objid (the-object standard-object))
  #+:lisp-doc "Does nothing."
  new-objid)

;;; ---------------------------------------------------------------------------
(defmethod persistent-object-objid ((the-object standard-object))
  #+:lisp-doc "Returns always \\lispnil."
  nil)

;;; ---------------------------------------------------------------------------
(defconstant +plob-slot-represented-extents+
    '(:transient :cached :cached-write-through :object :objid)
  #+:lisp-doc "\\Purposelabel
 List with slot extents of persistent objects which are represented
 in transient instances. The elements in
 \lisp{+plob-slot-represented-extents+}\\ must be a
 subset of the values of the list in \\fcite{+plob-slot-extents+}.
\\Seealsolabel
 \\Fcite{+plob-slot-extents+};
 \\fcite{+plob-slot-write-through-extents+}.")

;;; ---------------------------------------------------------------------------
(defconstant +cant-set-slot-to-immediate+
    "Cannot set slot ~A of ~A to immediate objid ~A"
  #+:lisp-doc "Prompt in warnings for slots with \\lisp{:extent}\\ \\lisp{:objid},
 for which a non-immediate object is loaded. A non-immediate object
 cannot be represented as a short \\objid.")

;;; ---------------------------------------------------------------------------
(defun store-instance (t-instance p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isaclosobject{\\funarg{t-instance}}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-instance}}
\\Purposelabel
 Store the transient \\clos\\ instance in \\funarg{t-instance}\\ to
 the persistent \\clos\\ instance referenced by \\funarg{p-objid}.

 Only the slots with \\lisp{:extent :cached}\\ are stored here;
 the slots with other extents are stored by calls to the
 \\fcite{(setf slot-value-using-class) (t standard-class t effective-slot-description)}.
\\Seealsolabel
 \\Fcite{p-instance}."

  (with-transaction (p-heap)
    (let* ((class-of-t-inst (class-of t-instance))
	   (t-class-name (class-name class-of-t-inst))
	   (newest-generation (ensure-class-description t-class-name))
	   (number-of-persistent-slots
	    (class-description-persistent-slot-numbers newest-generation))
	   (to-store nil) (force-write nil))
      (unless p-objid
	(setf p-objid (persistent-object-objid t-instance)))
      ;; Do not allocate persistent storage for objects with no persistent
      ;; slots:
      (when (> number-of-persistent-slots 0)
	(unless p-objid
	  (setf p-objid (p-allocate-instance newest-generation p-heap))
	  (setf (persistent-object-objid t-instance) p-objid)
	  (setf force-write t))
	(with-write-lock
	    (p-heap p-objid t-instance
		    (if (class-description-dependent newest-generation)
			:nocache
		      depth)
		    +instance-type-tag+ force-write)
	  (setf to-store t)
	  (let ((p-class-descr
		 (p-class-description
		  (p-instance-class-wrapper p-objid :objid p-heap)
		  :cached p-heap)))
	    (unless (eq newest-generation p-class-descr)
	      (update-instance-for-redefined-description
	       p-class-descr newest-generation p-objid p-heap))
	    (let ((slots (class-description-effective-slots newest-generation))
		  (p-objid-instance-vector
		   (p-instance-data-vector p-objid :objid p-heap))
		  (*transient-slot-value* t))
	      ;; Now the persistent object has a valid reference to its
	      ;; class-description. Store the persistent slots.
	      ;; (write-lock ...) no longer necessary, since the instance
	      ;; vector is write-locked by the referencing persistent
	      ;; CLOS instance
	      ;; (write-lock p-heap p-objid-instance-vector nil :nocache
	      ;;             +vector-type-tag+)
	      (with-objid-buffer
		  (objid-instance-buffer p-objid-instance-vector
					 number-of-persistent-slots p-heap)
		(map
		    nil
		  #'(lambda (s)
		      (let* ((slot-name (slot-definition-name s))
			     (slot-allocation (slot-definition-allocation s))
			     (slot-extent (slot-description-extent s))
			     (slot-deferred (slot-description-deferred s))
			     (sub-objid +unbound-type-tag+)
			     (sub-type-tag +unbound-type-tag+))
			#-:lispworks4 ;; and hopefully not later
			(declare (dynamic-extent slot-name slot-allocation
						 slot-extent slot-deferred
						 sub-objid sub-type-tag))
			(when (eq slot-name 'objid)
			  (setf (persistent-object-objid t-instance) p-objid))
			(when (and (not (eq slot-extent :transient))
				   (member slot-extent
					   +plob-slot-represented-extents+))
			  (ecase slot-allocation
			    (:instance
			     (let ((location (slot-description-location s)))
			       (declare (type fixnum location)
					#-:lispworks4
					;; and hopefully not later
					(dynamic-extent location))
			       (when (and *verbose* (>= *verbose* 7))
				 (format t "; Storing slot ~A with depth ~A of~%; ~A~%"
					 slot-name depth t-instance))
			       (when (slot-boundp t-instance slot-name)
				 (multiple-value-setq (sub-objid sub-type-tag)
				   (t-object-to-p-objid 
				    (slot-value t-instance slot-name)
				    depth p-heap)))
			       (write-objid-buffer objid-instance-buffer
						   location
						   sub-objid sub-type-tag)))
			    (:class
			     (let ((p-objid-vector
				    (p-slot-description-location s :objid))
				   (location +cons-location-cdr+))
			       (declare (type fixnum p-objid-vector location)
					#-:lispworks4
					;; and hopefully not later
					(dynamic-extent p-objid-vector
							location))
			       (if (slot-boundp t-instance slot-name)
				   (let ((t-slot-value nil))
				     (declare (ignore t-slot-value))
				     (multiple-value-setq
					 (t-slot-value sub-objid sub-type-tag)
				       (t-slot-to-p-objid-in-transaction
					(slot-value t-instance slot-name)
					depth p-heap p-objid-vector location
					nil +cons-type-tag+))
				     (setf (p-marker
					    p-heap p-objid-vector location)
				       +unbound-type-tag+))))))
			  (cond
			   ((eql sub-type-tag +unbound-type-tag+)
			    nil)
			   ((and (eq slot-extent :objid)
				 (not (p-immediatep sub-type-tag)))
			    ;; Put the numerical objid into the slot:
			    (setf (slot-value t-instance slot-name)
			      sub-objid))
			   ((with-direct-representation-p slot-extent)
			    (setf (slot-value t-instance slot-name)
			      (make-persistent-object
			       sub-objid sub-type-tag)))))))
		  slots))))))
      (when (and *verbose* (>= *verbose* 5) to-store)
	(format t ";; Stored instance~%;; ~A~%" t-instance))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-instance)
     (t-instance &optional p-objid (depth *default-depth*)
		 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isaclosobject{\\funarg{t-instance}}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-instance}}
\\Purposelabel
 Store the transient \\clos\\ instance in \\funarg{t-instance}\\ to
 the persistent \\clos\\ instance referenced by \\funarg{p-objid}.

 Only the slots with \\lisp{:extent :cached}\\ are stored here;
 the slots with other extents are stored by calls to the
 \\fcite{(setf slot-value-using-class) (t standard-class t effective-slot-description)}.
\\Seealsolabel
 \\Fcite{p-instance}."

  (values t-instance (store-instance t-instance p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defmethod load-instance-for-class
     (t-class-name
      (class-descr class-description)
      p-objid depth p-heap &optional t-into-object)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{class-descr}}
      {a class-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient \\clos\\ instance representing the persistent
 \\clos\\ instance referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{load-instance-for-redefined-class}."

  (labels
      ((electric-load-instance-for-class
	   (class-descr p-objid p-objid-instance-vector depth p-heap
	    t-into-object)
	(let* ((*default-depth* depth)
	       (*transient-slot-value* t)
               (the-class (find-class t-class-name))
	       (persistent-metaclass-p
		(subtypep (class-of the-class) +persistent-metaclass-class+))
	       (slots (class-slots class-descr))
	       (number-of-persistent-slots
	        (class-description-persistent-slot-numbers class-descr)))
	  (unless t-into-object
	    (setf t-into-object (if persistent-metaclass-p
				    (make-instance the-class
				                   :objid p-objid
                                                   :p-heap p-heap
				                   :store-cached-slots nil)
				  (make-instance the-class))))
	  (unless (or (class-description-dependent class-descr)
		      (eq (is-registered-object t-into-object) p-objid))
            (register-to-cache p-objid t-into-object))
          (when (> number-of-persistent-slots 0)
	    ;; Count upwards from 0 to last slot will make it possible for
	    ;; class-descriptions to load the class prototype correctly
            ;; (because this is the only slot of a class description that
            ;; contains an object of the class to load; because the prototype
            ;; is the last slot, the class description object is initialized
            ;; so much that it can be used to generate instances):
            (with-objid-buffer
                (objid-instance-buffer p-objid-instance-vector
                                       number-of-persistent-slots p-heap)
	      (loop for i
		    from 0
		    below (class-description-slot-numbers class-descr)
		    do
		    (let* ((slot-descr (svref slots i))
		           (slot-name (slot-definition-name slot-descr))
			   (slot-descr-using-class
			    (if persistent-metaclass-p slot-descr slot-name))
		           (slot-allocation (slot-definition-allocation
                                             slot-descr))
		           (slot-extent (slot-description-extent
				         slot-descr))
		           (slot-deferred (slot-description-deferred
				           slot-descr))
                           (sub-objid +null-objid+)
                           (sub-type-tag +null-type-tag+))
		      (declare (type fixnum p-objid-vector location
                                     sub-objid sub-type-tag)
			       #-:lispworks4 ;; and hopefully not later
			       (dynamic-extent slot-name slot-allocation
					       slot-extent slot-deferred
					       sub-objid sub-type-tag))
		      (when (eq slot-name 'objid)
		        (setf (slot-value-using-class
			       the-class t-into-object slot-descr) p-objid))
		      (when (and (not (eq slot-extent :transient))
				 (member slot-extent
					 +plob-slot-represented-extents+))
                        (ecase slot-allocation
		          (:instance
		           (let ((location
                                  (slot-definition-location slot-descr)))
                             (declare (type fixnum location))
			     (when (and *verbose* (>= *verbose* 7))
			       (format t "; Loading slot ~A with depth ~A of~%; ~A~%"
				       slot-name depth t-into-object))
		             (multiple-value-setq (sub-objid sub-type-tag)
			         (read-objid-buffer objid-instance-buffer
                                                    location))))
                          (:class
		           (multiple-value-setq (sub-objid sub-type-tag)
			       (p-index p-heap
                                        (p-slot-description-location
                                         slot-descr :objid)
                                        +cons-location-cdr+
                                        +cons-type-tag+))))
			(if (and (p-markerp sub-type-tag)
				 (eql sub-type-tag +unbound-type-tag+))
			    (slot-makunbound-using-class
			     the-class t-into-object slot-descr-using-class)
			  (setf (slot-value-using-class
				 the-class t-into-object
				 slot-descr-using-class)
			    (cond
			     ((and (eq slot-extent :objid)
				   (not (p-immediatep sub-type-tag)))
			      sub-objid)
			     ((with-direct-representation-p slot-extent)
			      (make-persistent-object sub-objid sub-type-tag))
			     (t
			      (if (or *in-bootstrap-p*
				      (not persistent-metaclass-p)
				      (p-immediatep sub-type-tag)
				      (registered-objid-p sub-objid)
                                      ;; LispWorks cannot load instances of
                                      ;; description classes on demand;
				      ;; reason is that the access methods
				      ;; of PLOB's description classes
				      ;; cannot be patched safely in the
				      ;; bootstrap phase.
                                      #+:lispworks
                                      (subtypep the-class
                                                +plob-description-class+))
				  (p-objid-to-t-object sub-objid sub-type-tag
						       depth p-heap)
				(make-slot-load-on-demand
				 sub-objid sub-type-tag)))))))))))
	  t-into-object))
       
       (manual-load-instance-for-class
	   (class-descr constructor p-objid depth p-heap t-into-object)
	 (unless t-into-object
	   (setf t-into-object (funcall constructor p-objid depth p-heap)))
	 (unless (or (class-description-dependent class-descr)
		     (eq (is-registered-object t-into-object) p-objid))
	   (register-to-cache p-objid t-into-object))
	 t-into-object))

    (let* ((numeric-objid (persistent-object-objid p-objid))
	   (p-objid-instance-vector
            (p-instance-data-vector numeric-objid :objid p-heap))	    
           (constructor (class-description-constructor class-descr)))
      (if constructor
          (manual-load-instance-for-class
	   class-descr constructor numeric-objid depth p-heap t-into-object)
        (electric-load-instance-for-class
	 class-descr numeric-objid p-objid-instance-vector
	 depth p-heap t-into-object)))))

;;; ---------------------------------------------------------------------------
(defun update-instance-for-redefined-description
     (old-class-descr new-class-descr p-objid
		      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{old-class-descr}\\ resp.\\ \\funarg{new-class-descr}}
      {a class-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 The persistent \\clos\\ instance referenced by \\funarg{p-objid}\\ is
 migrated (or `schema-evoluted') from its current
 \\funarg{old-class-descr}\\ (found in the persistent \\clos\\ instance's
 class-wrapper) to the global actual
 \\funarg{new-class-descr}\\ (as now contained in the class table,
 i.e.\\ the class-description as returned by
 \\fcite{p-find-class}).
\\Seealsolabel
 \\Fcite{load-instance-for-redefined-class}."

  (labels ((set-evolution
            (class-descr new-evolution)
	    (setf (class-description-schema-evolution class-descr)
                  new-evolution)
            (setf (p-class-description-schema-evolution class-descr)
                  new-evolution)
            new-evolution)

           (migrate-vector
            (old-slot-descriptions new-slot-descriptions
                                   p-objid
                                   p-objid-old-vector p-heap)
            (let* ((number-of-old-slots
                    (p-vector-length p-objid-old-vector p-heap))
	           (number-of-new-slots (length new-slot-descriptions))
	           (new-sh-vector (make-array number-of-new-slots))
	           (number-of-copied-slots 0)
                   (p-objid-new-vector nil)
                   (added-slots (map 'list #'slot-definition-name
                                     new-slot-descriptions)))
              (with-objid-buffer
                  (objid-old-vector-buffer p-objid-old-vector
					   number-of-old-slots p-heap)
                ;; Load the object slots into a vector with migrate infos:
                (map
                 nil
	         #'(lambda (old-slot-descr)
		     (let* ((slot-name
			     (slot-definition-name old-slot-descr))
			    (new-slot-descr
			     (find slot-name
				   new-slot-descriptions
				   :test #'eq :key #'slot-definition-name)))
		       (when new-slot-descr
		         ;; Found 'old' slot in 'new'
		         ;; class description:
		         (delete slot-name added-slots :test #'eq)
		         (multiple-value-bind (value type)
                             (read-objid-buffer objid-old-vector-buffer
				                (slot-definition-location
				                 old-slot-descr))
			   (setf (svref new-sh-vector
				        number-of-copied-slots)
			         (make-migrate-info
				  (slot-definition-location new-slot-descr)
				  value
				  (if (p-immediatep type)
				      type
				    +short-objid-tag+))))
		         (incf number-of-copied-slots))))
	         old-slot-descriptions))
	      (p-unlock p-heap p-objid-old-vector
			(logior +lock-vector-read+ +lock-force+))
              ;; 1996/11/14: The old instance vector is write-locked too,
              ;; since it is marked as being dependent from the persistent
              ;; CLOS instance:
	      (p-unlock p-heap p-objid-old-vector
			(logior +lock-vector-write+ +lock-force+))
              ;; Allocate a new instance data vector, if necessary:
	      (setf p-objid-new-vector
                    (if (eql number-of-old-slots number-of-new-slots)
                        p-objid-old-vector
                      (p-allocate-vector number-of-new-slots p-heap)))
              ;; Copy the slots back into the persistent object:
              (write-lock p-heap p-objid-new-vector nil
                          :nocache +vector-type-tag+)
              (with-objid-buffer
                  (objid-new-vector-buffer p-objid-new-vector
					   number-of-new-slots p-heap)
	        (map
                 nil
		 #'(lambda (info)
                     (when info
                       (write-objid-buffer objid-new-vector-buffer
				           (migrate-info-location info)
			                   (migrate-info-value info)
				           (migrate-info-type  info))))
		 new-sh-vector))
	      (p-unlock p-heap p-objid-new-vector
			(logior +lock-vector-write+ +lock-force+))
              (unless (eql p-objid-old-vector p-objid-new-vector)
                (unregister-by-objid p-objid-old-vector)
                (remhash p-objid *instance->data-vector-cache*)
                (setf (gethash p-objid *instance->data-vector-cache*)
                      p-objid-new-vector)
                (p-destroy p-objid-old-vector))
              (values p-objid-new-vector added-slots))))

    (with-transaction (p-heap)
      (unless (class-description-equal-p old-class-descr new-class-descr)
	(let ((evolution
	       (class-description-schema-evolution old-class-descr))
	      (numeric-objid (persistent-object-objid p-objid)))
	  (unless evolution
	    (setf evolution *default-clos-schema-evolution*)
	    (set-evolution old-class-descr evolution))
	  (write-lock p-heap numeric-objid nil :nocache +instance-type-tag+)
	  (multiple-value-bind (p-objid-instance-vector added-instance-slots)
	      (migrate-vector
	       (remove-if-not #'(lambda (slot-descr)
				  (and (eq (slot-definition-allocation
					    slot-descr)
					   :instance)
				       (not (eq (slot-description-extent
						 slot-descr)
						:transient))))
			      (class-slots 
			       old-class-descr))
	       (remove-if-not #'(lambda (slot-descr)
				  (and (eq (slot-definition-allocation
					    slot-descr)
					   :instance)
				       (not (eq (slot-description-extent
						 slot-descr)
						:transient))))
			      (class-slots 
			       new-class-descr))
	       numeric-objid
	       (p-instance-data-vector numeric-objid :objid p-heap)
	       p-heap)
	    (setf (p-instance-data-vector numeric-objid :objid p-heap)
	      p-objid-instance-vector)
	    (setf (p-instance-class-wrapper numeric-objid :objid p-heap)
	      (persistent-object-objid new-class-descr))
	    added-instance-slots))))))

;;; ---------------------------------------------------------------------------
(defmethod load-instance-for-redefined-class
     (t-class-name
      (old-class-descr class-description)
      (new-class-descr class-description)
      p-objid depth p-heap &optional t-into-object)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{old-class-descr}\\ resp.\\ \\funarg{new-class-descr}}
      {a class-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient \\clos\\ instance representing the persistent
 \\clos\\ instance referenced by \\funarg{p-objid}\\ whose
 class-description was redefined.
\\Seealsolabel
 \\Fcite{update-instance-for-redefined-description};
 \\fcite{load-instance-for-class};
 \\fcite{(setf schema-evolution)}."

  (let ((added-slots
         (update-instance-for-redefined-description old-class-descr
                                                    new-class-descr
                                                    p-objid p-heap))
        (object
         (load-instance-for-class t-class-name new-class-descr
                                  p-objid depth p-heap t-into-object)))
    (when added-slots
      (shared-initialize object added-slots))
    object))

;;; ---------------------------------------------------------------------------
(defun p-instance (p-objid
                   &optional (depth *default-depth*)
                   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent \\clos\\ instance
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-instance)}."

  (let* ((t-object (is-registered-objid p-objid))
	 (t-class-name (when t-object
		         (class-name (class-of t-object))))
	 (p-class-descr (unless t-class-name
			  (p-class-description
			   (p-instance-class-wrapper p-objid :objid p-heap)
			   :cached
                           p-heap)))
         (from-store nil))
    #-:lispworks4 ;; and hopefully not later
    (declare (dynamic-extent t-class-name p-class-descr from-store))
    (unless t-class-name
      (setf t-class-name (class-name p-class-descr)))
    (let ((newest-generation (ensure-class-description t-class-name)))
      (with-transaction
       (p-heap)
       (with-read-lock
        (p-heap p-objid depth +instance-type-tag+ (null t-object))
        (setf from-store t)
        (unless p-class-descr
          (setf p-class-descr
	        (p-class-description
	         (p-instance-class-wrapper p-objid :objid p-heap)
	         :cached p-heap)))
        (setf t-object (if (eq newest-generation p-class-descr)
                           (load-instance-for-class
                            t-class-name p-class-descr
                            p-objid depth p-heap t-object)
                         (progn
                           (unregister-by-objid p-objid)
                           (load-instance-for-redefined-class
                            t-class-name p-class-descr newest-generation
                            p-objid depth p-heap t-object)))))))
    (when (null t-object)
      ;; When t-object is still nil, there was already a read-lock set
      ;; onto the object which prevented loading the object; this
      ;; means that someone tried to re-load the object while the
      ;; object loading is active on some previous level and the
      ;; object load on that level was not registered into the cache:
      (error "Detected circular reference at loading object ~A."
	     (make-persistent-object (persistent-object-objid p-objid))))
    (when (and *verbose* (>= *verbose* 5) from-store)
      (format t ";; Loaded CLOS instance~%;; ~A~%" t-object))
    t-object))

;;; ---------------------------------------------------------------------------
(defmethod initialize-instance :around
	   ((object persistent-clos-object)
	    &rest all-keys
	    &key suppress-initialization)
  #+:lisp-doc "Do not call next method if no initialization should be done at all,
 i.e.\\ if the \\keyarg{suppress-initialization}\\ argument was passed as
 \\lispnil."
  (when (or *in-bootstrap-p* (not suppress-initialization))
        (call-next-method)))

;;; ---------------------------------------------------------------------------
;;; Storing of CLOS instances
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object persistent-clos-object)
                                (depth (eql :objid))
				to-p-heap)
  (declare (ignore to-p-heap))
  (persistent-object-objid t-object))

;;; ---------------------------------------------------------------------------
(defmethod t-object-to-p-objid ((t-object persistent-clos-object)
                                (depth (eql :object))
				to-p-heap)
  (declare (ignore to-p-heap))
  (persistent-object-objid t-object))

;;; ---------------------------------------------------------------------------
;; Class metaobjects are stored as class-descriptions:
(defmethod t-object-to-p-objid-using-class ((t-object class)
                                            (t-class standard-class)
				            depth p-heap)
  #+:lisp-doc "Stores \\funarg{t-object}\\ as an instance of
 \\fcite{class-description}."
  (declare (ignore depth))
  (multiple-value-bind (p-objid p-type-tag)
      (p-find-class-objid (class-name t-object) p-heap)
    (unless p-objid
      (let ((p-class-descr nil))
	(multiple-value-setq (p-class-descr p-objid p-type-tag)
	  (ensure-class-description (class-name t-object)))))
    (values p-objid
	    (if (or (null p-type-tag) (p-immediatep p-type-tag))
		p-type-tag
	      +short-objid-tag+))))

;;; ---------------------------------------------------------------------------
;; All other metaobjects cannot be stored by PLOB:
(defmethod t-object-to-p-objid-using-class ((t-object metaobject)
                                            (t-class standard-class)
				            depth p-heap)
  #+:lisp-doc "Signals an error."
  (declare (ignore depth p-heap))
  (when (and *verbose* (>= *verbose* 1))
    (cerror "Don't store the object"
	    "Sorry, PLOB does not support storing of metaobject ~A."
	    t-object))
  (values +unstorable-object-marker+ +unstorable-object-marker+))

;;; ---------------------------------------------------------------------------
;; 'Simple' CLOS instances:
(defmethod t-object-to-p-objid-using-class ((t-object standard-object)
                                            (t-class standard-class)
				            depth p-heap)
  (store-instance t-object nil depth p-heap))

;;; ---------------------------------------------------------------------------
;;; Loading of CLOS instances
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +instance-type-tag+))
				depth
				p-heap)
  (p-instance p-objid depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
