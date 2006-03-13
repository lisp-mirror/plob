;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-clos-descr.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	9.3.94
;;;; Description	PLOB allocate and accessor functions for
;;;;		LISP CLOS class descriptions
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
(defvar *transient-slot-value* nil
  #+:lisp-doc "
\\Purposelabel
 The value of this variable affects the overall slot handling done.
 For a value being \\lispnil, all \\textbf{slot-\\ldots-using-class}
 methods implemented in \\plob\\ will work on both the transient and,
 if appropriate, the persistent representation.
 For a value being \\nonnil, all \\textbf{slot-\\ldots-using-class}
 methods implemented in \\plob\\ will work on only the transient
 representation. This is for example used at object loading, when
 a new transient representation is allocated by a call to
 \\fcite{make-instance}, which in turn calls
 \\fcite{initialize-instance}, which calls \\fcite{shared-initialize}.
 This call chain would normally store the object into the persistent
 heap again; to suppress this behavior, \\fcite{*transient-slot-value*}
 is set to \\nonnil.")

;;; ---------------------------------------------------------------------------
(defgeneric fill-slot-description
  (slot-definition the-class
                   &optional t-slot-descr (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isa{\\funarg{slot-definition}}
      {a slot definition metaobject}
 \\isacls{\\funarg{the-class}}
 \\isa{\\funarg{t-slot-descr}}
      {a slot description}
\\Valueslabel
 \\retarg{\\funarg{t-slot-descr}}
\\Purposelabel
 Stores a description of slot \\funarg{slot-definition}\\ of class
 \\funarg{the-class}\\ to \\funarg{t-slot-descr};
 if \\funarg{t-slot-descr}\\ is missing, the method returns a new
 created transient description object.
\\Seealsolabel
 \\Fcite{fill-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric finalize-class-description (class-descr)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isa{\\funarg{class-descr}}
      {a class description}
\\Purposelabel
 Finalize a class description. This finalization is done {\\sl after}
 the \\clos\\ finalization done by \\fcite{finalize-inheritance}.
\\Seealsolabel
 Slot {\\bf t-finalized-p} of \\fcite{persistent-metaclass};
 \\fcite{finalize-inheritance}."))

;;; ---------------------------------------------------------------------------
;;; Transient CLOS class description
;;; ---------------------------------------------------------------------------
(defconstant +standard-metaobject-classes+
    (mapcar #'find-class
	    '(t
	      clos::standard-object clos:metaobject
	      clos::class
	      clos::built-in-class
	      clos::standard-class
	      clos:slot-definition
	      clos:standard-slot-definition
	      clos:standard-direct-slot-definition
	      clos:standard-effective-slot-definition
	      clos::method
	      clos::standard-method
	      clos:standard-accessor-method
	      clos:standard-reader-method
	      clos:standard-writer-method
	      clos::method-combination))
  #+:lisp-doc
  "A list containing all standard CLOS metaobject classes.")

;;; ---------------------------------------------------------------------------
(defconstant +metaobject-class+ (find-class 'metaobject)
  #+:lisp-doc
  "The \\clsmo\\ of class \\class{metaobject}.")

;;; ---------------------------------------------------------------------------
(defconstant +standard-direct-slot-definition-class+
  (find-class 'standard-direct-slot-definition)
  #+:lisp-doc
  "The \\clsmo\\ of class \\class{standard-direct-slot-definition}.")

;;; ---------------------------------------------------------------------------
(defconstant +standard-effective-slot-definition-class+
  (find-class 'standard-effective-slot-definition)
  #+:lisp-doc
  "The \\clsmo\\ of class \\class{standard-effective-slot-definition}.")

;;; ---------------------------------------------------------------------------
(defun standard-metaclass-p (the-class)
  #+:lisp-doc
  "Check if \\funarg{the-class}\\ is a standard metaobject class
 as defined in \\cite{bib:AMOP}."
  (and (subtypep the-class +metaobject-class+)
       (member the-class +standard-metaobject-classes+)))

;;; ---------------------------------------------------------------------------
(defun remove-metaclass-p (the-class)
  #+:lisp-doc
  "Check if \\funarg{the-class}\\ is a non-standard metaobject class
 which should not be considered for storing into the database.
 This is the case for classes being no standard metaobject class
 as defined by a call to \\fcite{standard-metaclass-p}\\ and which
 do not inherit from \\class{standard-class},
 \\class{standard-direct-slot-definition}\\ or
 \\class{standard-effective-slot-definition}. Subclasses of
 these classes are allowed to store user-defined metaobject classes."
  (not (or (standard-metaclass-p the-class)
           (subtypep the-class +standard-class-class+)
           (subtypep the-class +standard-direct-slot-definition-class+)
           (subtypep the-class +standard-effective-slot-definition-class+))))

;;; ---------------------------------------------------------------------------
(defun remove-non-standard-metaclasses (metaclass-p classes)
  (let ((classes-vector (coerce classes 'vector)))
    (if metaclass-p
        (remove-if #'remove-metaclass-p classes-vector)
      classes-vector)))

;;; ---------------------------------------------------------------------------
(defmethod fill-description :around
    (the-class &optional t-descr (p-heap *default-persistent-heap*))
  #+:lisp-doc "Make sure that only a transient description will be filled."
  (declare (ignore the-class t-descr p-heap))
  (let ((*transient-slot-value* t))
    (call-next-method)))

;;; ---------------------------------------------------------------------------
(defmethod fill-description
     ((the-class class)
      &optional t-descr (p-heap *default-persistent-heap*))
  (unless t-descr
      (setf t-descr (make-instance 'class-description
		      :store-cached-slots nil
		      :p-heap p-heap)))
  (when (subtypep (class-of the-class) +standard-class-class+)
    (unless (class-finalized-p the-class)
      (finalize-inheritance the-class)
    (mapc #'(lambda (class)
              (unless (class-finalized-p the-class)
                (finalize-inheritance class)))
          (class-direct-superclasses the-class))))
  (let* ((effective-slots
	  ;; Sort slots by location and name:
	  (sort (copy-seq (class-slots the-class))
		#'(lambda (slot-1 slot-2)
		    (let ((location-1
			   (persistent-slot-definition-location slot-1))
			  (location-2
			   (persistent-slot-definition-location slot-2)))
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
			(string< (symbol-name (slot-definition-name slot-1))
				 (symbol-name
				  (slot-definition-name slot-2)))))))))
         (number-of-effective-slots (length effective-slots))
         (metaclass-p (subtypep the-class +metaobject-class+))
         #+:store-methods
         (list-of-methods (class-direct-methods the-class))
         #+:store-methods
         (number-of-methods (length list-of-methods)))
    (setf (class-description-name t-descr) (class-name the-class))
    (setf (class-description-superclasses t-descr)
          (remove-non-standard-metaclasses
           metaclass-p (class-direct-superclasses the-class)))
    (setf (class-description-precedence-list t-descr)
          (remove-non-standard-metaclasses
           metaclass-p (class-precedence-list the-class)))
    (setf (class-description-metaclass t-descr)
          (class-of the-class))
    (let ((evolution (schema-evolution the-class)))
      (unless evolution
        (setf evolution *default-clos-schema-evolution*)
        (setf (schema-evolution the-class) evolution))
      (setf (class-description-schema-evolution t-descr)
            evolution))
    (setf (class-description-next-generation t-descr) nil)
    (setf (class-description-slot-numbers t-descr)
          number-of-effective-slots)
    (setf (class-description-direct-methods t-descr) nil)
    (setf (class-description-direct-slots t-descr) nil)
    (setf (class-description-effective-slots t-descr) nil)
    (setf (class-description-constructor t-descr)
          (class-constructor the-class))
    (setf (class-description-dependent t-descr)
          (class-dependent the-class))
    (when (> number-of-effective-slots 0)
      (let ((slot-descr-vector (make-array number-of-effective-slots))
	    (persistent-slots 0) (shared-slots 0) (next-location 0))
	(loop for slot in effective-slots
	    for i from 0
	    as slot-descr = (fill-slot-description slot the-class nil p-heap)
	    as slot-allocation = (slot-description-allocation slot-descr)
	    as slot-extent = (slot-description-extent slot-descr)
	    do
	      (setf (svref slot-descr-vector i) slot-descr)
	      (unless (eq slot-extent :transient)
		(if (eq slot-allocation :class)
		    (incf shared-slots)
		  (let ((location (slot-description-location slot-descr)))
		    (when (and *in-bootstrap-p* location)
		      (setf next-location
			(max next-location (1+ location))))))))
	(loop for i from 0 below number-of-effective-slots
	    as slot-descr = (svref slot-descr-vector i)
	    as slot-allocation = (slot-description-allocation slot-descr)
	    as slot-extent = (slot-description-extent slot-descr)
	    as slot-location = (slot-description-location slot-descr)
	    do
	      (unless (or (eq slot-extent :transient)
			  (eq slot-allocation :class)
			  (and *in-bootstrap-p* slot-location))
		(setf (slot-description-location slot-descr) next-location)
		(incf next-location)))
	(setf persistent-slots next-location)
	(setf (class-description-persistent-slot-numbers t-descr)
	  persistent-slots)
        (when (or (> persistent-slots 0) (> shared-slots 0))
	  ;; Store the slot descriptions and methods only if there is at
	  ;; least one persistent slot.
          (let* ((direct-slots (class-direct-slots the-class))
                 (number-of-direct-slots (length direct-slots)))
            (when (> number-of-direct-slots 0)
              (let ((slot-descr-vector (make-array number-of-direct-slots)))
	        (do ((s direct-slots (cdr s))
	             (i 0 (1+ i)))
	            ((null s))
		  (setf (svref slot-descr-vector i)
		    (fill-slot-description (car s) the-class nil p-heap)))
	        (setf (class-description-direct-slots t-descr)
		  slot-descr-vector))))
	  (setf (class-description-effective-slots t-descr) slot-descr-vector)
          #+:store-methods
	  (when (> number-of-methods 0)
	    (let ((method-vector (make-array number-of-methods)))
	      (setf (class-description-direct-methods t-descr) method-vector)
	      (do ((m list-of-methods (cdr m))
		   (i 0 (1+ i)))
		  ((null m))
		(setf (svref method-vector i)
		  (fill-description (car m) nil p-heap)))))))))
  t-descr)

;;; ---------------------------------------------------------------------------
(defmethod class-description-equal-p
     ((t-class-descr class-description)
      (p-class-descr class-description)
      &optional verbose)
  #-:store-methods
  (declare (ignore verbose))
  (multiple-value-bind (equal-p reason)
      (call-next-method)
    (block nil
      (unless equal-p
	(return (values nil reason)))
      #-:store-methods
      t
      #+:store-methods
      (let* ((t-methods (class-direct-methods t-class-descr))
	     (number-of-t-methods (length t-methods))
	     (p-methods (class--direct-methods t-class-descr))
	     (number-of-p-methods (length p-methods)))
	(unless (= number-of-t-methods number-of-p-methods)
	  (return (values nil
			  (when verbose
			    (format nil "number of methods; tr.: ~A / pe.: ~A"
				    number-of-t-methods
				    number-of-p-methods)))))
	(dotimes (i number-of-t-methods t)
	  (multiple-value-bind (equal-p reason)
	      (method-description-equal-p (svref t-methods i)
					  (svref p-methods i)
					  verbose)
	    (unless equal-p
	      (return (values nil reason))))))
      t)))

;;; ---------------------------------------------------------------------------
(defmethod generate-description
     ((class-descr class-description)
      &optional add-extra-slot-options add-extra-class-options)
  #+:lisp-doc "See \\fcite{generate-defclass}."

  (flet ((generate-slot-specification
          (slot &optional add-extra-slot-options)
	  "See [MOP91], p. 54"
	  `(,(slot-definition-name slot)
	     ,@(let ((allocation (slot-definition-allocation slot)))
		 (when (eq allocation :class)
		   `(:allocation ,allocation)))
	     ,@(when (slot-definition-initfunction slot)
		 `(:initform ,(slot-definition-initform slot)))
	     ,@(when (slot-definition-initargs slot)
		 (mapappend #'(lambda (initarg)
				`(:initarg ,initarg))
			    (slot-definition-initargs slot)))
	     ,@(let ((type (slot-definition-type slot)))
		 (when (and type (not (eq type t)))
		   `(:type ,type)))
	     ,@(when add-extra-slot-options
		 (let ((extra-options ()))
		   (when (slot-description-index slot)
		     (push (slot-description-index slot) extra-options)
		     (push :index extra-options))
		   (when (slot-description-extent slot)
		     (push (slot-description-extent slot) extra-options)
		     (push :extent extra-options))
		   (when (slot-description-deferred slot)
		     (push (slot-description-deferred slot) extra-options)
		     (push :deferred extra-options))
		   extra-options)))))

    `(defclass ,(class-name class-descr)
      ,(map 'list #'class-name (class-direct-superclasses class-descr))
      ,(map 'list
            #'(lambda (s)
                (generate-slot-specification s add-extra-slot-options))
	    (class-direct-slots class-descr))
      ,@(let ((metaclass-name
               (class-name (class-description-metaclass class-descr))))
          (when (not (eq metaclass-name 'standard-class))
            `((:metaclass ,metaclass-name))))
      ,@(when add-extra-class-options
          (make-extra-class-options class-descr)))))

;;; ---------------------------------------------------------------------------
(defmethod compile-description ((class-descr class-description))
  (flet ((generate-slot-specification
          (slot)
	  "See [MOP91], p. 54"
	  `(:name ,(slot-definition-name slot)
	          ,@(let ((allocation (slot-definition-allocation slot)))
                      (when (eq allocation :class)
                        `(:allocation ,allocation)))
	          ,@(let ((initform (slot-definition-initform slot)))
                      (when (slot-definition-initfunction slot)
                        (cond
                         ((eq initform nil)
                          `(:initform nil :initfunction #'return-nil))	
                         ((eq initform t)
                          `(:initform t :initfunction #'return-t))
                         (t
                          `(:initform ,initform :initfunction
                            ,(compile-silent nil
					     `(lambda ()
						,initform)))))))
	          ,@(when (slot-definition-initargs slot)
		      `(:initargs ,(slot-definition-initargs slot)))
	          ,@(when (slot-description-index slot)
		      `(:index ,(slot-description-index slot)))
	          ,@(when (slot-description-extent slot)
		      `(:extent ,(slot-description-extent slot)))
	          ,@(when (slot-description-deferred slot)
		      `(:deferred ,(slot-description-deferred slot)))
	          ,@(when (slot-definition-readers slot)
		      `(:readers ,(slot-definition-readers slot)))
	          ,@(when (slot-definition-type slot)
		      `(:type ,(slot-definition-type slot)))
	          ,@(when (slot-definition-writers slot)
		      `(:writers ,(slot-definition-writers slot))))))

    (let ((*default-depth* :cached))
      (when (/= (class-description-slot-numbers class-descr)
		(length (class-description-effective-slots class-descr)))
	(error "Request to compile incomplete class description ~A."
	       class-descr))
      (let ((all-slots (map 'list
			 #'(lambda (s)
			     (generate-slot-specification s))
			 (class-direct-slots class-descr))))
	(when (and *verbose* (>= *verbose* 2))
	  (format t ";;;;; Compiling ~A~%" class-descr))
	(ensure-class (class-name class-descr)
		      :metaclass (class-name (class-description-metaclass
					      class-descr))
                      ;;; 2004-04-13 hkirschk: Starting from LispWorks 4.4,
                      ;;; the argument is called now :direct-slots instead
                      ;;; of :slots. Sigh. It took pretty long ...
		      #-(or :lispworks3 :lispwork4.0 :lispwork4.1
                            :lispwork4.2 :lispwork4.3) :direct-slots
		      #+(or :lispworks3 :lispwork4.0 :lispwork4.1
                            :lispwork4.2 :lispwork4.3) :slots
		      all-slots
		      #-:lispworks3 :direct-superclasses
		      #+:lispworks3 :superclasses
		      (map 'list
			#'class-name
			(class-direct-superclasses class-descr)))))))

;;; ---------------------------------------------------------------------------
(defun ensure-class-description (name &optional (depth :cached))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{name}}
      {a symbol naming a class}
\\Valueslabel
 Returns two values:
 \\begin{enumerate}

 \\item The actual class description of the class
  named \\funarg{name}\\ is returned as the first value.

 \\item If the class description was subject to schema evolution,
  \\nonnil\\ is returned as the second value; \\lispnil\\ otherwise.

 \\end{enumerate}

\\Purposelabel
 Establish the class named \\funarg{name}:
 \\begin{itemize}

 \\item When there was no class description named
  \\funarg{name}\\ found in the \\sh\\ class table, store the
  class description of the class named
  \\funarg{name}\\ to the \\sh.

 \\item When a class description named
  \\funarg{name}\\ was found in the \\sh\\ class table, check if there
  is a mismatch between the persistent class description and the
  current transient version of the class definition.
  If a mismatch is detected, resolve the mismatch by calling
  \\fcite{handle-class-mismatch}. Do nothing if no mismatch
  was detected.

 \\end{itemize}
\\Seealsolabel
 \\Fcite{p-find-class};
 \\fcite{handle-class-mismatch};
 \\fcite{ensure-structure-description}."

  (if (eq name 'class-description)
      (case depth
        (:objid
         *class-description-objid*)
        (:object
         (make-persistent-object *class-description* +instance-type-tag+))
        (t
         *class-description*))
    (let ((p-type-tag (gethash name *lisp-type->type-tag*)))
      (if p-type-tag
	  (values (make-persistent-object p-type-tag +built-in-type-tag+)
		  p-type-tag +built-in-type-tag+)
	(let ((*default-depth* depth)
	      (the-class (find-class name nil)))
	  (declare (special *default-depth*))
	  (when (and the-class (not (class-finalized-p the-class)))
	    (finalize-inheritance the-class))
	  (let* ((mismatch-p nil)
		 (reason nil)
		 (t-class-descr nil)
		 (p-class-descr
		  (p-find-class-description name *default-persistent-heap*)))
	    (if p-class-descr
		(if the-class
		    (when (mismatch-p the-class)
		      (unless t-class-descr
			(setf t-class-descr (fill-description the-class)))
		      (multiple-value-setq (mismatch-p reason)
			(class-description-equal-p t-class-descr
						   p-class-descr t))
		      (setf mismatch-p (not mismatch-p))
		      (when (and mismatch-p
				 (eq (class-description-schema-evolution
				      p-class-descr)
				     :no-evolution))
			(when (and *verbose* (>= *verbose* 1))
			  (cerror "Do not store transient class definition."
				  "Class ~S is marked with :no-evolution ~
			           but its transient definition was modified."
				  name))
			(setf mismatch-p nil)))
		  (progn
		    ;; There is no transient class definition up to now;
		    ;; so there can also be no mismatch:
		    (setf t-class-descr p-class-descr)
		    (map nil
			 #'(lambda (class)
			     (ensure-class-description (class-name class)))
			 (class-direct-superclasses p-class-descr))
		    (compile-description p-class-descr)
		    (setf the-class (find-class name))
		    (when (and (not (class-finalized-p the-class))
			       (subtypep (class-of the-class)
					 +standard-class-class+))
		      (finalize-inheritance the-class))
		    (setf (class-description-of the-class) p-class-descr)))
	      (progn
                (unless the-class
                  (error "Could not locate class named ~A"
                         (pretty-print-symbol name)))
		;; There is no persistent class description up to now;
		;; so generate, fill ...
		(setf p-class-descr
		      (fill-description the-class nil
					*default-persistent-heap*))
		(setf t-class-descr p-class-descr)
		;; ... and save it:
		(update-class p-class-descr *default-persistent-heap*)))
	    (setf (mismatch-p the-class) mismatch-p)
	    (when mismatch-p
	      (unless t-class-descr
		(setf t-class-descr (fill-description the-class)))
	      (unless (handle-class-mismatch t-class-descr p-class-descr
					     reason *default-persistent-heap*)
		(let ((p-objid-class
		       (p-find-class-objid name *default-persistent-heap*)))
		  (setf p-class-descr
                    (p-class-description p-objid-class depth
					 *default-persistent-heap*)))))
	    (unless p-class-descr
	      (setf p-class-descr
		    (p-find-class-description name *default-persistent-heap*)))
	    (values p-class-descr
		    (persistent-object-objid p-class-descr)
		    +short-objid-tag+)))))))

;;; ---------------------------------------------------------------------------
(defmethod update-class :around
  ((class-descr class-description)
   &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Ensure that the slots
 with \\lisp{:allocation :class}\\ are allocated on the \\sh."

  (let ((*default-depth*
	 ;; :deep ;; 1998/09/18 HK
	 :cached
	 ))
    (labels

	((search-most-specific-direct-slot
	     (slot start-class)
	   (some #'(lambda (class)
		     (when (find-direct-slot (slot-definition-name slot)
					     class)
		       class))
		 (subseq (class-precedence-list start-class) 1)))

	 (allocate-shared-slots
	     (old-class-descr new-class-descr p-heap)
	   "Cycle through all effective slot definitions with
 :allocation :class and search the shared slot cons."
	   (delete
	    nil
	    (map 'list
	      #'(lambda (shared-slot)
		  (let* ((slot-name (slot-definition-name shared-slot))
			 (old-slot
			  (when old-class-descr
			    (find-effective-slot slot-name
						 old-class-descr)))
			 (shared-value nil)
			 (shared-type-tag nil)
			 (new-shared-slot nil))
		    (if (and old-slot
			     (eq (slot-definition-allocation old-slot)
				 :class))
			;; Found the shared slot in the old class
			;; description; so copy its value to new class
			;; description:
			(multiple-value-setq
			    (shared-value shared-value shared-type-tag)
			  (p-slot-description-location old-slot :objid
						       p-heap))
		      (let* ((inherited-slot-class
			      (search-most-specific-direct-slot
			       shared-slot new-class-descr))
			     (inherited-slot-class-descr
			      (when inherited-slot-class
				(class-description-of
				 inherited-slot-class)))
			     (inherited-slot
			      (when inherited-slot-class-descr
				(find-effective-slot
				 slot-name
				 inherited-slot-class-descr))))
			(when inherited-slot
			  (multiple-value-setq
			      (shared-value shared-value shared-type-tag)
			    (p-slot-description-location inherited-slot
							 :objid p-heap)))))
		    (unless (and shared-value
			         (= shared-type-tag +cons-type-tag+))
		      ;; Did not found the shared slot by inheritance from
		      ;; one of the superclasses; so generate a new shared
		      ;; slot cons:
		      (setf shared-value (p-allocate-cons p-heap))
		      (setf shared-type-tag +cons-type-tag+)
		      (setf (p-car shared-value :cached p-heap) slot-name)
		      (setf new-shared-slot shared-slot))
		    (setf (p-slot-description-location
                           shared-slot :objid p-heap)
		      shared-value)
		    (setf (slot-description-location shared-slot)
		      (p-objid-to-t-object shared-value shared-type-tag
					   :cached p-heap))
		    new-shared-slot))
	      (remove-if-not #'(lambda (slot)
				 (eq (slot-definition-allocation slot)
				     :class))
			     (class-slots new-class-descr)))))

	 (allocate-indices-for-slots
	     (old-class-descr new-class-descr)
	   "Cycle through all effective slot definitions with an
 :index and search the index object."
	   (map nil
	     #'(lambda (indexed-slot)
		 (let* ((slot-name (slot-definition-name indexed-slot))
			(old-slot
			 (when old-class-descr
			   (find-effective-slot slot-name
						old-class-descr)))
                        (index-value nil) (index-type-tag nil)
			(old-slot-index
			 (when old-slot
			   (slot-description-index old-slot))))
                   (if old-slot-index
		       ;; Found the index in the old class
		       ;; description; so copy its value to new class
		       ;; description:
		       (multiple-value-setq
			   (index-value index-value index-type-tag)
			 (p-slot-description-index old-slot :objid p-heap))
		     (let* ((inherited-slot-class
			     (search-most-specific-direct-slot
			      indexed-slot new-class-descr))
			    (inherited-slot-class-descr
			     (when inherited-slot-class
			       (class-description-of
				inherited-slot-class)))
			    (inherited-slot
			     (when inherited-slot-class-descr
			       (find-effective-slot
				slot-name
				inherited-slot-class-descr))))
		       (when inherited-slot
			 (multiple-value-setq
			     (index-value index-value index-type-tag)
			   (p-slot-description-index inherited-slot
						     :objid p-heap)))))
                   (unless index-value
		     ;; Did not found the index by inheritance from
		     ;; one of the superclasses; so generate a new index:
		     (multiple-value-setq
			 (index-value index-type-tag)
		       (t-object-to-p-objid
			(make-index (slot-description-index indexed-slot))
			*default-depth* p-heap)))
                   (unless index-type-tag
                     (setf index-type-tag +short-objid-tag+))
                   (when (p-immediatep index-type-tag)
                     (error "Trying to establish an immediate index object on slot ~A of ~A."
                            indexed-slot new-class-descr))
                   (setf (p-slot-description-index indexed-slot :objid p-heap)
		     index-value)
		   (setf (slot-description-index indexed-slot)
		     (p-objid-to-t-object index-value index-type-tag
					  *default-depth* p-heap))))
	     (remove-if-not #'(lambda (slot)
				(slot-description-index slot))
			    (class-slots new-class-descr)))))

      (with-transaction (p-heap)
	(let* ((name (class-name class-descr))
	       (old-class-descr (p-find-class-description name p-heap))
	       (the-class (find-class name))
	       (the-result nil))
	  (when old-class-descr
	    (setf (class-description-prototype old-class-descr) nil))
	  (setf the-result (call-next-method))
	  (setf (class-description-of the-class) class-descr)
	  (allocate-indices-for-slots old-class-descr class-descr)
	  (let ((new-shared-slots
		 (delete nil (allocate-shared-slots old-class-descr
						    class-descr p-heap))))
	    (when (and new-shared-slots
		       (subtypep (class-of the-class)
				 +persistent-metaclass-class+))
	      (shared-initialize (class-prototype class-descr)
				 (map 'list
				   #'slot-definition-name
				   new-shared-slots))))
	  the-result)))))

;;; ---------------------------------------------------------------------------
;;; Persistent CLOS class description
;;; ---------------------------------------------------------------------------

(defun p-allocate-class-description (&optional
				     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 class-description
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{class-description};
 \\fcite{p-allocate}."

  (p-allocate-instance *class-description-objid* p-heap))

;;; ---------------------------------------------------------------------------
(defun p-class-description-p
     (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of class
 class-description,
 \\lispnil\\ otherwise.
\\Seealsolabel
 \\Fcite{class-description}."

  (and (= (p-type-tag-of p-objid p-heap) +instance-type-tag+)
       (with-transaction (p-heap)
	 (= (p-index p-heap p-objid +clos-location-class-wrapper+)
	    *class-description-objid*))))

;;; ---------------------------------------------------------------------------
#+(and :lisp-doc (not :document-api))
(:defdoc
 "p-class-description- ..."
 "Class Description Accessors"
 "
\\Purposelabel
 All following functions
 {\\bf p-class-description-\\textsl{\\lt{}slot-name\\gt}} and
 {\\bf (setf p-class-description-\\textsl{\\lt{}slot-name\\gt}\\/)}
 with \\textsl{\\lt{}slot-name\\gt} being one of the slot names of
 \\fcite{class-description}\\ without the `p-' prefix
 access directly a slot of a persistent \\clos\\ instance
 of \\fcite{class-description}\\ in the \\sh.

 The accessor functions defined here are merely used in the
 bootstrap phase. After the bootstrap, the persistent objects of
 \\fcite{class-description}\\ are created and accessed as all
 persistent \\clos\\ instances by employing their
 class-descriptions, i.e.\\ all information needed to create
 a persistent \\clos\\ instance or to access a persistent
 \\clos\\ instance's slot is contained in its class-description.
\\Seealsolabel
 \\Fcite{class-description};
 section \\fcite{bootstrap ...}.")

;;; ---------------------------------------------------------------------------
(defun p-class-description-name
    (p-objid &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-name}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-name+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-name)
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
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-name}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-name+
		   depth p-heap)
      t-name)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-superclasses
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-superclasses}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-superclasses+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-superclasses)
    (t-superclasses p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-superclasses}}
      {a list or vector either with \\clsmo{}s or class-descriptions}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-superclasses}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-superclasses}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-superclasses+
		   depth p-heap)
      t-superclasses)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-precedence-list
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-precedence-list}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-precedence-list+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-precedence-list)
    (t-precedence-list p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-precedence-list}}
      {a list or vector either with \\clsmo{}s or class-descriptions}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-precedence-list}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-precedence-list}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-precedence-list+
		   depth p-heap)
      t-precedence-list)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-metaclass
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-metaclass}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-metaclass+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-metaclass)
    (t-metaclass p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-metaclass}}
      {a \\clsmo\\ or a class-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-metaclass}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-metaclass}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-metaclass+
		   depth p-heap)
      t-metaclass)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-version-number
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-version-number}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-version+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-version-number)
    (t-version-number p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-version-number}}
      {a fixnum}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-version-number}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-version-number}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
	          +class-description-location-version+
	          depth p-heap)
      t-version-number)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-time-stamp
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-time-stamp}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-time-stamp+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-time-stamp)
    (t-time-stamp p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-time-stamp}}
      {a time stamp in \\cl\\ Universal Time divided by 60,
       i.e.\\ the time is in minutes, not in seconds.}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-time-stamp}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-time-stamp}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-time-stamp+
		   depth p-heap)
      t-time-stamp)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-schema-evolution
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-schema-evolution}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-schema-evolution+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-schema-evolution)
     (t-schema-evolution p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-schema-evolution}}
      {a keyword symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-schema-evolution}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-schema-evolution}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-schema-evolution+
		   depth p-heap)
      t-schema-evolution)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-next-generation
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-next-generation}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-next-generation+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-next-generation)
     (t-next-generation p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-next-generation}}
      {either \\lispnil\\ or a class-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-next-generation}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-next-generation}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-next-generation+
		   depth p-heap)
      t-next-generation)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-direct-methods
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-direct-methods}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-direct-methods+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-direct-methods)
     (t-direct-methods p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-direct-methods}}
      {a list or vector either with \\mtdmo{}s or method-descriptions}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-direct-methods}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-direct-methods}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
   (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
	          +class-description-location-direct-methods+
	          depth p-heap)
         t-direct-methods)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-persistent-slot-numbers
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-persistent-slot-numbers}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-persistent-slot-numbers+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-persistent-slot-numbers)
     (t-persistent-slot-numbers p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{n-slots}}
      {a fixnum}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-persistent-slot-numbers}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{n-slots}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-persistent-slot-numbers+
		   depth p-heap)
      t-persistent-slot-numbers)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-slot-numbers
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-slot-numbers}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-slot-numbers+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-slot-numbers)
     (t-slot-numbers p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{n-slots}}
      {a fixnum}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-slot-numbers}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{n-slots}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
   (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
	          +class-description-location-slot-numbers+
	          depth p-heap)
         t-slot-numbers)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-direct-slots
    (p-objid &optional
	     (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-direct-slots}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
   (p-svref (p-instance-data-vector p-objid :objid p-heap)
	    +class-description-location-direct-slots+
	    depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-direct-slots)
     (t-slots p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-slots}}
      {a list or vector either with \\dir\\ \\sltmo{}s or
       direct-slot-descriptions}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-direct-slots}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-slots}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
   (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
	          +class-description-location-direct-slots+
	          depth p-heap)
     t-slots)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-effective-slots
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-effective-slots}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
   (p-svref (p-instance-data-vector p-objid :objid p-heap)
	    +class-description-location-effective-slots+
	    depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-effective-slots)
     (t-slots p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-effective-slots}}
      {a list or vector either with \\eff\\ \\sltmo{}s or
       effective-slot-descriptions}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-effective-slots}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-effective-slots}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
   (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
	          +class-description-location-effective-slots+
	          depth p-heap)
         t-slots)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-constructor
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-constructor}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-constructor+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-constructor)
     (t-constructor p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-constructor}}
      {a symbol bound to a function which generates an instance of
       the class described by the persistent class description
       referenced by \\funarg{p-objid}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-constructor}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-constructor}.
\\Seealsolabel

 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-constructor+
		   depth p-heap)
      t-constructor)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-dependent
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-dependent}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
   (p-svref (p-instance-data-vector p-objid :objid p-heap)
	    +class-description-location-dependent+
	    depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-dependent)
     (t-dependent p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-dependent}}
      {a symbol bound to a function which generates an instance of
       the class described by the persistent class description
       referenced by \\funarg{p-objid}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-dependent}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-dependent}.
\\Seealsolabel

 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-dependent+
		   depth p-heap)
      t-dependent)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-plist
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-plist}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-plist+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-plist)
     (t-plist p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-plist}}
      {a property list}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-plist}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-plist}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
   (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
	          +class-description-location-plist+
	          depth p-heap)
         t-plist)))

;;; ---------------------------------------------------------------------------
(defun p-class-description-prototype
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-prototype}
 of the persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +class-description-location-prototype+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description-prototype)
     (t-prototype p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-prototype}}
      {a persistent instance of
       the class described by the persistent class description
       referenced by \\funarg{p-objid}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-prototype}
 of the persistent class-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-prototype}.
\\Seealsolabel
 Section \\fcite{p-class-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +class-description-location-prototype+
		   depth p-heap)
         t-prototype)))

;;; ---------------------------------------------------------------------------
(defmethod class-prototype ((class-descr class-description))
  #+:lisp-doc "The returned prototype is a persistent object."
  (let ((prototype (class-description-prototype class-descr)))
    (unless prototype
      (let ((the-class (find-class (class-name class-descr))))
        (if (subtypep (class-of the-class) +persistent-metaclass-class+)
            (let ((p-objid (p-allocate-instance class-descr)))
              (setf prototype (make-instance (class-name class-descr)
				:store-cached-slots nil
				:suppress-initialization t))
              (register-to-cache p-objid prototype)
              (setf (persistent-object-objid prototype) p-objid))
          (setf prototype (class-prototype the-class)))
	(setf (class-description-prototype class-descr) prototype)
	(setf (p-class-description-prototype class-descr :cached
                                             *default-persistent-heap*)
              prototype)))
    prototype))

;;; ---------------------------------------------------------------------------
(defun p-class-description-into
     (t-into-descr p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-into-descr}}
      {a class-description}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Read the persistent class-description referenced by
 \\funarg{p-objid}\\ destructively into \\funarg{t-into-descr}.
\\Seealsolabel
 \\Fcite{p-class-description}."
  (setf (persistent-object-objid t-into-descr) p-objid)
  (with-transaction
   (p-heap)
   (with-read-lock
    (p-heap p-objid depth +instance-type-tag+ t)
    (setf (class-description-name t-into-descr)
          (p-class-description-name p-objid depth p-heap))
    (setf (class-description-superclasses t-into-descr)
          (p-class-description-superclasses p-objid depth p-heap))
    (setf (class-description-precedence-list t-into-descr)
          (p-class-description-precedence-list p-objid depth p-heap))
    (setf (class-description-metaclass t-into-descr)
          (p-class-description-metaclass p-objid depth p-heap))
    (setf (class-description-version-number t-into-descr)
          (p-class-description-version-number p-objid depth p-heap))
    (setf (class-description-time-stamp t-into-descr)
          (p-class-description-time-stamp p-objid depth p-heap))
    (setf (class-description-schema-evolution t-into-descr)
          (p-class-description-schema-evolution p-objid depth p-heap))
    (setf (class-description-next-generation t-into-descr)
          (p-class-description-next-generation p-objid depth p-heap))
    (setf (class-description-direct-methods t-into-descr)
          (p-class-description-direct-methods p-objid depth p-heap))
    (setf (class-description-persistent-slot-numbers t-into-descr)
          (p-class-description-persistent-slot-numbers p-objid depth p-heap))
    (setf (class-description-slot-numbers t-into-descr)
          (p-class-description-slot-numbers p-objid depth p-heap))
    (setf (class-description-direct-slots t-into-descr)
          (p-class-description-direct-slots p-objid depth p-heap))
    (setf (class-description-effective-slots t-into-descr)
          (p-class-description-effective-slots p-objid depth p-heap))
    (setf (class-description-constructor t-into-descr)
          (p-class-description-constructor p-objid depth p-heap))
    (setf (class-description-dependent t-into-descr)
          (p-class-description-dependent p-objid depth p-heap))
    (setf (class-description-plist t-into-descr)
          (p-class-description-plist p-objid depth p-heap))
    (setf (class-description-prototype t-into-descr)
          (p-class-description-prototype p-objid depth p-heap))))
  t-into-descr)

;;; ---------------------------------------------------------------------------
(defun p-class-description
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 class-description
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-class-description)}."

  (let ((t-object (is-registered-objid p-objid)))
    (unless t-object
      (let ((*transient-slot-value* t))
	(setf t-object (make-instance 'class-description
			              :p-heap p-heap
			              :store-cached-slots nil)))
      (register-to-cache p-objid t-object)
      (p-class-description-into t-object p-objid depth p-heap))
    t-object))

;;; ---------------------------------------------------------------------------
(defun store-class-description (t-descr p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a class-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient class-description in
 \\funarg{t-descr}\\ to the
 persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-class-description}."

  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-descr))
      (unless p-objid
	(setf p-objid (p-allocate-class-description p-heap))
	(setf (persistent-object-objid t-descr) p-objid)
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-descr depth
			       +instance-type-tag+ force-write)
	(setf (p-class-description-name p-objid depth p-heap)
	  (class-name t-descr))
	(setf (p-class-description-superclasses p-objid depth p-heap)
	  (class-direct-superclasses t-descr))
	(setf (p-class-description-precedence-list p-objid depth p-heap)
	  (class-precedence-list t-descr))
	(setf (p-class-description-metaclass p-objid depth p-heap)
	  (class-description-metaclass t-descr))
	(setf (p-class-description-version-number p-objid depth p-heap)
	  (class-description-version-number t-descr))
	(setf (p-class-description-time-stamp p-objid depth p-heap)
	  (class-description-time-stamp t-descr))
	(setf (p-class-description-schema-evolution p-objid depth p-heap)
	  (class-description-schema-evolution t-descr))
	(setf (p-class-description-next-generation p-objid depth p-heap)
	  (class-description-next-generation t-descr))
	(setf (p-class-description-direct-methods p-objid depth p-heap)
	  (class-direct-methods t-descr))
	(setf (p-class-description-persistent-slot-numbers
	       p-objid depth p-heap)
	  (class-description-persistent-slot-numbers t-descr))
	(setf (p-class-description-slot-numbers p-objid depth p-heap)
	  (class-description-slot-numbers t-descr))
	(setf (p-class-description-direct-slots p-objid depth p-heap)
	  (class-direct-slots t-descr))
	(setf (p-class-description-effective-slots p-objid depth p-heap)
	  (class-slots t-descr))
	(setf (p-class-description-constructor p-objid depth p-heap)
	  (class-description-constructor t-descr))
	(setf (p-class-description-dependent p-objid depth p-heap)
	  (class-description-dependent t-descr))
	(setf (p-class-description-plist p-objid depth p-heap)
	  (class-description-plist t-descr))
	(setf (p-class-description-prototype p-objid depth p-heap)
	  (class-prototype t-descr)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-class-description)
    (t-descr &optional p-objid (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a class-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient class-description in
 \\funarg{t-descr}\\ to the
 persistent class-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-class-description}."

  (values t-descr (store-class-description t-descr p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defmethod ensure-allocated-object
     ((t-object class-description)
      &optional (p-heap *default-persistent-heap*))
  (let ((p-objid (persistent-object-objid t-object)))
    (unless p-objid
      (setf p-objid (p-allocate-class-description p-heap))
      (setf (persistent-object-objid t-object) p-objid))
    p-objid))

;;; ---------------------------------------------------------------------------
(defmethod initialize-instance :before
  ((class standard-class) &rest all-keys)
  #+:lisp-doc "Marks \\funarg{class}\\ as being (perhaps) mismatched;
 see \\fcite{(setf mismatch-p)}."
  (setf (mismatch-p class) t))

;;; ---------------------------------------------------------------------------
(defmethod reinitialize-instance :before
  ((class standard-class) &rest all-keys)
  #+:lisp-doc "Marks \\funarg{class}\\ as being (perhaps) mismatched;
 see \\fcite{(setf mismatch-p)}."
  (declare (ignore all-keys))
  (setf (mismatch-p class) t))

;;; ---------------------------------------------------------------------------
(defmethod reinitialize-instance :after
  ((class persistent-metaclass) &rest all-keys)
  #+:lisp-doc "Patch the class' accessor methods."
  (declare (ignore all-keys))
  (patch-class-methods class))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-description-of) (class-description (the-class class))
  #+:lisp-doc "Does nothing."
  class-description)

;;; ---------------------------------------------------------------------------
(defmethod class-description-of ((the-class class))
  #+:lisp-doc "Trapped to \\fcite{p-find-class-description}."
  (p-find-class-description (class-name the-class) *default-persistent-heap*))

;;; ---------------------------------------------------------------------------
(defmethod class-description-of :around ((the-class persistent-metaclass))
  #+:lisp-doc "
 Reads the class-description from the slot {\\bf t-class-description}
 of \\funarg{the-class};
 if the returned value is \\lispnil, the class description is loaded
 and written into the slot {\\bf t-class-description}."
  (let ((class-description (call-next-method)))
    (unless (or class-description *in-bootstrap-p*)
      (setf class-description
            (ensure-class-description (class-name the-class))))
    class-description))

;;; ---------------------------------------------------------------------------
#+(or :allegro :lispworks)
(defmethod shared-initialize
     ((instance persistent-clos-object) slot-names
      &rest all-keys
      &key objid
	   (p-heap *default-persistent-heap*)
	   (store-cached-slots t)
	   suppress-initialization)
  #+:lisp-doc "
\\Purposelabel
%% ;;; HK 03.09.92:
 In \\allegro\\ and \\lw, the methods of the
 \\fcite{shared-initialize}\\ do not work like expected:\\ the slot
 initialization values are set by using direct low-level slot
 accessors and not by calling the standard writer methods; so I
 implemented this method specialized to
 \\fcite{persistent-clos-object}.  The code used here is a modified
 version of \\fcite{shared-initialize (standard-object)}.

 Passing the \\keyarg{suppress-initialization}\\ as \\nonnil\\ will
 suppress the initialization totally; the only place where this
 behavior is needed is within generating the uninitialized prototype
 object, since \\fcite{make-instance}\\ can't be told to suppress
 initialization.

 Unless the \\keyarg{suppress-initialization}\\ argument is passed as
 \\nonnil, the method will always initialize the transient
 representation of the slot.  If \\keyarg{store-cached-slots}\\ is
 passed as \\nonnil\\ (the default), the slot initializations will be
 written into the persistent representation, too.

\\Seealsolabel
 \\Fcite{shared-initialize}."

  (let* ((class (class-of instance))
	 (slots (unless suppress-initialization
		  (class-slots class)))
	 (cached-slots 0))
    (when slots
      (with-transaction
       (p-heap)
       (let ((*default-depth* :cached)
             ;; An objid being non-NIL indicates that the object
             ;; has been loaded from persistent memory, so
             ;; work only on the transient representation:
             (*transient-slot-value* (or *transient-slot-value*
                                         (not (null objid))))
             (persistent-metaclass-p
              (subtypep (class-of class) +persistent-metaclass-class+)))
         (dolist (slot slots)
           (let ((slot-name (slot-definition-name slot))
                 (init-slot-value nil) (init-slot-value-p nil))
             (multiple-value-bind (init-key init-value foundp)
                 (get-properties all-keys (slot-definition-initargs slot))
               (declare (ignore init-key))
               (if foundp
                   (progn
                     (setf init-slot-value-p t)
                     (setf init-slot-value init-value))
                 (let ((initfunction (slot-definition-initfunction slot)))
                   (when (and initfunction
                              (or (eq slot-names t)
                                  (member slot-name slot-names))
                              (not (slot-boundp instance slot-name)))
                     ;; 1997/09/09 HK: Allegro writes a
                     ;; '(function return-nil)
                     ;; into the initfunction slot, which is not
                     ;; funcallable:
                     (unless (or (functionp initfunction)
                                 (not (consp initfunction))
                                 (not (eq (car initfunction) 'function))
                                 (not (fboundp (cadr initfunction))))
                       (setf initfunction
			     (symbol-function (cadr initfunction))))
                     (setf init-slot-value-p t)
                     (setf init-slot-value (funcall initfunction))))))
             (when init-slot-value-p
               (setf (slot-value-using-class class instance slot-name)
		     init-slot-value)
               (let ((slot-extent (if persistent-metaclass-p
                                      (persistent-slot-definition-extent slot)
                                    (slot-description-extent
                                     (find-effective-slot-description
                                      slot-name class)))))
                 (when (eq slot-extent :cached)
                   (incf cached-slots)))))))
	(unless (or *in-bootstrap-p*
		    (null store-cached-slots)
		    (= cached-slots 0))
	  (t-object-to-p-objid instance :flat p-heap)))))
  instance)

;;; ---------------------------------------------------------------------------
;;; Realising my own slot-allocation and access for LispWorks CLOS.
;;; This is done after class finalization and modifies the class metaobject.
;;; ---------------------------------------------------------------------------

(defconstant +standard-accessor-method-class+
  (find-class 'standard-accessor-method)
  #+:lisp-doc "The \\clsmo\\ of class \\class{standard-accessor-method}.")

;;; ---------------------------------------------------------------------------
(defconstant +standard-reader-method-class+
  (find-class 'standard-reader-method)
  #+:lisp-doc "The \\clsmo\\ of class \\class{standard-reader-method}.")

;;; ---------------------------------------------------------------------------
(defconstant +standard-writer-method-class+
  (find-class 'standard-writer-method)
  #+:lisp-doc "The \\clsmo\\ of class \\class{standard-writer-method}.")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defconstant +wrapper-instance-slots-index+ 1
  #+:lisp-doc "
\\Purposelabel
 The index into the class wrapper vector which contains the list
 of instance slot names.
\\Remarkslabel
 \\sysdep{constant}
\\Seealsolabel
 \\Fcite{+wrapper-class-slots-index+}.")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defconstant +wrapper-class-slots-index+ 2
  #+:lisp-doc "
\\Purposelabel
 The index into the class wrapper vector which contains the list
 of the conses used for holding the values of the slots with
 \\lisp{:allocation :class}.
\\Remarkslabel
 \\sysdep{constant}
\\Seealsolabel
 \\Fcite{+wrapper-instance-slots-index+}.")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun patch-class-wrapper
     (the-class list-of-instance-slot-names list-of-class-slot-names)
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isa{\\funarg{list-of-instance-slot-names}}
      {a list of instance slot name symbols naming the instance
       slots of \\funarg{the-class}}
 \\isa{\\funarg{list-of-class-slot-names}}
      {a list of slot name symbols naming the slots of
       \\funarg{the-class} with \\lisp{:allocation :class}}
\\Purposelabel
 Does some \\horrible\\ things with the class wrapper
 of the \\clsmo\\ \\funarg{the-class}\\ necessary to cope with
 some missing \\mop\\ support of \\lw\\ \\clos.

 The list of instance slot names of the class wrapper of class
 \\funarg{the-class}\\ is set to
 \\funarg{list-of-instance-slot-names}.
 The list of slot names of the class wrapper of class
 \\funarg{the-class}\\ with the names of the slots which
 have an \\lisp{:allocation :class}\\ is ordered so that its
 ordering is the same as given by
 \\funarg{list-of-class-slot-names}.

 This is done to keep the \\lw\\ methods
 {\\bf slot-value-using-class (t t t)} and
 {\\bf (setf slot-value-using-class) (t t t)} working despite
 the changes done by \\plob\\ to the \\clsmo;
 they use the ordering established in the list of slot names
 to do a low-level access to an object slot's value.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{(setf slot-extent)};
 \\fcite{patch-class-metaobject};
 \\fcite{patch-class-methods};
 \\fcite{patch-class-slots}."

  (let ((wrapper #+:lispworks3
                 (clos::class-wrapper the-class)
                 #+:lispworks4
                 (low:coerce-record-to-vector
                  (clos::class-wrapper the-class))))
    (setf (svref wrapper +wrapper-instance-slots-index+)
          list-of-instance-slot-names)
    (let ((new-class-var-assoc-list ())
          (class-var-assoc-list (svref wrapper +wrapper-class-slots-index+)))
      ;; The shared-slot assoc dotted pairs have to keep eq:
      (dolist (slot-name list-of-class-slot-names)
        (push-on-end (assoc slot-name class-var-assoc-list)
                     new-class-var-assoc-list))
      (setf (svref wrapper +wrapper-class-slots-index+)
            new-class-var-assoc-list))
    #+:lispworks4
    (setf (clos::class-wrapper the-class)
          (low:coerce-vector-to-record wrapper))
    wrapper))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun patch-class-slots-p (the-class)
  #+:lisp-doc "
\\Purposelabel
 Check if \\funarg{the-class-descr}\\ needs a patch for a correct
 transient representing for persistent objects. A patch is needed
 if at least one slot is declared as being \\lisp{:persistent}\\ and
 therefore not being represented in transient memory."
  (some #'(lambda (slot)
            (eq (slot-description-extent slot) :persistent))
        (class-slots (class-description-of the-class))))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun patch-class-slots (the-class clos-effective-slots plob-effective-slots)
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isa{\\funarg{clos-effective-slots}}
      {a list of \\eff\\ \\sltmo{}s}
 \\isa{\\funarg{plob-effective-slots}}
      {a vector with effective-slot-descriptions}
\\Purposelabel
 Does some \\horrible\\ things with the
 \\clsmo\\ \\funarg{the-class}\\ necessary to cope with some
 missing \\mop\\ support of \\lw\\ \\clos:
 \\begin{itemize}

 \\item The number of transient \\clos\\ slots is decremented for
  each slot with an \\lisp{:extent :persistent}.

 \\item Therefore, the locations of the \\clos\\ slots must be
   adjusted to meet non-existent transient \\clos\\ slots;
   the class wrapper vectors containing the instance-and
   class-slot-names must be rebuild also to match the changed
   locations.

 \\end{itemize}
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{(setf slot-extent)};
 \\fcite{patch-class-metaobject};
 \\fcite{patch-class-methods};
 \\fcite{patch-class-wrapper}."

  (when (patch-class-slots-p the-class)
    (let* ((mismatch-p (not (eql (length clos-effective-slots)
                                 (length plob-effective-slots))))
           (number-of-slots (length clos-effective-slots))
           (instance-slots-head nil) (instance-slots-tail nil)
	   (class-slots-head nil) (class-slots-tail nil)
	   (new-number-of-slots 0))
      (unless mismatch-p
        (dotimes (i number-of-slots)
          (let* ((clos-slot (elt clos-effective-slots i))
                 (slot-name (slot-definition-name clos-slot))
	         (plob-slot (find slot-name
                                  plob-effective-slots
                                  :key #'slot-description-name)))
            (cond
             ((null plob-slot)
              (setf mismatch-p t))
             ((eq (slot-description-extent plob-slot) :persistent)
              (ecase (slot-description-allocation plob-slot)
                (:class
                 (push-on-end slot-name class-slots-tail))
                (:instance
                 (setf (slot-definition-location clos-slot) nil)
                 (push-on-end slot-name instance-slots-tail))))
             (t
              (ecase (slot-description-allocation plob-slot)
                (:class
                 (push-on-end slot-name class-slots-head))
                (:instance
                 (setf (slot-definition-location clos-slot)
                       new-number-of-slots)
                 (incf new-number-of-slots)
                 (push-on-end slot-name instance-slots-head))))))))
      (if mismatch-p
          (setf (mismatch-p the-class) t)
        (progn
          (setf (slot-value the-class 'clos::no-instance-slots)
                new-number-of-slots)
          (patch-class-wrapper the-class
	                       (nconc instance-slots-head instance-slots-tail)
                               (nconc class-slots-head class-slots-tail))))))
  the-class)

;;; ---------------------------------------------------------------------------
#+(or :allegro :lispworks)
(defun patch-class-methods (the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isa{\\funarg{clos-effective-slots}}
      {a list of \\eff\\ \\sltmo{}s}
 \\isa{\\funarg{plob-effective-slots}}
      {a vector with effective-slot-descriptions}
\\Purposelabel
 Does some \\horrible\\ things with the
 \\clsmo\\ \\funarg{the-class}\\ necessary to cope with some
 missing \\mop\\ support of \\lw\\ \\clos.

 For \\clos\\ slots whose corresponding \\plob\\ slot
 has an \\lisp{:extent :persistent}, the reader and writer
 methods are replaced with instances of my own \\mtdmo{}s
 of \\fcite{persistent-reader-method}\\ resp.\\ %
 \\fcite{persistent-writer-method}. These in turn always call
 the \\fcite{slot-value-using-class}\\ resp.\\ %
 the \\fcite{(setf slot-value-using-class)}\\ to access a slot.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{(setf slot-extent)};
 \\fcite{patch-class-metaobject};
 \\fcite{patch-class-slots};
 \\fcite{patch-class-wrapper}."

  (loop for method in
	#+:allegro
	(car (slot-value the-class #-(version>= 6)
				   'clos::direct-methods
				   #+(version>= 6)
				   'excl::direct-methods))
	#-:allegro
	(class-direct-methods the-class)
        as class-of-method = (class-of method)
        as standard-reader-method-p =
        (and (subtypep class-of-method +standard-reader-method-class+)
             (not (subtypep class-of-method
                            +persistent-reader-method-class+)))
        as standard-writer-method-p =
        (and (subtypep class-of-method +standard-writer-method-class+)
             (not (subtypep class-of-method
                            +persistent-writer-method-class+)))
        as qualifiers = (method-qualifiers method)
        as generic-function = (method-generic-function method)
        as new-method-class =
	(cond
	 (qualifiers               nil)
	 (standard-reader-method-p +persistent-reader-method-class+)
	 (standard-writer-method-p +persistent-writer-method-class+))
        as new-method = (when new-method-class
			  (make-instance
                           new-method-class
                           :qualifiers
                           qualifiers
                           :lambda-list
                           (method-lambda-list method)
                           :specializers
                           (method-specializers method)
                           :function #'identity
                           :generic-function
                           generic-function
                           ;;; 2005-04-06 hkirschk: Corrected for
                           ;;; LispWorks 4.4, Allegro 7
                           ;;; #-:lispworks
                           #+(or :lispworks4 :allegro-v7.0) ;; and later
                           :slot-definition
                           ;;; 2005-04-06 hkirschk: Corrected for
                           ;;; LispWorks 4.4, Allegro 7
                           ;;; #-:lispworks
                           #+(or :lispworks4 :allegro-v7.0) ;; and later
                           (accessor-method-slot-definition method)
                           :slot-name
                           (method-slot-name method)
                           :documentation
                           #-(or :lispworks4 :allegro-v7.0)
                           (documentation method)
                           #+(or :lispworks4 :allegro-v7.0)
                           (documentation method t)))
        when new-method
        do
	(setf #+:allegro (slot-value new-method 'function)
	      #-:allegro (method-function new-method)
	      (compile-silent nil
			      (make-accessor-lambda generic-function
						    new-method nil)))
	(add-method generic-function new-method)))

;;; ---------------------------------------------------------------------------
#+(or :allegro :lispworks)
(defun patch-class-metaobject (the-class class-descr)
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
 \\isa{\\funarg{class-descr}}
      {a class-description}
\\Purposelabel
 Does some \\horrible\\ things with the
 \\clsmo\\ \\funarg{the-class}\\ necessary to cope with some
 missing \\mop\\ support of \\lw\\ \\clos:
 the \\horrible\\ functions {\\bf patch-class-methods} and
 {\\bf patch-class-slots} are called when necessary;
 it is necessary if the class represented by
 \\funarg{the-class}\\ has at least one slot.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{patch-class-methods};
 \\fcite{patch-class-slots};
 \\fcite{patch-class-wrapper}."

  ;; 1998/02/01 HK: Debug:
  ;; (format t "Patching class metaobject ~A~%" the-class)
  (let* ((plob-effective-slots (class-slots class-descr))
         (number-of-slots (length plob-effective-slots)))
    (when (> number-of-slots 0)
      (patch-class-methods the-class)
      #+:lispworks
      (patch-class-slots the-class (class-slots the-class)
      			 plob-effective-slots)))
  the-class)

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod finalize-inheritance :before ((the-class persistent-metaclass))
  #+:lisp-doc
  "Finalize the direct superclasses of \\funarg{the-class}\\ (looks
 as if LispWorks forgets to do so)."
  (mapc #'finalize-inheritance (class-direct-superclasses the-class)))

;;; ---------------------------------------------------------------------------
(defun load-shared-slots (the-class)
  #+:lisp-doc
  "Load all shared slots after a class has been established."
  (let ((prototype nil) (prototype-objid nil))
    (loop for slot in (class-slots the-class)
          as slot-allocation = (persistent-slot-definition-allocation slot)
          as slot-extent = (persistent-slot-definition-extent slot)
          when (and (eq slot-allocation :class)
                    (not (eq slot-extent :transient)))
          do
          (unless prototype
            (let ((class-description
                   (ensure-class-description (class-name the-class))))
            (setf prototype (class-prototype class-description))
            (setf prototype-objid (persistent-object-objid prototype))))
          (let ((slot-name (slot-definition-name slot))
                (shared-value-boundp nil) (shared-value nil))
            (let ((*transient-slot-value* nil))
              (declare (special *transient-slot-value*))
              (setf shared-value-boundp
                    (persistent-slot-boundp-using-class
                     the-class prototype-objid slot-name nil))
              (when shared-value-boundp
                (setf shared-value
                      (persistent-slot-value-using-class
                       the-class prototype-objid slot-name nil))))
            (let ((*transient-slot-value* t))
              (declare (special *transient-slot-value*))
              (if shared-value-boundp
                  (setf (slot-value prototype slot-name) shared-value)
                (slot-makunbound prototype slot-name)))))))

;;; ---------------------------------------------------------------------------
(defmethod finalize-inheritance :after ((the-class persistent-metaclass))
  #+:lisp-doc
  "Mark the class description of \\funarg{the-class}\\ as
 being not finalized."
  (setf (class-description-finalized-p the-class) nil))

;;; ---------------------------------------------------------------------------
(defmethod finalize-class-description ((class-descr class-description))
  #+:lisp-doc
  "Patch-as-patch-can: Call \\fcite{patch-class-metaobject}\\ and
 \\fcite{patch-class-methods}\\ so that \\lw\\ does a slot allocation
 and access in the sense of \\plob."
  (clrhash (class-description-name->slot-cache class-descr))
  (unless (rassoc class-descr plob::*plob-base-objects*)
    #+(or :allegro :lispworks)
    (let ((the-class (class-description-class class-descr)))
      #+:lispworks
      (patch-class-metaobject the-class class-descr)
      (patch-class-methods the-class))))

;;; ---------------------------------------------------------------------------
(defmethod finalize-class-description :around ((class-descr class-description))
  (let ((the-class (class-description-class class-descr)))
    (unless the-class
      (setf the-class (find-class (class-name class-descr)))
      (setf (class-description-class class-descr) the-class))
    (unless (class-description-finalized-p the-class)
      (setf (class-description-finalized-p the-class) t)
      (when (next-method-p)
        (call-next-method)))
    ;; Load the persistent slots with :allocation :class.
    (unless *in-bootstrap-p*
      (load-shared-slots the-class)))
  nil)

;;; ---------------------------------------------------------------------------
(defmethod make-instance :around ((the-class persistent-metaclass)
                                  &rest initargs)
  #+:lisp-doc "
 Finalize the class-description of
 \\funarg{the-class}\\ and call the next method."
  (declare (ignore initargs))
  (unless (or *in-bootstrap-p*
              (class-description-finalized-p the-class))
    (finalize-class-description
     (ensure-class-description (class-name the-class))))
  (call-next-method))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun signal-obsolete-instance-error (class)
  #+:lisp-doc "
\\Argumentslabel
 \\isacls{\\funarg{class}}
\\Purposelabel
 Signal an obsolete instance error; called always from
 \\fcite{obsolete-instance-trap-internal (persistent-metaclass t t)}.
\\Seealsolabel
 \\Fcite{obsolete-instance-trap-internal (persistent-metaclass t t)}."

  ;;(error "Sorry, cannot access old persistent instance of class ~A."
  ;;	   class)
  (warn "Trying to access old persistent instance of class ~A."
        class)
  )

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod clos::obsolete-instance-trap-internal
     ((class persistent-metaclass) oinstance owrapper
      #+:lispworks4 ;; and later
      special-wrapper)
  #+:lisp-doc "
 This is the obsolete-instance-trap-internal for instances of
 \\fcite{persistent-clos-object}; it is called each time
 \\clos\\ detects an out-of-date instance.
 The \\lw\\ method
 {\\bf obsolete-instance-trap-internal (standard-class t t)}
 cannot be used because \\plob\\ modifies a \\clsmo\\ with persistent
 instances so that slots with \\lisp{:extent :persistent} are
 not allocated by \\clos\\ and therefore are also not direct
 accessible by \\clos; the mentioned method uses fairly low-level
 accessing functions which do not know about the modifications done
 by \\plob\\ and will crash \\lw\\ when called.

 The right solution would be to compute the discarded slots here in
 this method, re-allocate an \\clos\\ instance vector, modify the
 \\funarg{oinstance}\\ to use the new re-allocated instance vector,
 fill this new vector from the old one and call
 \\fcite{update-instance-for-redefined-class}. Since some
 of these steps involve calling low-low-low-level
 \\lw\\ \\clos\\ functions, I ommitted this for now here; only an
 error is signalled.

 Since there might be old instances in the cache,
 the cache is cleared too; so this method is perhaps not
 called any more because the offending instance was only
 found in the cache.
\\Seealsolabel
 \\Fcite{signal-obsolete-instance-error}."

  (declare (ignore oinstance owrapper))
  (let ((signal-error (patch-class-slots-p class))
        (*transient-slot-value* t))
    (declare (special *transient-slot-value*))
    (if signal-error
        (progn
          (clear-cache)
          (signal-obsolete-instance-error class)
          (call-next-method))
      (call-next-method))))

;;; ---------------------------------------------------------------------------
(defmethod update-instance-for-redefined-class :before
  ((instance persistent-clos-object) added-slots discarded-slots property-list
   &rest initargs)
  #+:lisp-doc
  "Promotes the update of the transient \\funarg{instance}\\ to 
 its persistent companion in the \\sh."
  (declare (ignore added-slots discarded-slots property-list initargs))
  (let* ((the-class (class-of instance))
         (class-descr (class-description-of the-class))
         (p-objid (persistent-object-objid instance)))
    (unless (class-description-finalized-p the-class)
      (finalize-class-description class-descr))
    (when p-objid
      (update-instance-for-redefined-description
       (p-class-description (p-instance-class-wrapper p-objid :objid)
			    :cached)
       class-descr p-objid))))

;;; ---------------------------------------------------------------------------
(defmethod update-class :after
  ((class-descr class-description)
   &optional (p-heap *default-persistent-heap*))
  (declare (ignore p-heap))
  (setf (p-dependent (p-class-description-direct-slots class-descr :objid))
	:read)
  (setf (p-dependent (p-class-description-effective-slots class-descr :objid))
	:read))


;;;; Local variables:
;;;; buffer-file-coding-system: raw-text-unix
;;;; End:
