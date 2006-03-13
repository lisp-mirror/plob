;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-metaclass-3.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	1997/03/11
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

(use-package :clos)

;;; ---------------------------------------------------------------------------
(defconstant +persistent-direct-slot-definition-class+
  (find-class 'persistent-direct-slot-definition)
  #+:lisp-doc
  "The \\clsmo\\ of \\fcite{persistent-direct-slot-definition}.")

;;; ---------------------------------------------------------------------------
(defmethod initialize-instance :around
  ((slot persistent-direct-slot-definition)
   &rest initargs
   &key (extent nil extentp) (deferred nil deferredp)
	(index nil indexp) (location nil locationp))
  #+:lisp-doc
  "Stores the values of the extra slot options."
  (let ((slot-definition (call-next-method)))
    (when extentp
      (setf (persistent-slot-definition-extent slot-definition) extent))
    (when deferredp
      (setf (persistent-slot-definition-deferred slot-definition) deferred))
    (when indexp
      (setf (persistent-slot-definition-index slot-definition) index))
    (when locationp
      (setf (persistent-slot-definition-location slot-definition) location))
    slot-definition))

;;; ---------------------------------------------------------------------------
(defconstant +persistent-effective-slot-definition-class+
  (find-class 'persistent-effective-slot-definition)
  #+:lisp-doc "The \\clsmo\\ of \\fcite{persistent-effective-slot-definition}.")

;;; ---------------------------------------------------------------------------
(defmethod direct-slot-definition-class ((class persistent-metaclass)
					 #-(or :lispworks4.0 :lispworks4.1 :lispworks4.2)
					 &rest
                                         initargs)
  #+:lisp-doc
  "Returns \\fcite{persistent-direct-slot-definition}."
  (declare (ignore initargs))
  +persistent-direct-slot-definition-class+)

;;; ---------------------------------------------------------------------------
(defmethod effective-slot-definition-class ((class persistent-metaclass)
                                            #-(or :lispworks4.0 :lispworks4.1 :lispworks4.2) &rest initargs)
  #+:lisp-doc
  "Returns \\fcite{persistent-effective-slot-definition}."
  (declare (ignore initargs))
  +persistent-effective-slot-definition-class+)

;;; ---------------------------------------------------------------------------
(defconstant +standard-object-class+
  (find-class 'standard-object)
  #+:lisp-doc
  "The \\clsmo\\ of class \\class{standard-object}.")

;;; ---------------------------------------------------------------------------
(defconstant +persistent-metaclass-class+
  (find-class 'persistent-metaclass)
  #+:lisp-doc
  "The \\clsmo\\ of \\fcite{persistent-metaclass}.")

;;; ---------------------------------------------------------------------------
(defconstant +persistent-clos-object-class+
  (let ((the-class (find-class 'persistent-clos-object nil)))
    (if the-class
        the-class
      (ensure-class 'persistent-clos-object
                    :metaclass 'persistent-metaclass)))
  #+:lisp-doc
  "The \\clsmo\\ of \\fcite{persistent-clos-object}.")

;;; ---------------------------------------------------------------------------
(defconstant +plob-description-class+
  (let ((the-class (find-class 'plob-description nil)))
    (if the-class
        the-class
      (ensure-class 'plob-description :metaclass 'persistent-metaclass)))
  #+:lisp-doc
  "The \\clsmo\\ of \\fcite{plob-description}.")

;;; ---------------------------------------------------------------------------
(defconstant +class-description-class+
  (let ((the-class (find-class 'class-description nil)))
    (if the-class
        the-class
      (ensure-class 'class-description :metaclass 'persistent-metaclass)))
  #+:lisp-doc
  "The \\clsmo\\ of \\fcite{class-description}.")

;;; ---------------------------------------------------------------------------
;;; Add class persistent-clos-object to the list of superclasses for each
;;; class which has a :metaclass persistent-metaclass (see [AP91], p. 24):
;;; ---------------------------------------------------------------------------
(defun re-and-initialize-persistent-metaclass-instance
    (class direct-superclasses
     constructor dependent extent schema-evolution
     call-next-method all-keys)
  #+:lisp-doc "
 \\Argumentslabel
  \\isacls{class}
  \\isa{\\funarg{direct-superclasses}}
       {a list of \\clsmo{}s.}
  \\isa{\\funarg{call-next-method}}
       {a function.}
  \\isa{\\funarg{all-key}}
       {a keyword argument list}
 \\Purposelabel
  Stores the values of the extra class options and inserts
  \\fcite{persistent-clos-object}\\ into the list of the class
  direct' superclasses.
 \\Remarkslabel
  Hint for \\allegro:
  The arguments of extra class options are put into a list by \\allegro;
  in \\lw, they are passed `plain' (so for \\lw, a class option can have
  only one argument).

  1998/11/19: Jon Dyte had the idea to put the code of this function
  into \\fcite{shared-initialize}\\ specialized to
  \\fcite{persistent-metaclass}; this does not work in \\allegrocl\\ 5.x,
  the list of direct-superclasses will not be extended.
 \\Seealsolabel
  \\Fcite{initialize-instance (persistent-metaclass)};
  \\fcite{reinitialize-instance (persistent-metaclass)}."

  ;; Store the extent and schema-evolution option:
  (setf (class-constructor class)
    #+:lispworks constructor
    #-:lispworks (car constructor))
  (setf (class-dependent class)
    #+:lispworks dependent
    #-:lispworks (car dependent))
  (setf (class-extent class)
    #+:lispworks extent
    #-:lispworks (car extent))
  (setf (schema-evolution class)
    (if #+:lispworks schema-evolution #-:lispworks (car schema-evolution)
	#+:lispworks schema-evolution #-:lispworks (car schema-evolution)
	*default-clos-schema-evolution*))
  ;; If (eq class +persistent-clos-object-class+), (call-next-method)
  ;; because a class should not have itself as its own superclass,
  ;; especially not in LispWorks because this causes an infinite recursion
  ;; in compute-class-precedence-list:
  (if (or (eq class +persistent-clos-object-class+)
	  (some #'(lambda (super)
		    (eq (class-of super)
			+persistent-metaclass-class+))
		direct-superclasses))
      (funcall call-next-method)
    (apply call-next-method class
	   #+:lispworks3 :superclasses
	   #-:lispworks3 :direct-superclasses
	   (if (eq (car direct-superclasses)
		   +standard-object-class+)
	       (list +persistent-clos-object-class+)
	     (append direct-superclasses
		     (list +persistent-clos-object-class+)))
	   all-keys)))

;;; ---------------------------------------------------------------------------
(defmethod initialize-instance #-:lispworks :around
  ((class persistent-metaclass) &rest all-keys
   &key ((#+:lispworks3 :superclasses #-:lispworks3 :direct-superclasses
		        direct-superclasses))
   constructor dependent extent schema-evolution)
  #+:lisp-doc 
  "Trapped to \\fcite{re-and-initialize-persistent-metaclass-instance}."
  (re-and-initialize-persistent-metaclass-instance
   class direct-superclasses
   constructor dependent extent schema-evolution
   #'call-next-method all-keys))

;;; ---------------------------------------------------------------------------
(defmethod reinitialize-instance #-:lispworks :around
  ((class persistent-metaclass) &rest all-keys
   &key ((#+:lispworks3 :superclasses #-:lispworks3 :direct-superclasses
		      direct-superclasses))
   constructor dependent extent schema-evolution)
  #+:lisp-doc
  "Trapped to \\fcite{re-and-initialize-persistent-metaclass-instance}."
  (re-and-initialize-persistent-metaclass-instance
   class direct-superclasses 
   constructor dependent extent schema-evolution
   #'call-next-method all-keys))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-extent) (extent
                               (slot-definition
				persistent-direct-slot-definition)
                               the-class)
  #+:lisp-doc "Stores the \\funarg{extent}\\ in slot {\\bf t-extent} of
 \\funarg{slot-definition}."
  (declare (ignore the-class))
  (setf (persistent-slot-definition-extent slot-definition) extent))

;;; ---------------------------------------------------------------------------
(defmethod slot-extent ((slot-definition persistent-direct-slot-definition)
                        the-class)
  #+:lisp-doc "
 Returns the extent stored in slot {\\bf t-extent} of
 \\funarg{slot-definition}."
  (declare (ignore the-class))
  (persistent-slot-definition-extent slot-definition))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-extent) (extent
                               (slot-definition
				persistent-effective-slot-definition)
                               the-class)
  (declare (ignore the-class))
  (setf (persistent-slot-definition-extent slot-definition) extent))

;;; ---------------------------------------------------------------------------
(defmethod slot-extent ((slot-definition persistent-effective-slot-definition)
                        the-class)
  (declare (ignore the-class))
  (persistent-slot-definition-extent slot-definition))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-deferred)
     (deferred
      (slot-definition persistent-direct-slot-definition)
      the-class)
  #+:lisp-doc "
 Stores the \\funarg{deferred}\\ in slot {\\bf t-deferred} of
 \\funarg{slot-definition}."
  (declare (ignore the-class))
  (setf (persistent-slot-definition-deferred slot-definition) deferred))

;;; ---------------------------------------------------------------------------
(defmethod slot-deferred ((slot-definition persistent-direct-slot-definition)
                          the-class)
  #+:lisp-doc "
 Returns the deferred stored in slot {\\bf t-deferred} of
 \\funarg{slot-definition}."
  (declare (ignore the-class))
  (persistent-slot-definition-deferred slot-definition))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-deferred)
     (deferred
      (slot-definition persistent-effective-slot-definition)
      the-class)
  (declare (ignore the-class))
  (setf (persistent-slot-definition-deferred slot-definition) deferred))

;;; ---------------------------------------------------------------------------
(defmethod slot-deferred
     ((slot-definition persistent-effective-slot-definition)
      the-class)
  (declare (ignore the-class))
  (persistent-slot-definition-deferred slot-definition))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-location)
     (location
      (slot-definition persistent-direct-slot-definition)
      the-class)
  #+:lisp-doc "
 Stores the \\funarg{location}\\ in slot {\\bf t-location} of
 \\funarg{slot-definition}."
  (declare (ignore the-class))
  (setf (persistent-slot-definition-location slot-definition) location))

;;; ---------------------------------------------------------------------------
(defmethod slot-location ((slot-definition persistent-direct-slot-definition)
                          the-class)
  #+:lisp-doc "
 Returns the location stored in slot {\\bf t-location} of
 \\funarg{slot-definition}."
  (declare (ignore the-class))
  (persistent-slot-definition-location slot-definition))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-location)
     (location
      (slot-definition persistent-effective-slot-definition)
      the-class)
  (declare (ignore the-class))
  (setf (persistent-slot-definition-location slot-definition) location))

;;; ---------------------------------------------------------------------------
(defmethod slot-location
     ((slot-definition persistent-effective-slot-definition)
      the-class)
  (declare (ignore the-class))
  (persistent-slot-definition-location slot-definition))

;;; ---------------------------------------------------------------------------
(defmethod persistent-slot-definition-location
    ((slot-definition standard-effective-slot-definition))
  nil)
    
;;; ---------------------------------------------------------------------------
(defconstant +plob-slot-write-through-extents+
    '(:cached-write-through :persistent :object :objid)
  #+:lisp-doc "
 \\Purposelabel
  List with slot extents which are handled as write-through slots. The
  elements in \\lisp{+plob-slot-write-through-extents+}\\ must be a
  subset of the values of the list in \\fcite{+plob-slot-extents+}.
 \\Seealsolabel
  \\Fcite{+plob-slot-extents+};
  \\fcite{+plob-slot-represented-extents+}.")

;;; ---------------------------------------------------------------------------
(defun slot-write-through-extent-p (slot-extent)
  #+:lisp-doc "
 Check if \\funarg{slot-extent}\\ names a slot extent which is handled
 as write-through."
  (member slot-extent +plob-slot-write-through-extents+))

;;; ---------------------------------------------------------------------------
(defmethod compute-effective-slot-definition :around
  ((the-class persistent-metaclass) slot-name direct-slot-definitions)
  #+:lisp-doc
  "Handles the extra allowed slot options."
  ;; Let default system do its work first:
  (let ((slot-definition (call-next-method))
        (effective-extent nil)
        (effective-deferred nil)
        (effective-index nil)
	(effective-location nil))
    ;; Now check if an index is requested for the slot:
    (block nil
      (map nil
           #'(lambda (direct-slot)
               (let ((index (persistent-slot-definition-index direct-slot)))
                 (cond
                  ((and index effective-index
                        (not (equal index effective-index)))
		   (cerror "Do not create an index on the slot."
                           "Index ~A is incompatible ~
			    with existing index ~A ~
			    requested for slot ~A of class ~A."
                           effective-index index slot-name the-class)
                   (setf effective-index nil)
                   (return))
                  (index
		   (setf effective-index index)))))
           direct-slot-definitions))
    ;; Now check if the effective slot should be made persistent;
    ;; this is done iff at least one direct slot definition declares
    ;; the slot as non-transient:
    (block nil
      (map nil
           #'(lambda (direct-slot)
               (let ((extent (slot-extent direct-slot the-class)))
                 (cond
                  ((eq extent :persistent)
		   (setf effective-extent extent))
                  ((or (eq extent :cached)
                       (eq extent :cached-write-through))
		   (setf effective-extent extent)
                   (return))
                  (t
                   (unless effective-extent
                     (setf effective-extent extent))))))
           direct-slot-definitions))
    (when (and (not effective-extent)
	       (find slot-name (slot-value the-class
					   #-(and :allegro (version>= 6))
					   'clos::direct-slots
					   #+(and :allegro (version>= 6))
					   'excl::direct-slots)
		     :key #'slot-definition-name))
      (setf effective-extent (class-extent the-class)))
    (unless effective-extent
      (setf effective-extent *default-plob-slot-extent*))
    (when (and effective-index
	       (not (member effective-extent
			    +plob-slot-write-through-extents+)))
      (cerror (format nil "Set the slot's :extent to ~A."
		      (car +plob-slot-write-through-extents+))
              "Cannot create index ~S on slot ~A of class ~A because the slot's :extent is ~S; allowed for indexed slots are ~A."
	      effective-index slot-name the-class effective-extent
	      +plob-slot-write-through-extents+)
      (setf effective-extent (car +plob-slot-write-through-extents+)))
    (setf (persistent-slot-definition-index slot-definition)
          effective-index)
    (setf (persistent-slot-definition-extent slot-definition)
          effective-extent)
    ;; Now check if the effective slot should be made deferred:
    (map nil
	 #'(lambda (direct-slot)
	     (let ((deferred (slot-deferred direct-slot the-class)))
               (if (or (not deferred)
		       (numberp deferred))
	           (when (and (numberp deferred)
                              (or (not effective-deferred)
		                  (< effective-deferred deferred)))
		     (setf effective-deferred deferred))
                 (cerror "Ignore this :deferred slot option."
                         "Found non-canonic :deferred slot option ~A for slot ~A. Allowed is only NIL or a number."
                         deferred slot-name))))
	 direct-slot-definitions)
    (setf (persistent-slot-definition-deferred slot-definition)
          effective-deferred)
    ;; Scan through the direct slot definitions for a location option:
    (map nil
      #'(lambda (direct-slot)
	  (let ((location (slot-location direct-slot the-class)))
	    (cond
	     ((and location effective-location
		   (not (eql location effective-location)))
	      (when (and *verbose* (>= *verbose* 1))
		(cerror "Use the more specific location"
			"Found more than one :location slot option.")))
	     (location
	      (setf effective-location location)))))
      direct-slot-definitions)
    ;; Evaluate the effective-location once:
    (setf (persistent-slot-definition-location slot-definition)
          (if (symbolp effective-location)
              (symbol-value effective-location)
            effective-location))
    (setf (persistent-slot-definition-allocation slot-definition)
          (slot-definition-allocation slot-definition))
    ;; #$&^%%$ LispWorks: Only :class and :instance are allowed as
    ;; slot-definition-allocation (see [MOP91], p. 99); all other
    ;; allocation types signal an error.
    ;; Holds for Allegro, too.
    ;; Allegro has no (setf slot-definition-allocation) at all.
    #-(or :lispworks :allegro)
    (when (eq effective-extent :persistent)
      (setf (slot-definition-allocation slot-definition) effective-extent))
    slot-definition))

;;; ---------------------------------------------------------------------------
(defconstant +persistent-reader-method-class+
  (find-class 'persistent-reader-method)
  #+:lisp-doc
  "The \\clsmo\\ of \\fcite{persistent-reader-method}.")

;;; ---------------------------------------------------------------------------
(defconstant +persistent-writer-method-class+
  (find-class 'persistent-writer-method)
  #+:lisp-doc
  "The \\clsmo\\ of \\fcite{persistent-writer-method}.")

;;; ---------------------------------------------------------------------------
;;; Since Allegro and LispWorks don't support the generic function
;;; make-method-lambda, the persistent reader and writer methods are patched
;;; into the class metaobject when the class description is finalized;
;;; the indicator if the patch is done or not is if the methods metaobject's
;;; class is persistent-reader-method resp. persistent-writer-method,
;;; so never use these classes as predefined accessor methods.
;;; If make-method-lambda is supported, the code from patch-class-methods
;;; should be moved into the methods of make-method-lambda specialized to
;;; persistent-reader resp. persistent-writer-method.
;;; ---------------------------------------------------------------------------
#-(or :allegro :lispworks)
(defmethod clos::reader-method-class ((the-class persistent-metaclass)
                                      direct-slot &rest initargs)
  #+:lisp-doc
  "Returns \\fcite{persistent-reader-method}."
  +persistent-reader-method-class+)

;;; ---------------------------------------------------------------------------
#-(or :allegro :lispworks)
(defmethod clos::writer-method-class ((the-class persistent-metaclass)
                                      direct-slot &rest initargs)
  #+:lisp-doc
  "Returns \\fcite{persistent-writer-method}."
  +persistent-writer-method-class+)

;;; ---------------------------------------------------------------------------
(defgeneric make-accessor-lambda (generic-function method environment)
  #+:lisp-doc
  (:documentation "
 \\Argumentslabel
  \\isa{\\funarg{generic-function}}
       {a generic function metaobject}
  \\isa{\\funarg{method}}
       {a method metaobject of a subclass of
        \\class{standard-accessor-method}}
 \\Valueslabel
  Returns a $\\lambda$-expression to use as method function for
  \\funarg{method}.
 \\Purposelabel
  Make a $\\lambda$-expression for a slot accessor method.
  The code returned is intended to be used for accessing
  an instance slot. The $\\lambda$-expression returned
  takes one argument (an instance) for reader-methods
  and two arguments (a new value and an instance) for
  writer methods.

  Similar to \\fcite{make-method-lambda}."))

;;; ---------------------------------------------------------------------------
(defun find-effective-slot-description (slot-or-name the-class)
  #+:lisp-doc "
 \\Argumentslabel
  \\isa{\\funarg{slot-or-name}}
       {a slot or a symbol naming an effective slot of \\funarg{the-class}}
  \\isacls{\\funarg{the-class}}
 \\Purposelabel
  Returns the effective slot description object of the slot
  named \\funarg{slot-or-name}\\ in \\funarg{the-class}.
 \\Seealsolabel
  \\Fcite{find-effective-slot};
  \\fcite{find-effective-slot-description-by-objid};
  \\fcite{find-slot-description}."

  (declare (inline find-effective-slot-description))
  (unless *in-bootstrap-p*
    (let ((slot-name (if (symbolp slot-or-name)
			 slot-or-name
		       (slot-definition-name slot-or-name)))
	  (class-description (class-description-of the-class)))
      (unless class-description
        (setf class-description
              (ensure-class-description (class-name the-class))))
      (let* ((name->slot-cache
	      (class-description-name->slot-cache class-description))
	     (effective-slot-description (gethash slot-name name->slot-cache)))
        (unless effective-slot-description
          (let ((effective-slots
	         (class-slots class-description)))
	    (setf effective-slot-description
	          (find slot-name
		        effective-slots
		        :test #'eq
		        :key #'slot-definition-name)))
          (when effective-slot-description
	    (setf (gethash slot-name name->slot-cache)
		  effective-slot-description)))
	effective-slot-description))))

;;; ---------------------------------------------------------------------------
#-:lispworks
(defmethod method-slot-name ((method standard-accessor-method))
  (slot-definition-name (accessor-method-slot-definition method)))

;;; ---------------------------------------------------------------------------
(defun make-effective-slot-lambda (the-class slot-name)
  #+:lisp-doc "
 \\Argumentslabel
  \\isacls{\\funarg{the-class}}
  \\isa{\\funarg{slot-name}}
       {a symbol naming a slot of \\funarg{the-class}}
 \\Purposelabel
  Returns a $\\lambda$-expression which returns the effective slot
  definition metaobject of the slot named
  \\funarg{slot-name}\\ in \\funarg{the-class}\\ when called.
  The returned $\\lambda$-expression is very similar to the body
  of the \\fcite{find-effective-slot-description}.
 \\Seealsolabel
  \\Fcite{find-effective-slot-description}."

  `(let ((class-description (class-description-of ,the-class)))
    (unless class-description
      (setf class-description
            (ensure-class-description (class-name the-class))))
    (let* ((name->slot-cache
	    (class-description-name->slot-cache class-description))
	   (effective-slot-description (gethash ,slot-name name->slot-cache)))
      (unless effective-slot-description
	(let ((effective-slots
	       (class-slots class-description)))
	  (setf effective-slot-description
		(find ,slot-name
		      effective-slots
		      :test ,#'eq
		      :key ,#'slot-definition-name)))
	(when effective-slot-description
	  (setf (gethash ,slot-name name->slot-cache)
		effective-slot-description)))
      effective-slot-description)))

;;; ---------------------------------------------------------------------------
(defmethod make-accessor-lambda ((generic-function standard-generic-function)
                                 (method persistent-reader-method)
                                 environment)
  (declare (ignore environment))
  `(lambda (object)
     (let* ((the-class (class-of object))
            (slot-name (quote ,(method-slot-name method)))
            (effective-slot-description
             ,(make-effective-slot-lambda 'the-class 'slot-name)))
       (if effective-slot-description
           (slot-value-using-class the-class object
                                   effective-slot-description)
	 (slot-value-using-class the-class object slot-name)))))
                                                 
;;; ---------------------------------------------------------------------------
(defmethod make-accessor-lambda ((generic-function standard-generic-function)
                                 (method persistent-writer-method)
                                 environment)
  (declare (ignore environment))
  `(lambda (new-value object)
     (let* ((the-class (class-of object))
            (slot-name (quote ,(method-slot-name method)))
            (effective-slot-description
             ,(make-effective-slot-lambda 'the-class 'slot-name)))
       (if effective-slot-description
           (setf (slot-value-using-class
	          the-class object effective-slot-description)
	         new-value)
         (setf (slot-value-using-class the-class object slot-name)
               new-value)))))

;;; ---------------------------------------------------------------------------
(defmethod make-method-lambda
  ((generic-function standard-generic-function)
   (method persistent-reader-method)
   lambda-list declarations body &optional environment)
  #+:lisp-doc "
 \\allegro\\ 4.3 and \\lw\\ 3.1.1 do not support this method for
 reader methods, i.e.\\ this method will now never be called;
 it is left here for future expansion."
  (call-next-method))

;;; ---------------------------------------------------------------------------
(defmethod make-method-lambda
     ((generic-function standard-generic-function)
      (method persistent-writer-method)
      lambda-list declarations body &optional environment)
  #+:lisp-doc "
 \\allegro\\ 4.3 and \\lw\\ 3.1.1 do not support this method for
 writer methods, i.e.\\ this method will now never be called;
 it is left here for future expansion."
  (call-next-method))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
