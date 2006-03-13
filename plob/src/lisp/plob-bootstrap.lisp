;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-bootstrap.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	7.2.94
;;;; Description	PLOB bootstrap
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
;;; Macro reader character
;;; ---------------------------------------------------------------------------
(defun persistent-symbol-reader (stream subchar arg)
  #+:lisp-doc "
\\Argumentslabel
 As specified for the \\funarg{function}\\ argument of
 \\fcite{set-dispatch-macro-character}.
\\Purposelabel
 This function handles the macro reader character \\#! (hash sign
 followed by an exclamation mark).

 The identifier following the macro character is interpreted as the
 name of a persistent symbol; the expansion returned is
 \\begin{quote}\\tt
  (p-symbol-value \\textsl{\\lt{}persistent-symbol-name\\gt}\\/)
 \\end{quote}
 This form can be used to read the value of a persistent symbol;
 the \\lisp{setf}-form can be used to set the value of a persistent
 symbol; see section \\fcite{symbol ...}\\ for examples.
\\Seealsolabel
 Section \\fcite{symbol ...};
 \\fcite{set-dispatch-macro-character}."

  (declare (ignore subchar arg))
  (let ((items (read stream t nil t)))
    (cond
     ((consp items)
      (let ((item (car items))
            (depth (cadr items))
            (p-heap (caddr items)))
        (cond
         ((eq item 'quote)
          (setf item depth)
          (setf depth p-heap)
          (setf p-heap nil))
         ((and (consp item) (eq (car item) 'quote))
          (setf item (cadr item))))
        `(p-symbol-value (quote ,item)
                         ,(if depth
			       depth
			     (quote *default-depth*))
                         ,(if p-heap
			      p-heap
			    (quote *default-persistent-heap*)))))
     (t
      `(p-symbol-value (quote ,items) *default-depth*
		       *default-persistent-heap*)))))

;;; ---------------------------------------------------------------------------
(set-dispatch-macro-character #\# #\! #'plob::persistent-symbol-reader)
#+:allegro
(setf excl::std-lisp-readtable *readtable*)
#+:allegro
(setf (excl:named-readtable :plob-readtable) *readtable*)

;;; ---------------------------------------------------------------------------
;;; Process local variables
;;; ---------------------------------------------------------------------------
(defun make-process-heap ()
  #+:lisp-doc "
\\Purposelabel
 Lightweight process variable initialization.
"
  (make-persistent-heap))

;;; ---------------------------------------------------------------------------
(make-process-variable '*default-persistent-heap*
		       '(plob::make-process-heap)
		       t)

;;; ---------------------------------------------------------------------------
(defun bootstrap-clear-cache ()
  #+:lisp-doc "
 Like \\fcite{clear-cache}\\ plus the list of base objects is deleted."
  (setf *plob-base-objects* ())
  (clear-cache))

;;; ---------------------------------------------------------------------------
#+(and :lisp-doc :document-api)
(:defdoc
 "bootstrap ..."
 "Bootstrap Issues"
 "
 \\subsubsection*{Why Bootstrapping at all?}

 %% Hmpf, this sentence is perhaps too silly ...
 % Of course, the \\cl\\ system and \\clos\\ are already bootstrapped
 % when \\plob\\ is started.
 The bootstrap meant here concerns the
 persistent objects contained in the \\sh. The class of each persistent
 object in the \\sh\\ is either built-in or it is represented by its
 {\\sl structure} resp.\\ {\\sl class description object}, analogous to
 \\cl\\ and \\clos\\ where each object's class is either built-in or
 represented by its \\clsmo.\\footnote{\\cl\\ structure classes are also
 represented, but there is no standard defined for them; so they are more
 ore less hidden in one of the system packages.}

 The bootstrap problems arise from the fact that the description objects
 are either described by themself or contain references to
 other objects whose transitive closure w.r.t.\\ contained references
 to other objects leads again to themself,
 e.g.\\
 \\begin{itemize}

 \\item Persistent structures are described by persistent objects
  of \\fcite{structure-description}, i.e.\\ the description of the
  object representing the \\fcite{structure-description}\\ points
  to itself.

 \\item The names of the structures (and of its slots too) are
  persistent symbols (these are built-in to \\plob\\ and therefore
  need no special bootstrap handling therefore) which belong
  to persistent packages which are in turn persistent structures.

 \\item The slots of structures are described by persistent
  structure objects of \\fcite{structure-slot-description}.

 \\end{itemize}
 The way to break this circularity of self-describing
 resp.\\ -referencing objects for bootstrapping is to use
 `low-level' functions:
 \\begin{description}

 \\item [Object creation]
  This is done by directly calling the class-specific allocation
  function; since I know exactly what objects have to be created
  to get the system up, there is no need to use the description
  of the object for its creation.

 \\item [Object access]
  Similary, object access is done by directly calling the
  class-specific accessor functions.

 \\end{description}
 The bootstrap has to be done only for very special objects like
 the \\clsdo[s]\\ of the \\clsdc[es]\\ themself.

 \\subsubsection*{Storing the description objects themself}

 The very first step to be done for using the \\sh\\ at all is to store
 the descriptions of the describing objects themself in the initially empty
 \\sh; this process is called `formatting the root' and is accomplished
 by the \\fcite{format-plob-root}.
 The problems which occure in formatting
 are the above mentioned typical bootstrap problems and are solved
 as indicated:\\ the description objects are build up as transient
 objects; afterwards, they are dumped out to the \\sh\\ by creating
 the persistent objects by calling low-level allocation functions
 and saving their states by calling low-level writer functions.
 The formatting proceeds in phases, where each phase adds a more
 complicated type description to the \\sh, e.g.\\ in the first phase
 only immediate or simple objects can be stored (since no description
 objects are in the \\sh\\ in the first phase) whereas after finishing
 the last phase all objects can be saved to the \\sh\\ (since all
 description objects are in the \\sh\\ after the last phase).

 This formatting is done exactly once (except the \\sh\\ is in an
 inconsistent state which makes a re-formatting necessary by calling
 the \\fcite{format-plob-root}\\ again).

 \\subsubsection*{Loading the description objects}

 After the root is formatted, the \\sh\\ must be opened for using
 it; this process is called `loading the root' and is accomplished
 by the \\fcite{open-heap}. Since the transient description
 objects should match exactly their persistent description objects
 found in the
 \\sh\\ (and not the other way round), there is again a bootstrap
 problem. This is solved by reading the description objects from
 the \\sh\\ in phases by using low-level read functions.
 Similar to the formatting process, each phase enables reading of more
 complicated persistent objects, e.g.\\ in the first phase only
 immediate or simple objects can be read whereas after finishing the
 last phase all objects can be read from the \\sh.

 After the bootstrap is done, the objects loaded in the bootstrap
 behave like `normal' objects, i.e.\\ they are now accessed using
 their descriptions.
\\Seealsolabel
 \\Fcite{format-plob-root};
 \\fcite{open-heap};
 section \\fcite{p-structure-description- ...};
 section \\fcite{p-structure-slot-description- ...};
 section \\fcite{p-package- ...};
 section \\fcite{p-lisproot- ...};
 section \\fcite{p-class-description- ...};
 section \\fcite{p-slot-description- ...};
 section \\fcite{p-method-description- ...};
 section Bootstrap Issues \\amopcite{270}.")

;;; ---------------------------------------------------------------------------
(defun bootstrap-allocate-structure
     (number-of-slots &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 Allocate a structure at formatting of the root."
  (let ((p-objid (p-allocate p-heap +structure-type-tag+ number-of-slots)))
    ;; 1996/13/11 HK: No good idea to make all objects allocated in the
    ;; bootstrap :read dependent
    ;; (setf (p-dependent p-objid p-heap) :read)
    p-objid))

;;; ---------------------------------------------------------------------------
(defun bootstrap-store-phase-1-structure-description
     (p-objid t-descr class-of-struct
	      &optional (depth :flat) (p-heap *root-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{t-descr}}
      {a structure description}
 \\isastrcls{\\funarg{class-of-struct}}
\\Purposelabel
 Fills the transient \\funarg{t-descr}\\ with a description of
 \\funarg{class-of-struct}\\ and begins storing
 \\funarg{t-descr}\\ `by hand', i.e.\\ by calling low-level
 writer functions. In phase~1, only objects of immediate or simple
 classes (fixnum and string) can be stored to the \\sh.
\\Remarkslabel
 It is an error to call this function being not in the bootstrap phase.
\\Seealsolabel
 Section \\fcite{bootstrap ...}."

  (labels ((bootstrap-allocate-structure-slot-description
            (&optional (p-heap *default-persistent-heap*))
            "Allocate a structure slot description at formatting of the root."
            (let ((p-objid (p-allocate p-heap
                                       +structure-type-tag+
                                       +structure-slot-description-size+)))
	      (setf (p-index p-heap p-objid +structure-location-description+)
		    *structure-slot-description-objid*)
	      p-objid)))

    (with-transaction (p-heap)
      (write-lock p-heap p-objid t-descr depth +structure-type-tag+)
      (fill-description class-of-struct t-descr p-heap)
      (let* ((total-number-of-slots
	      (class-description-slot-numbers t-descr))
	     (p-objid-slot-descr-vector
	      (p-allocate-vector total-number-of-slots p-heap))
	     (t-slots (class-slots t-descr))
	     (i 0))
	(setf (p-structure-description-persistent-slot-numbers
	       p-objid depth p-heap)
	  (class-description-persistent-slot-numbers t-descr))
	(setf (p-structure-description-slot-numbers p-objid depth p-heap)
	  total-number-of-slots)
	(setf (p-structure-description-slots p-objid :objid p-heap)
	  p-objid-slot-descr-vector)
	(map nil
	  #'(lambda (s)
	      (multiple-value-bind (p-objid-slot-descr type-tag)
		  (p-svref p-objid-slot-descr-vector
			   i :objid p-heap)
		(when (and type-tag
			   (= type-tag +unbound-type-tag+))
		  (setf p-objid-slot-descr
		    (bootstrap-allocate-structure-slot-description
		     p-heap)))
		(setf (structure-slot-description-objid s)
		  p-objid-slot-descr)
		(setf (p-structure-slot-description-location
		       p-objid-slot-descr :flat p-heap)
		  (slot-definition-location s))
		(setf (p-svref p-objid-slot-descr-vector
			       i :objid p-heap)
		  p-objid-slot-descr)
		(incf i)))
	  t-slots)))
    (when (and *verbose* (>= *verbose* 3))
      (format t ";;;; Bootstrap(1): Stored ~S, ~D/~D slots.~%"
              (class-name t-descr)
              (class-description-persistent-slot-numbers t-descr)
              (class-description-slot-numbers t-descr))))
  t-descr)

;;; ---------------------------------------------------------------------------
(defun bootstrap-store-phase-2-structure-description
     (p-objid t-descr class-of-struct
	      &optional (depth :flat) (p-heap *root-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{t-descr}}
      {a structure description}
 \\isastrcls{\\funarg{class-of-struct}}
\\Purposelabel
 Stores the slots not stored in phase~1 of
 \\funarg{t-descr}\\ `by hand', i.e.\\ by calling low-level writer
 functions. In phase~2, symbols may be stored to the \\sh.
\\Remarkslabel
 It is an error to call this function being not in the bootstrap phase.
\\Seealsolabel
 Section \\fcite{bootstrap ...}."

  (with-transaction (p-heap)
    (write-lock p-heap p-objid t-descr depth +structure-type-tag+)
    (setf (p-structure-description-name p-objid :cached p-heap)
      (class-name class-of-struct))
    (setf (p-structure-description-schema-evolution p-objid :cached p-heap)
      (class-description-schema-evolution t-descr))
    (register-class t-descr p-heap)
    (let* ((p-objid-slot-descr-vector
	    (p-structure-description-slots p-objid :objid p-heap))
	   (t-slots (class-slots t-descr))
	   (i 0))
      (map nil
	#'(lambda (s)
	    (setf (p-structure-slot-description
		   (p-svref p-objid-slot-descr-vector i :objid p-heap)
		   :flat p-heap)
	      s)
	    (incf i))
	t-slots)))
  (when (and *verbose* (>= *verbose* 3))
    (format t ";;;; Bootstrap(2): Stored ~S, ~D/~D slots.~%"
            (class-name t-descr)
            (class-description-persistent-slot-numbers t-descr)
            (class-description-slot-numbers t-descr)))
  t-descr)

;;; ---------------------------------------------------------------------------
(defun bootstrap-store-phase-3-structure-description
     (p-objid t-descr class-of-struct
	      &optional (depth :flat) (p-heap *root-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{t-descr}}
      {a structure description}
 \\isastrcls{\\funarg{class-of-struct}}
\\Purposelabel
 Now the `normal' functions save \\funarg{t-descr}. In phase~3,
 all structure objects may be stored to the \\sh.
\\Remarkslabel
 It is an error to call this function being not in the bootstrap phase.
\\Seealsolabel
 Section \\fcite{bootstrap ...}."

  (declare (ignore class-of-struct))
  (setf (p-structure p-objid depth p-heap) t-descr)
  (setf (p-read-only p-objid) t)
  (when (and *verbose* (>= *verbose* 3))
    (format t ";;;; Bootstrap(3): Stored ~S, ~D/~D slots.~%"
            (class-name t-descr)
            (class-description-persistent-slot-numbers t-descr)
            (class-description-slot-numbers t-descr)))
  t-descr)

;;; ---------------------------------------------------------------------------
(defun bootstrap-allocate-instance
     (p-objid-class-description number-of-slots
				&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 Allocate a CLOS instance at formatting of the root."
  (let ((p-objid (p-allocate p-heap +instance-type-tag+)))
    (with-transaction (p-heap)
      (when p-objid-class-description
	(setf (p-index p-heap p-objid +clos-location-class-wrapper+)
	  p-objid-class-description))
      (when number-of-slots
	(sh-write-instance-data
	 (persistent-object-objid p-heap)
	 (persistent-object-objid p-objid)
	 (p-allocate-vector number-of-slots p-heap))))
    ;; 1996/13/11 HK: No good idea to make all objects allocated in the
    ;; bootstrap :read dependent
    ;; (setf (p-dependent p-objid p-heap) :read)
    p-objid))

;;; ---------------------------------------------------------------------------
(defun bootstrap-store-phase-4-class-description
     (p-objid t-descr the-class
	      &optional (depth :flat) (p-heap *root-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{t-descr}}
      {a class description}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Fills the transient \\funarg{t-descr}\\ with a description of
 \\funarg{the-class}\\ and begins storing
 \\funarg{t-descr}\\ `by hand', i.e.\\ by calling low-level
 writer functions. In phase~4, only non-\\clos\\ objects including
 structures can be stored to the \\sh.
\\Remarkslabel
 It is an error to call this function being not in the bootstrap phase.
\\Seealsolabel
 Section \\fcite{bootstrap ...}."

  (labels ((bootstrap-allocate-effective-slot-description
            (&optional (p-heap *default-persistent-heap*))
            "Allocate an effective slot description at formatting of the root."
            (bootstrap-allocate-instance
             *effective-slot-description-objid*
	     +effective-slot-description-size+
	     p-heap)))

    (setf (persistent-object-objid t-descr) p-objid)
    (with-transaction (p-heap)
      (write-lock p-heap p-objid t-descr depth +instance-type-tag+)
      (fill-description the-class t-descr p-heap)
      (let* ((total-number-of-slots
	      (class-description-slot-numbers t-descr))
	     (number-of-persistent-slots
	      (class-description-persistent-slot-numbers t-descr))
	     (p-objid-slot-descr-vector
	      (when (> number-of-persistent-slots 0)
		(p-allocate-vector total-number-of-slots p-heap)))
	     (t-slots (class-slots t-descr))
	     (i 0))
	(setf (p-class-description-name p-objid depth p-heap)
	  (class-name t-descr))
	(setf (p-class-description-schema-evolution p-objid depth p-heap)
	  (class-description-schema-evolution t-descr))
	(setf (p-class-description-persistent-slot-numbers
	       p-objid depth p-heap)
	  number-of-persistent-slots)
	(setf (p-class-description-slot-numbers p-objid depth p-heap)
	  total-number-of-slots)
	(if p-objid-slot-descr-vector
	    (progn
	      (setf (p-class-description-effective-slots p-objid :objid p-heap)
		p-objid-slot-descr-vector)
	      (map nil
		#'(lambda (s)
		    (multiple-value-bind (p-objid-slot-descr type-tag)
			(p-svref p-objid-slot-descr-vector
				 i :objid p-heap)
		      (when (and type-tag
				 (= type-tag +unbound-type-tag+))
			(setf p-objid-slot-descr
			  (bootstrap-allocate-effective-slot-description
			   p-heap)))
		      (setf (persistent-object-objid s) p-objid-slot-descr)
		      (setf (p-slot-description-location
			     p-objid-slot-descr :flat p-heap)
			(slot-definition-location s))
		      (setf (p-slot-description-allocation
			     p-objid-slot-descr :cached p-heap)
			(slot-definition-allocation s))
		      (setf (p-svref p-objid-slot-descr-vector
				     i :objid p-heap)
			p-objid-slot-descr)
		      (incf i)))
		t-slots))
	  (setf (p-class-description-effective-slots p-objid :objid p-heap)
	    nil))
	(register-class t-descr p-heap)))
    (when (and *verbose* (>= *verbose* 3))
      (format t ";;;; Bootstrap(4): Stored ~S, ~D/~D slots.~%"
              (class-name t-descr)
              (class-description-persistent-slot-numbers t-descr)
              (class-description-slot-numbers t-descr)))
    (setf (mismatch-p (find-class (class-name t-descr))) nil)
    t-descr))

;;; ---------------------------------------------------------------------------
(defun bootstrap-store-phase-5-class-description
     (p-objid t-descr the-class
	      &optional (depth :flat) (p-heap *root-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{t-descr}}
      {a class description}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 In phase~5, all objects can be stored to the \\sh.
\\Remarkslabel
 It is an error to call this function being not in the bootstrap phase.
\\Seealsolabel
 Section \\fcite{bootstrap ...}."

  (declare (ignore the-class))
  (with-transaction
   (p-heap)
   (write-lock p-heap p-objid t-descr depth +instance-type-tag+)
   (let* ((p-objid-slot-descr-vector
	   (p-class-description-effective-slots p-objid :objid p-heap))
	  (t-slots (class-slots t-descr))
	  (i 0))
     (map nil
	  #'(lambda (s)
	      (setf (p-slot-description
		     (p-svref p-objid-slot-descr-vector i :objid p-heap)
		     :flat p-heap)
		    s)
	      (incf i))
	  t-slots)))
  (setf (p-instance p-objid depth p-heap) t-descr)
  (setf (p-read-only p-objid) t)
  (when (and *verbose* (>= *verbose* 3))
    (format t ";;;; Bootstrap(5): Stored ~S, ~D/~D slots.~%"
            (class-name t-descr)
            (class-description-persistent-slot-numbers t-descr)
            (class-description-slot-numbers t-descr)))
  t-descr)

;;; ---------------------------------------------------------------------------
(defun bootstrap-p-symbol
     (p-objid &optional (p-heap *root-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{p-objid}
\\Purposelabel
 Loads a symbol during the bootstrap.
\\Remarkslabel
 It is an error to call this function being not in the bootstrap phase.
\\Seealsolabel
 Section \\fcite{bootstrap ...}."

  (multiple-value-bind (the-symbol the-symbol-p)
      (is-registered-objid p-objid)
    (unless the-symbol-p
      (let* ((name (p-symbol-name p-objid :cached p-heap))
             (p-objid-package-name (p-package-name
		                    (p-symbol-package p-objid :objid p-heap)
		                    :objid
		                    p-heap))
             (package-name (external-to-internal-name
			    (cond
			     ((p-stringp p-objid-package-name p-heap)
			      (p-string p-objid-package-name :cached p-heap))
			     ((p-symbolp p-objid-package-name p-heap)
			      (p-symbol-name p-objid-package-name
					     :cached p-heap))))))
        (unless (or (null package-name)
                    (find-package package-name))
          ;; The package-name is non-NIL and the
          ;; transient package wasn't found; so create the
          ;; package and supply a warning that a new package
          ;; was created:
          (make-package package-name)
          (when (and *verbose* (>= *verbose* 1))
            (warn "Bootstrap: PLOB created package ~A ~
                   at loading symbol ~A"
                  package-name name)))
        (setf the-symbol (intern name package-name)))
      (when (and *verbose* (>= *verbose* 6))
        (format t "; Loaded symbol ~A in bootstrap~%"
                (pretty-print-symbol the-symbol)))
      (register-to-base-cache p-objid the-symbol))
    the-symbol))

;;; ---------------------------------------------------------------------------
(defparameter +force-read-in-bootstrap+ t)

;;; ---------------------------------------------------------------------------
(defun bootstrap-load-phase-1-structure-description
     (p-objid t-into-descr
	      &optional (depth :cached) (p-heap *root-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{t-into-descr}}
      {a structure description}
\\Purposelabel
 Begins loading \\funarg{t-into-descr}\\ `by hand', i.e.\\ by calling
 low-level reader functions. In phase~1, only objects of immediate or simple
 classes (fixnum and string) can be read from the \\sh.
 The transient structure-slot-descriptions referenced by
 \\funarg{t-into-descr}\\ are allocated and partly filled.
\\Remarkslabel
 It is an error to call this function being not in the bootstrap phase.
\\Seealsolabel
 Section \\fcite{bootstrap ...}."

  (with-transaction (p-heap)
   (with-read-lock (p-heap p-objid depth +structure-type-tag+ +force-read-in-bootstrap+)
     (setf (structure-description-objid t-into-descr) p-objid)
     (register-to-base-cache p-objid t-into-descr)
     (let* ((p-slots (p-structure-description-slots p-objid :objid p-heap))
	    (total-number-of-slots
	     (p-structure-description-slot-numbers p-objid depth p-heap))
	    (number-of-persistent-slots
	     (p-structure-description-persistent-slot-numbers
	      p-objid depth p-heap))
	    (t-slot-descr-vector (when (> number-of-persistent-slots 0)
				   (make-array total-number-of-slots))))
       (when t-slot-descr-vector
	 (register-to-base-cache p-slots t-slot-descr-vector))
       (setf (class-description-name t-into-descr)
	 (bootstrap-p-symbol
	  (p-structure-description-name p-objid :objid p-heap)
	  p-heap))
       (setf (class-description-schema-evolution t-into-descr)
	 (bootstrap-p-symbol
	  (p-structure-description-schema-evolution
	   p-objid :objid p-heap)
	  p-heap))
       (setf (structure-description-p-constructor t-into-descr)
	 (bootstrap-p-symbol
	  (p-structure-description-constructor
	   p-objid :objid p-heap)
	  p-heap))
       (setf (structure-description-p-dependent t-into-descr)
	 (bootstrap-p-symbol
	  (p-structure-description-dependent
	   p-objid :objid p-heap)
	  p-heap))
       (setf (class-description-persistent-slot-numbers t-into-descr)
	 number-of-persistent-slots)
       (setf (class-description-slot-numbers t-into-descr)
	 total-number-of-slots)
       (setf (class-description-effective-slots t-into-descr)
	 t-slot-descr-vector)
       (when t-slot-descr-vector
	 (dotimes (i total-number-of-slots)
	   (let* ((p-objid-slot-descr (p-svref p-slots i :objid p-heap))
		  (t-slot-descr (is-registered-objid p-objid-slot-descr)))
	     (unless t-slot-descr
	       (setf t-slot-descr (make-structure-slot-description))
	       (register-to-base-cache p-objid-slot-descr t-slot-descr))
	     (setf (structure-slot-description-p-descr
		    t-slot-descr)
	       *structure-slot-description*)
	     (setf (structure-slot-description-objid t-slot-descr)
	       p-objid-slot-descr)
	     (setf (slot-description-name t-slot-descr)
	       (bootstrap-p-symbol
		(p-structure-slot-description-name
		 p-objid-slot-descr :objid p-heap)
		p-heap))
	     (setf (structure-slot-description-p-reader t-slot-descr)
	       (bootstrap-p-symbol
		(p-structure-slot-description-reader
		 p-objid-slot-descr :objid p-heap)
		p-heap))
	     (setf (slot-description-location t-slot-descr)
	       (p-structure-slot-description-location
		p-objid-slot-descr depth p-heap))
	     (setf (slot-description-extent t-slot-descr)
	       (bootstrap-p-symbol
		(p-structure-slot-description-extent
		 p-objid-slot-descr :objid p-heap)
		p-heap))
	     (setf (svref t-slot-descr-vector i)
	       t-slot-descr)))))))
  (when (and *verbose* (>= *verbose* 3))
    (format t ";;;; Bootstrap(1): Loaded ~S, ~D/~D slots.~%"
            (class-name t-into-descr)
            (class-description-persistent-slot-numbers t-into-descr)
            (class-description-slot-numbers t-into-descr)))
  t-into-descr)

;;; ---------------------------------------------------------------------------
(defun bootstrap-load-phase-2-structure-description
     (p-objid t-into-descr
	      &optional (depth :cached) (p-heap *root-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{t-into-descr}}
      {a structure description}
\\Purposelabel
 Now the `normal' functions read the structure description slots
 not read in phase~1 into \\funarg{t-into-descr}.
 The transient structure-slot-descriptions referenced by
 \\funarg{t-into-descr}\\ are read from the \\sh.
\\Remarkslabel
 It is an error to call this function being not in the bootstrap phase.
\\Seealsolabel
 Section \\fcite{bootstrap ...}."

  (with-transaction (p-heap)
    (with-read-lock (p-heap p-objid depth +structure-type-tag+ +force-read-in-bootstrap+)
      (setf (class-description-name t-into-descr)
	(p-structure-description-name p-objid :cached p-heap))
      (let* ((total-number-of-slots (class-description-slot-numbers
				     t-into-descr))
	     (t-slot-descr-vector (class-slots
				   t-into-descr)))
	(dotimes (i total-number-of-slots)
	  (let* ((t-slot-descr (svref t-slot-descr-vector i))
		 (p-objid-slot-descr (persistent-object-objid t-slot-descr)))
	    (p-structure-slot-description-into t-slot-descr
					       p-objid-slot-descr
					       depth p-heap))))))
  (p-structure-description-into t-into-descr p-objid depth p-heap)
  (establish-all-slot-loaders t-into-descr depth p-heap)
  (when (and *verbose* (>= *verbose* 3))
    (format t ";;;; Bootstrap(2): Loaded ~S, ~D/~D slots.~%"
            (class-name t-into-descr)
            (class-description-persistent-slot-numbers t-into-descr)
            (class-description-slot-numbers t-into-descr)))
  t-into-descr)

;;; ---------------------------------------------------------------------------
(defun bootstrap-load-phase-3-class-description
     (p-objid t-into-descr
	      &optional (depth :cached) (p-heap *root-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{t-into-descr}}
      {a class description}
\\Purposelabel
 Begins loading \\funarg{t-into-descr}\\ `by hand', i.e.\\ by calling
 low-level reader functions. In phase~3, only non-\\clos\\ objects
 can be read from the \\sh.
 The transient slot-descriptions referenced by
 \\funarg{t-into-descr}\\ are allocated and partly filled.
\\Remarkslabel
 It is an error to call this function being not in the bootstrap phase.
\\Seealsolabel
 Section \\fcite{bootstrap ...}."

  (with-transaction (p-heap)
    (with-read-lock (p-heap p-objid depth +instance-type-tag+ +force-read-in-bootstrap+)
      (setf (persistent-object-objid t-into-descr) p-objid)
      (register-to-base-cache p-objid t-into-descr)
      (let* ((p-slots
	      (p-class-description-effective-slots p-objid :objid p-heap))
	     (total-number-of-slots
	      (p-class-description-slot-numbers p-objid :cached p-heap))
	     (number-of-persistent-slots
	      (p-class-description-persistent-slot-numbers
	       p-objid :cached p-heap))
	     (t-slot-descr-vector (when (> number-of-persistent-slots 0)
				    (make-array total-number-of-slots)))
	     (class-name (p-class-description-name p-objid :cached p-heap))
	     (the-class (find-class class-name))
	     (effective-slots nil))
	(unless (class-finalized-p the-class)
	  (finalize-inheritance the-class))
	(setf effective-slots (class-slots the-class))
	(when t-slot-descr-vector
	  (register-to-base-cache p-slots t-slot-descr-vector))
	(setf (class-description-name t-into-descr) class-name)
	(setf (class-description-schema-evolution t-into-descr)
	  (p-class-description-schema-evolution p-objid :cached p-heap))
	(setf (class-description-persistent-slot-numbers t-into-descr)
	  number-of-persistent-slots)
	(setf (class-description-slot-numbers t-into-descr)
	  total-number-of-slots)
	(setf (class-description-effective-slots t-into-descr)
	  t-slot-descr-vector)
	(when t-slot-descr-vector
	  (dotimes (i total-number-of-slots)
	    (let* ((p-objid-slot-descr (p-svref p-slots i :objid p-heap))
		   (t-slot-descr (is-registered-objid p-objid-slot-descr))
		   (slot-name
		    (p-slot-description-name p-objid-slot-descr :cached p-heap))
		   (effective-slot (find slot-name
					 effective-slots
					 :key #'slot-definition-name)))
	      (unless t-slot-descr
		(setf t-slot-descr
		  (make-instance 'effective-slot-description
		    :objid p-objid-slot-descr
		    :store-cached-slots nil
		    :p-heap p-heap))
		(register-to-base-cache p-objid-slot-descr t-slot-descr))
	      (setf (slot-description-name t-slot-descr) slot-name)
	      (setf (slot-description-location t-slot-descr)
		(p-slot-description-location
		 p-objid-slot-descr :cached p-heap))
	      (setf (slot-description-allocation t-slot-descr)
		(p-slot-description-allocation
		 p-objid-slot-descr :cached p-heap))
	      (setf (slot-description-extent t-slot-descr)
		(p-slot-description-extent
		 p-objid-slot-descr :cached p-heap))
	      (setf (slot-description-deferred t-slot-descr)
		(p-slot-description-deferred
		 p-objid-slot-descr :cached p-heap))
	      (setf (svref t-slot-descr-vector i) t-slot-descr)
	      (setf (persistent-slot-definition-description effective-slot)
		t-slot-descr)))))))
  (let* ((class-name (class-name t-into-descr))
         (the-class (find-class class-name t-into-descr)))
    (when (and *verbose* (>= *verbose* 3))
      (format t ";;;; Bootstrap(3): Loaded ~S, ~D/~D slots.~%"
              class-name
              (class-description-persistent-slot-numbers t-into-descr)
              (class-description-slot-numbers t-into-descr)))
    (setf (mismatch-p the-class) nil))
  t-into-descr)

;;; ---------------------------------------------------------------------------
(defun bootstrap-load-phase-4-class-description
     (p-objid t-into-descr
	      &optional (depth :cached) (p-heap *root-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{t-into-descr}}
      {a structure description}
\\Purposelabel
 Now the `normal' functions read the class description slots
 not read in phase~3 into \\funarg{t-into-descr}.
 The transient slot-descriptions referenced by
 \\funarg{t-into-descr}\\ are read from the \\sh.
\\Remarkslabel
 It is an error to call this function being not in the bootstrap phase.
\\Seealsolabel
 Section \\fcite{bootstrap ...}."

  (with-transaction (p-heap)
    (with-read-lock (p-heap p-objid depth +instance-type-tag+ +force-read-in-bootstrap+)
      (let* ((total-number-of-slots
	      (class-description-slot-numbers t-into-descr))
	     (t-slot-descr-vector
	      (class-slots t-into-descr)))
	(when t-slot-descr-vector
	  (dotimes (i total-number-of-slots)
	    (let* ((t-slot-descr (svref t-slot-descr-vector i))
		   (p-objid-slot-descr (persistent-object-objid t-slot-descr)))
	      (p-slot-description-into
	       t-slot-descr p-objid-slot-descr depth p-heap)))))))
  (p-class-description-into t-into-descr p-objid depth p-heap)
  (let* ((name (class-name t-into-descr))
         (class (find-class name)))
    (setf (class-description-of class) t-into-descr)
    (setf (class-description-class t-into-descr) class)
    (when (and *verbose* (>= *verbose* 3))
      (format t ";;;; Bootstrap(4): Loaded ~S, ~D/~D slots.~%"
              name
              (class-description-persistent-slot-numbers t-into-descr)
              (class-description-slot-numbers t-into-descr))))
  t-into-descr)

;;; ---------------------------------------------------------------------------
(defun format-plob-root (&optional (url *database-url*))
  #+:lisp-doc "
\\Purposelabel
 Initialize the \\sh\\ for usage. The description objects (comparable to
 the \\mo[s]\\ of \\clos) are stored to the initially empty \\sh.
 When at startup time the \\sh\\ is detected as being empty, this function
 is called; so there is no need to call it explicit. There is one exception
 from this rule: When the state of the \\sh\\ is inconsistent, this
 function can be called to force a re-format, but {\\sl all objects
 stored in the \\sh\\ will be lost}. The symbol {\\bf format-plob-root}
 is not exported from the \\lisp{:plob}\\ package,
 so evaluate \\lisp{(plob::format-plob-root)}.
\\Seealsolabel
 \\Fcite{open-heap};
 section \\fcite{bootstrap ...}."

  (labels ((bootstrap-allocate-lisproot
            (&optional (p-heap *default-persistent-heap*))
            "Allocate a lisproot structure at formatting of the root."
            (let ((p-objid (bootstrap-allocate-structure
                            +root-size+ p-heap)))
              (with-transaction (p-heap)
		(p-initialize-lisproot p-objid))
              p-objid))

	   (setf-bootstrap-p-instance-class-wrapper
	    (t-class-wrapper
	     p-objid
	     &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
	      (t-slot-to-p-objid t-class-wrapper depth p-heap p-objid
				 +clos-location-class-wrapper+ nil
				 +instance-type-tag+))

	   (do-format-root
	       (&optional (url *database-url*))
            (close-heap)
            (when (and *verbose* (>= *verbose* 3))
              (format t ";;;; Bootstrap   : Formatting ~A~%"
		      (effective-url url)))
	    (bootstrap-clear-cache)
	    (invalidate-all-globals)

            (let* ((root-persistent-heap-objid
                    (sh-open url "format root heap"))
                   (root-persistent-heap nil))

              (setf *root-persistent-heap-objid* nil)
              (setf root-persistent-heap-objid
	            (sh-write-root root-persistent-heap-objid nil))
              (setf *root-persistent-heap-objid*
                    root-persistent-heap-objid)
	      (setf root-persistent-heap
                    (make-persistent-heap root-persistent-heap-objid))
              (setf (persistent-object-objid *root-persistent-heap*)
	            *root-persistent-heap-objid*)
              (setf (persistent-heap-pid *root-persistent-heap*)
	            (process-pid))
              (setf (persistent-heap-pid root-persistent-heap)
	            (process-pid))
              (setf (persistent-object-objid root-persistent-heap)
	            root-persistent-heap-objid)
              (setf (persistent-object-objid *default-persistent-heap*)
	            root-persistent-heap-objid)
              (setf (persistent-heap-pid *default-persistent-heap*)
	            (process-pid))

              (setf (persistent-btree-objid *name->package-table*)
	            (p-allocate-btree root-persistent-heap))
              (setf (btree-test *name->package-table*) 'equal)
              (setf (persistent-btree-objid *symbol->class-table*)
	            (p-allocate-btree root-persistent-heap))
              (setf (btree-test *symbol->class-table*) 'eq)

              ;; Clear the heap hash tables:
              (bootstrap-clear-cache)

              (setf *structure-description-objid*
	            (bootstrap-allocate-structure
                     +structure-description-size+
                     root-persistent-heap))
              (setf *structure-description*
	            (make-structure-description-internal
	             :objid *structure-description-objid*))
              (setf (structure-description-p-descr *structure-description*)
	            *structure-description*)

              (setf *structure-slot-description-objid*
	            (bootstrap-allocate-structure
                     +structure-description-size+
                     root-persistent-heap))
              (setf *structure-slot-description*
	            (make-structure-description
                     *structure-slot-description-objid*))

              (setf *package-description-objid*
	            (bootstrap-allocate-structure
                     +structure-description-size+
                     root-persistent-heap))
              (setf *package-description*
	            (make-structure-description *package-description-objid*))

              (setf *lisproot-description-objid*
	            (bootstrap-allocate-structure
                     +structure-description-size+
                     root-persistent-heap))
              (setf *lisproot-description*
	            (make-structure-description *lisproot-description-objid*))

              (setf (persistent-object-internal-objid *root*)
                    (bootstrap-allocate-lisproot
                     root-persistent-heap))

              (setf *class-description-objid*
                    (bootstrap-allocate-instance
                     nil +class-description-size+ root-persistent-heap))
              (setf-bootstrap-p-instance-class-wrapper
	       *class-description-objid*
	       *class-description-objid* :objid root-persistent-heap)
              (setf *class-description*
                    (make-instance 'class-description
                                   :objid *class-description-objid*
                                   :p-heap root-persistent-heap
                                   :store-cached-slots nil))

              (setf *plob-description-objid*
                    (bootstrap-allocate-instance
                     *class-description-objid* +class-description-size+
		     root-persistent-heap))
              (setf *plob-description*
                    (make-instance 'class-description
                                   :objid *plob-description-objid*
                                   :p-heap root-persistent-heap
                                   :store-cached-slots nil))

              (setf *slot-description-objid*
                    (bootstrap-allocate-instance
                     *class-description-objid* +class-description-size+
		     root-persistent-heap))
              (setf *slot-description*
                    (make-instance 'class-description
                                   :objid *slot-description-objid*
                                   :p-heap root-persistent-heap
                                   :store-cached-slots nil))

              (setf *direct-slot-description-objid*
                    (bootstrap-allocate-instance
                     *class-description-objid* +class-description-size+
		     root-persistent-heap))
              (setf *direct-slot-description*
                    (make-instance 'class-description
                                   :objid *direct-slot-description-objid*
                                   :p-heap root-persistent-heap
                                   :store-cached-slots nil))

              (setf *effective-slot-description-objid*
                    (bootstrap-allocate-instance
                     *class-description-objid* +class-description-size+
		     root-persistent-heap))
              (setf *effective-slot-description*
                    (make-instance 'class-description
                                   :objid *effective-slot-description-objid*
                                   :p-heap root-persistent-heap
                                   :store-cached-slots nil))

              (setf *method-description-objid*
                    (bootstrap-allocate-instance
                     *class-description-objid* +class-description-size+
		     root-persistent-heap))
              (setf *method-description*
                    (make-instance 'class-description
				   :objid *method-description-objid*
                                   :p-heap root-persistent-heap
                                   :store-cached-slots nil))

              ;; Now the objects for structure-descriptions,
              ;; structure-slot-descriptions
              ;; and packages exists, but are still empty.
              ;; Fill and store the structure descriptions:

              (bootstrap-store-phase-1-structure-description
               *structure-description-objid*
	       *structure-description*
	       (find-class 'structure-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-1-structure-description
               *structure-slot-description-objid*
	       *structure-slot-description*
	       (find-class 'structure-slot-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-1-structure-description
               *package-description-objid*
	       *package-description*
	       (find-class 'persistent-package)
	       :flat
               root-persistent-heap)
              (bootstrap-store-phase-1-structure-description
               *lisproot-description-objid*
	       *lisproot-description*
	       (find-class 'persistent-lisproot)
	       :flat
               root-persistent-heap)

              (bootstrap-store-phase-2-structure-description
               *structure-description-objid*
	       *structure-description*
	       (find-class 'structure-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-2-structure-description
               *structure-slot-description-objid*
	       *structure-slot-description*
	       (find-class 'structure-slot-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-2-structure-description
               *package-description-objid*
	       *package-description*
	       (find-class 'persistent-package)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-2-structure-description
               *lisproot-description-objid*
	       *lisproot-description*
	       (find-class 'persistent-lisproot)
	       :flat
               root-persistent-heap)

              (bootstrap-store-phase-3-structure-description
               *structure-description-objid*
	       *structure-description*
	       (find-class 'structure-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-3-structure-description
               *structure-slot-description-objid*
	       *structure-slot-description*
	       (find-class 'structure-slot-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-3-structure-description
               *package-description-objid*
	       *package-description*
	       (find-class 'persistent-package)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-3-structure-description
               *lisproot-description-objid*
	       *lisproot-description*
	       (find-class 'persistent-lisproot)
	       :flat
               root-persistent-heap)

              (bootstrap-store-phase-4-class-description
               *class-description-objid*
	       *class-description*
	       (find-class 'class-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-4-class-description
               *plob-description-objid*
	       *plob-description*
	       (find-class 'plob-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-4-class-description
               *slot-description-objid*
	       *slot-description*
	       (find-class 'slot-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-4-class-description
               *direct-slot-description-objid*
	       *direct-slot-description*
	       (find-class 'direct-slot-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-4-class-description
               *effective-slot-description-objid*
	       *effective-slot-description*
	       (find-class 'effective-slot-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-4-class-description
               *method-description-objid*
	       *method-description*
	       (find-class 'method-description)
	       :flat
	       root-persistent-heap)

              (bootstrap-store-phase-5-class-description
               *class-description-objid*
	       *class-description*
	       (find-class 'class-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-5-class-description
               *plob-description-objid*
	       *plob-description*
	       (find-class 'plob-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-5-class-description
               *slot-description-objid*
	       *slot-description*
	       (find-class 'slot-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-5-class-description
               *direct-slot-description-objid*
	       *direct-slot-description*
	       (find-class 'direct-slot-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-5-class-description
               *effective-slot-description-objid*
	       *effective-slot-description*
	       (find-class 'effective-slot-description)
	       :flat
	       root-persistent-heap)
              (bootstrap-store-phase-5-class-description
               *method-description-objid*
	       *method-description*
	       (find-class 'method-description)
	       :flat
	       root-persistent-heap)

              (setf (btree-cached-p *name->package-table* :cached
				    root-persistent-heap)
                    t)
              (setf (btree-cached-p *symbol->class-table* :cached
				    root-persistent-heap)
                    t)

              ;; Fill the root object:

              (with-transaction (root-persistent-heap)

		(with-write-lock (root-persistent-heap
				  *root* nil :nocache +structure-type-tag+ t)

		  (setf (p-lisproot-formatted
			 *root* :immediate root-persistent-heap)
		    +lisp-symbolic-system-name+)
		  (setf (p-lisproot-time
			 *root* :immediate root-persistent-heap)
		    (floor (get-universal-time) 60))

		  (setf (p-lisproot-name->package-table
			 *root* :objid root-persistent-heap)
		    *name->package-table*)
		  (setf (p-lisproot-symbol->class-table
			 *root* :objid root-persistent-heap)
		    *symbol->class-table*)

		  (setf (p-lisproot-structure-description
			 *root* :objid root-persistent-heap)
		    *structure-description*)
		  (setf (p-lisproot-structure-slot-description
			 *root* :objid root-persistent-heap)
		    *structure-slot-description*)
		  (setf (p-lisproot-package-description
			 *root* :objid root-persistent-heap)
		    *package-description*)

		  (setf (p-lisproot-class-description
			 *root* :objid root-persistent-heap)
		    *class-description-objid*)
		  (setf (p-lisproot-plob-description
			 *root* :objid root-persistent-heap)
		    *plob-description-objid*)
		  (setf (p-lisproot-slot-description
			 *root* :objid root-persistent-heap)
		    *slot-description-objid*)
		  (setf (p-lisproot-direct-slot-description
			 *root* :objid root-persistent-heap)
		    *direct-slot-description-objid*)
		  (setf (p-lisproot-effective-slot-description
			 *root* :objid root-persistent-heap)
		    *effective-slot-description-objid*)
		  (setf (p-lisproot-method-description
			 *root* :objid root-persistent-heap)
		    *method-description-objid*))
		;; Write the root object into the stable heap:
		(sh-write-root root-persistent-heap-objid 
			       (persistent-object-internal-objid *root*))
		;; Store some often used objects:
		(store-object :read :cached root-persistent-heap)
		(store-object :write :cached root-persistent-heap)
		(store-object :read-write :cached root-persistent-heap))

	      (when (and (integerp root-persistent-heap-objid)
			 (/= root-persistent-heap-objid +null-objid+))
		(sh-stabilise root-persistent-heap-objid)
		(sh-close root-persistent-heap-objid)))

            (invalidate-all-globals)
            (close-session *default-persistent-heap*)
            (setf (persistent-object-objid *default-persistent-heap*) nil)
            (setf (persistent-heap-pid *default-persistent-heap*) nil)
            ))

    (let ((*in-bootstrap-p* t) (done nil))
      (unwind-protect
          (progn
            (do-format-root url)
            (setf done t))
	  (unless done
            (let ((p-heap-objid *root-persistent-heap-objid*))
              (setf *root-persistent-heap-objid* nil)
              (setf (persistent-object-objid *root-persistent-heap*)
                    *root-persistent-heap-objid*)
              (when (and (integerp p-heap-objid)
			 (/= p-heap-objid +null-objid+))
                (sh-close p-heap-objid)))))))
  (values))

;;; ---------------------------------------------------------------------------
(defconstant +metaheap-description+
  "Metaheap"
  #+:lisp-doc "
 The description string of the persistent heap used for storing and loading
 of metaobjects.")

;;; ---------------------------------------------------------------------------
(defmethod open-heap
  (&optional (url *database-url*))
  (labels ((do-load-root
	       (&optional (url *database-url*))

	     (close-heap)

	     (when (and *verbose* (>= *verbose* 3))
	       (format t ";;;; Bootstrap   : Opening ~A~%"
		       (effective-url url)))

	     (let* ((root-persistent-heap-objid
		     (sh-open url +metaheap-description+))
		    (root-persistent-heap
		     (make-persistent-heap root-persistent-heap-objid)))

 	       (setf *root-persistent-heap-objid*
		 root-persistent-heap-objid)
	       (setf (persistent-object-objid *root-persistent-heap*)
		 *root-persistent-heap-objid*)
	       (setf (persistent-heap-pid *root-persistent-heap*)
 		 (process-pid))
	       (setf (persistent-heap-pid root-persistent-heap)
		 (process-pid))
	       (setf (persistent-object-objid root-persistent-heap)
		 root-persistent-heap-objid)
	       (setf (persistent-object-objid *default-persistent-heap*)
		 root-persistent-heap-objid)
	       (setf (persistent-heap-pid *default-persistent-heap*)
		 (process-pid))

	       (setf (persistent-object-internal-objid *root*)
		 (when root-persistent-heap-objid
		   (sh-read-root root-persistent-heap-objid)))

	       (setf (persistent-heap-pid root-persistent-heap)
		 (process-pid))

	       (unless (and (persistent-object-internal-objid *root*)
			    (= (p-type-tag-of
				*root*
				root-persistent-heap-objid)
			       +structure-type-tag+))
		 (when (and root-persistent-heap-objid
			    *verbose* (>= *verbose* 1))
		   (cerror "Format the LISP PLOB root object ~
				 and delete all objects in the stable heap."
			   "It looks as if the LISP PLOB root object ~
				 should be formatted."))
		 (when (and (integerp root-persistent-heap-objid)
			    (/= root-persistent-heap-objid +null-objid+))
		   (sh-close root-persistent-heap-objid))
		 (format-plob-root url)
		 (invalidate-all-globals)
		 (setf root-persistent-heap-objid
		   (sh-open url +metaheap-description+))
		 (setf (persistent-object-internal-objid *root*)
		   (when root-persistent-heap-objid
		     (sh-read-root root-persistent-heap-objid)))
		 (setf (persistent-heap-objid root-persistent-heap)
		   root-persistent-heap-objid))

	       (setf (persistent-heap-pid root-persistent-heap)
		 (process-pid))
	       (setf *root-persistent-heap-objid*
                     root-persistent-heap-objid)
	       (setf (persistent-object-objid *root-persistent-heap*)
		 root-persistent-heap-objid)
	       (setf (persistent-object-objid *default-persistent-heap*)
		 root-persistent-heap-objid)
	       (setf (persistent-heap-pid *default-persistent-heap*)
		 (persistent-heap-pid root-persistent-heap))

	       (bootstrap-clear-cache)

	       (with-transaction (root-persistent-heap)

		 (read-lock root-persistent-heap *root*
			    :nocache
			    +structure-type-tag+)

		 (setf (persistent-btree-objid *name->package-table*)
		   (p-lisproot-name->package-table
		    *root* :objid root-persistent-heap))
		 (setf (persistent-btree-objid *symbol->class-table*)
		   (p-lisproot-symbol->class-table
		    *root* :objid root-persistent-heap))

		 (setf *structure-description-objid*
		   (p-lisproot-structure-description
		    *root* :objid root-persistent-heap))
		 (setf *structure-description*
		   (make-structure-description
		    *structure-description-objid*))
		 (setf (structure-description-p-descr
			*structure-description*)
		   *structure-description*)

		 (setf *structure-slot-description-objid*
		   (p-lisproot-structure-slot-description
		    *root* :objid root-persistent-heap))
		 (setf *structure-slot-description*
		   (make-structure-description
		    *structure-slot-description-objid*))

		 (setf *package-description-objid*
		   (p-lisproot-package-description
		    *root* :objid root-persistent-heap))
		 (setf *package-description*
		   (make-structure-description
		    *package-description-objid*))

		 (setf *lisproot-description-objid*
		   (p-structure-descr *root*
				      :objid root-persistent-heap))
		 (setf *lisproot-description*
		   (make-structure-description
		    *lisproot-description-objid*))

		 (setf *plob-description-objid*
		   (p-lisproot-plob-description
		    *root* :objid root-persistent-heap))
		 (setf *plob-description*
		   (make-instance 'class-description
		     :objid *plob-description-objid*
		     :p-heap root-persistent-heap
		     :store-cached-slots nil))

		 (setf *class-description-objid*
		   (p-lisproot-class-description
		    *root* :objid root-persistent-heap))
		 (setf *class-description*
		   (make-instance 'class-description
		     :objid *class-description-objid*
		     :p-heap root-persistent-heap
		     :store-cached-slots nil))

		 (setf *slot-description-objid*
		   (p-lisproot-slot-description
		    *root* :objid root-persistent-heap))
		 (setf *slot-description*
		   (make-instance 'class-description
		     :objid *slot-description-objid*
		     :p-heap root-persistent-heap
		     :store-cached-slots nil))

		 (setf *direct-slot-description-objid*
		   (p-lisproot-direct-slot-description
		    *root* :objid root-persistent-heap))
		 (setf *direct-slot-description*
		   (make-instance 'class-description
		     :objid
		     *direct-slot-description-objid*
		     :p-heap root-persistent-heap
		     :store-cached-slots nil))

		 (setf *effective-slot-description-objid*
		   (p-lisproot-effective-slot-description
		    *root* :objid root-persistent-heap))
		 (setf *effective-slot-description*
		   (make-instance 'class-description
		     :objid
		     *effective-slot-description-objid*
		     :p-heap root-persistent-heap
		     :store-cached-slots nil))

		 (setf *method-description-objid*
		   (p-lisproot-method-description
		    *root* :objid root-persistent-heap))
		 (setf *method-description*
		   (make-instance 'class-description
		     :objid *method-description-objid*
		     :p-heap root-persistent-heap
		     :store-cached-slots nil))

		 ;; Register the objects into the cache:
		 (register-to-base-cache root-persistent-heap
					 root-persistent-heap)
		 (register-to-base-cache *name->package-table*
					 *name->package-table*)
		 (register-to-base-cache *symbol->class-table*
					 *symbol->class-table*))

	       ;; Load the structure descriptions:
	       (bootstrap-load-phase-1-structure-description
		*structure-description-objid*
		*structure-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-1-structure-description
		*structure-slot-description-objid*
		*structure-slot-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-1-structure-description
		*package-description-objid*
		*package-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-1-structure-description
		*lisproot-description-objid*
		*lisproot-description*
		:cached
		root-persistent-heap)

	       (bootstrap-load-phase-2-structure-description
		*structure-description-objid*
		*structure-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-2-structure-description
		*structure-slot-description-objid*
		*structure-slot-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-2-structure-description
		*package-description-objid*
		*package-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-2-structure-description
		*lisproot-description-objid*
		*lisproot-description*
		:cached
		root-persistent-heap)

	       (bootstrap-load-phase-3-class-description
		*class-description-objid*
		*class-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-3-class-description
		*plob-description-objid*
		*plob-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-3-class-description
		*slot-description-objid*
		*slot-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-3-class-description
		*direct-slot-description-objid*
		*direct-slot-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-3-class-description
		*effective-slot-description-objid*
		*effective-slot-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-3-class-description
		*method-description-objid*
		*method-description*
		:cached
		root-persistent-heap)

	       (bootstrap-load-phase-4-class-description
		*class-description-objid*
		*class-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-4-class-description
		*plob-description-objid*
		*plob-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-4-class-description
		*slot-description-objid*
		*slot-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-4-class-description
		*direct-slot-description-objid*
		*direct-slot-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-4-class-description
		*effective-slot-description-objid*
		*effective-slot-description*
		:cached
		root-persistent-heap)
	       (bootstrap-load-phase-4-class-description
		*method-description-objid*
		*method-description*
		:cached
		root-persistent-heap)

	       (let ((slot-description (p-find-class-description
					'slot-description
					root-persistent-heap)))
		 (register-to-base-cache slot-description
					 slot-description))
	       (let ((effective-slot-description
		      (p-find-class-description
		       'effective-slot-description
		       root-persistent-heap)))
		 (register-to-base-cache effective-slot-description
					 effective-slot-description))

	       (when (and *verbose* (>= *verbose* 3))
		 (let ((lisp-formatted
			(p-lisproot-formatted
			 *root* :cached root-persistent-heap)))
		   (multiple-value-bind
		       (second minute hour date month year)
		       (decode-universal-time
			(* 60
			   (p-lisproot-time *root* root-persistent-heap)))
		     (declare (ignore second))
		     (format
		      t
		      ";;;; LISP root formatted at ~D/~2,'0D/~2,'0D ~D:~2,'0D by ~A~%"
		      year month date hour minute lisp-formatted))))

	       (clear-cache)
	       (when root-persistent-heap-objid
		 (setf (persistent-object-objid root-persistent-heap)
		   root-persistent-heap-objid)
		 (open-session url root-persistent-heap))
	       (setf (persistent-object-objid *default-persistent-heap*) nil)
	       (setf (persistent-heap-pid *default-persistent-heap*) nil))))

    (if *in-bootstrap-p*
        (progn
          (when (and *verbose* (>= *verbose* 3))
            (format t ";;;; Waiting for bootstrap ..."))
          (process-wait-with-timeout
           "Waiting for PLOB! bootstrap"
           *in-bootstrap-wait-timeout*
           #'(lambda ()
               (not *in-bootstrap-p*)))
          (if *in-bootstrap-p*
              (progn
                (format t " timed out~%")
                (error "Waiting for bootstrap timed out."))
            (format t " done!~%")))
      (let ((*in-bootstrap-p* t) (done nil))
        (unwind-protect
            (progn
              (do-load-root url)
              (setf done t))
	  (unless done
	    (let ((p-heap-objid *root-persistent-heap-objid*))
	      (setf *root-persistent-heap-objid* nil)
	      (setf (persistent-object-objid *root-persistent-heap*)
		    *root-persistent-heap-objid*)
	      (when (and (integerp p-heap-objid)
			 (/= p-heap-objid +null-objid+))
	        (sh-close p-heap-objid)))))
        (when done
	  (setf *database-url* url)
          (assert-open-session-p)
	  (check-version-numbers *root-persistent-heap*)))))
  *root-persistent-heap*)

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
