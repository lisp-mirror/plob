;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-extent.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	30.11.93
;;;; Description	PLOB class and slot administration functions
;;;;		regarding transient/persistent instances resp. slots.
;;;;		and functions for administration of structure slot initargs
;;;;		and functions on administrating :deferred slot options
;;;;
;;;; Copyright	PLOB! Copyright 1994--2001 Heiko Kirschke.
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
(use-package :clos)

;;; ---------------------------------------------------------------------------
(defgeneric (setf package-extent) (extent the-package)
  #+:lisp-doc (:documentation
"
\\Argumentslabel
 \\isa{\\funarg{extent}}
      {one of the symbols
       \\lisp{:transient}, \\lisp{\:cached}, \\lisp{\:cached-demand-load},
       \\lisp{:cached-write-through}\\ or \\lisp{:persistent}}
 \\isa{\\funarg{the-package}}
      {a \\cl\\ package object}
\\Valueslabel
 \\retarg{\\funarg{extent}}
\\Purposelabel
 Sets the extent of all classes which are named by symbols which belongs
 to \\funarg{the-package}\\ to \\funarg{extent}; for further explanations
 see \\fcite{(setf class-extent)}\\ and \\fcite{(setf slot-extent)}.

 If a class extent is specified for a class, this extent takes
 precedence over an eventually specified package extent for that class.

 This function is meant to be called from top-level to declare the
 extent of the predefined classes found in the \\cl\\ system packages;
 e.g.\\ some predefined packages have only predefined classes with
 instances which should never become persistent.
\\Exampleslabel
 Set the extent of all instances of all classes whose name symbol
 belongs to the package \\lisp{:clue}\\ to \\lisp{:transient}.
 This will prevent \\plob\\ from making instances of one of these
 classes persistent:
 \\begin{lispcode}
(setf (package-extent (find-package :clue)) :transient)
 \\end{lispcode}
\\Seealsolabel
 \\Fcite{package-extent};
 example at \\fcite{(setf class-extent)};
 \\fcite{(setf slot-extent)}."))

;;; ---------------------------------------------------------------------------
(defgeneric package-extent (the-package)
  #+:lisp-doc (:documentation
"
\\Argumentslabel
 \\isa{\\funarg{the-package}}
      {a \\cl\\ package object}
\\Purposelabel
 Returns the extent of \\funarg{the-package}.
\\Seealsolabel
 \\Fcite{(setf package-extent)};
 \\fcite{class-extent};
 \\fcite{slot-extent}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-extent) (extent the-class)
  #+:lisp-doc (:documentation
"
\\Argumentslabel
 For the \\funarg{extent}\\ argument see \\fcite{(setf slot-extent)}.
 \\isacls{\\funarg{the-class}}
\\Valueslabel
 \\retarg{\\funarg{extent}}
\\Purposelabel
 Sets the extent of \\funarg{the-class}\\ to \\funarg{extent}. The extent
 of all slots of \\funarg{the-class}\\ is set to \\funarg{extent},
 i.e.\\ {\\bf (setf slot-extent)} is called for each slot of
 \\funarg{the-class}; for further explanations see
 \\fcite{(setf slot-extent)}.

 If a slot extent is specified for a slot of a class, this extent takes
 precedence over an eventually specified class extent for that slot.

 This functions is meant to be called from top-level to declare the extent
 of predefined \\cl\\ system classes, e.g.\\ some predefined classes should
 never become persistent; or a class which was made transient by setting
 the package extent of the class naming symbol to
 \\lisp{:transient}\\ (see \\fcite{(setf package-extent)})
 should become persistent.
\\Exampleslabel
 In the first line, the extent of the package \\lisp{:system}\\ is set to
 \\lisp{:transient}; since the symbol \\lisp{logical-pathname}\\ is
 defined in the \\lisp{:system}\\ package, all instances of the class
 \\class{logical-pathname}\\ would be transient too; the second line
 declares instances of the class \\class{logical-pathname}\\ as being
 persistent with an extent of \\lisp{:cached}:
 \\begin{lispcode}
(setf (package-extent (find-package :system)) :transient)
{\\bf(setf (class-extent (find-class 'logical-pathname)) :cached)}
 \\end{lispcode}

\\Seealsolabel

 \\Fcite{package-extent}; \\fcite{(setf package-extent)};
 example at \\fcite{(setf slot-extent)}."))

;;; ---------------------------------------------------------------------------
(defgeneric class-extent (the-class)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Returns the extent of \\funarg{the-class}.
 For \\clsmo{}s which are instances of [a subclass of]
 \\class{persistent-metaclass}\\ (i.e.\\ for classes with
 direct \\plob\\ support),
 the value of the \\lisp{:extent}\\ class option which was given in
 the \\lisp{defclass}-statement of \\funarg{the-class}\\ is returned.
 For all other \\clsmo{}s it is checked if either a class extent is
 given (by a prior call to \\fcite{(setf class-extent)}) or
 if all slots of \\funarg{the-class}\\ have the same extent.
\\Seealsolabel
 \\Fcite{(setf class-extent)};
 \\fcite{slot-extent}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf slot-extent) (extent the-slot the-class)
  #+:lisp-doc (:documentation
"
\\Argumentslabel
 The \\funarg{extent}\\ argument depends on the value
 of the \\funarg{the-class}\\ argument: If \\funarg{the-class}\\ is
 an instance of \\class{structure-class}, the \\funarg{extent}\\ argument
 is one of the symbols
 \\lisp{:transient}, \\lisp{:cached}\\ or \\lisp{:cached-demand-load}.
 Otherwise, the \\funarg{extent}\\ argument
 is one of the symbols \\lisp{:transient}, \\lisp{:cached},
 \\lisp{:cached-write-through}\\ or \\lisp{:persistent}.

 \\isa{\\funarg{the-slot}}
      {either a symbol naming a slot of \\funarg{the-class}\\ or a \\sltmo}
 \\isacls{\\funarg{the-class}}
\\Valueslabel
 \\retarg{\\funarg{extent}}
\\Purposelabel
 Sets the extent of the slot described by
 \\funarg{the-slot}\\ of \\funarg{the-class}\\ to
 \\funarg{extent}.

 Each object handled by \\plob\\ has two representations: a
 {\\sl transient}
 representation, which makes possible an easy and efficient access
 to the object
 with the `well-known' \\cl\\ accessing functions resp.\\ methods
 and a {\\sl persistent} representation which holds the object's
 state for an undetermined amount of time. An object as a whole is
 represented by a collection of slots. The value of a slot's extent
 affects the lifetime of the slot, its representation and the
 interaction between the transient and persistent slot
 state. %
 \\input{tabext}%
 Figure~\\ref{tab:extent} shows the values for the slot extent
 option for persistent \\clos\\ classes;
 for persistent structure classes, there is an additional slot extent
 option value \\lisp{:cached-demand-load}, which will result in the
 slot's state
 being represented in both the transient and persistent object.
 Its state is loaded when the whole object is loaded.
 Slot reading is done from the transient slot's state.
 Slot writing is {\\sl immediate} propagated to
 the persistent object.

 This function is meant to be called from top-level to declare the extent
 of slots of predefined \\cl\\ system classes; e.g.\\ some predefined
 classes have slots which should never become persistent.
\\Exampleslabel
 In the first line, the extent of the package \\lisp{:system}\\ is set to
 \\lisp{:transient}; since the symbol \\lisp{logical-pathname}\\ is
 defined in the \\lisp{:system}\\ package, all instances of the class
 \\class{logical-pathname}\\ would be transient too; the second line
 declares instances of the class \\class{logical-pathname}\\ as being
 persistent with an extent of \\lisp{:cached}. The third line
 declares the slot named \\lisp{system::device}\\ of class
 logical-pathname as being \\lisp{:transient}:
 \\begin{lispcode}
(setf (package-extent (find-package :system)) :transient)
(setf (class-extent (find-class 'logical-pathname)) :cached)
{\\bf(setf (slot-extent 'system::device (find-class 'logical-pathname))
      :transient)}
 \\end{lispcode}
\\Seealsolabel
 \\Fcite{slot-extent};
 \\fcite{(setf class-extent)}."))

;;; ---------------------------------------------------------------------------
(defgeneric slot-extent (the-slot the-class)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isa{\\funarg{the-slot}}
      {either a symbol naming a slot of \\funarg{the-class}\\ or a \\sltmo}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Return the extent of the slot described by
 \\funarg{the-slot}\\ of \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{(setf slot-extent)}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf slot-deferred) (deferred the-slot the-class)
  #+:lisp-doc (:documentation
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{deferred}}
      {either \\lispnil\\ or a number}
 \\isa{\\funarg{the-slot}}
      {either a symbol naming a slot of \\funarg{the-class}\\ or a \\sltmo}
 \\isacls{\\funarg{the-class}}
\\Valueslabel
 \\retarg{\\funarg{deferred}}
\\Purposelabel
 Sets the deferred option of the slot described by
 \\funarg{the-slot}\\ of \\funarg{the-class}\\ to
 \\funarg{deferred}. Slot with the \\lisp{deferred}\\ attribute
 set are stored after the other slots of the objects. This is used
 e.g.\\ for objects having slots which reference many other objects.
 Setting the \\lisp{deferred}\\ attribute on this slot will store
 the slot's state when all other (maybe referenced) objects are
 stored.
\\Remarkslabel
 From the view of good programming, this slot option is absolutely
 unnecessary, since its task is more or less to store resp.\\ load
 the referenced objects before the dereferencing objects. This could
 be done more elegant but much more less efficient by a two-pass
 algorithm, where the first pass collects all objects to be saved,
 sorting the objects into referenced and dereferencing ones
 and write the objects out to the store resp.\\ read them from the
 store in a second pass. My hope is that the second pass will be
 necessary only in very special circumstances, e.g.\\ when the
 storing resp.\\ loading operations put many intermediate objects
 onto the stack.
\\Seealsolabel
 \\Fcite{slot-deferred}."))

;;; ---------------------------------------------------------------------------
(defgeneric slot-deferred (the-slot the-class)
  #+:lisp-doc (:documentation
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-slot}}
      {either a symbol naming a slot of \\funarg{the-class}\\ or a \\sltmo}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Return the deferred option of the slot described by
 \\funarg{the-slot}\\ of \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{(setf slot-deferred)}."))

;;; ---------------------------------------------------------------------------
;;; Auxiliary functions
;;; ---------------------------------------------------------------------------

(defvar *package->extent-table* (make-hash-table :test #'eq)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping package objects to their
 extents.
\\Seealsolabel
 \\Fcite{(setf package-extent)};
 \\fcite{package-extent}.")

;;; ---------------------------------------------------------------------------
(defvar *class->extent-table* (make-hash-table :test #'eq)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping \\clsmo{}s to their extents.
\\Seealsolabel
 \\Fcite{(setf class-extent)};
 \\fcite{class-extent}.")

;;; ---------------------------------------------------------------------------
(defvar *class->slot-extent-table* (make-hash-table :test #'eq)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping \\clsmo{}s to slot extents.
\\Seealsolabel
 \\Fcite{(setf slot-extent)};
 \\fcite{slot-extent}.")

;;; ---------------------------------------------------------------------------
;;; Set the extent of a package
;;; ---------------------------------------------------------------------------
(defmethod (setf package-extent) (extent the-package)
  (if extent
      (setf (gethash the-package *package->extent-table*) extent)
    (remhash the-package *package->extent-table*)))

;;; ---------------------------------------------------------------------------
(defmethod package-extent (the-package)
  (gethash the-package *package->extent-table*))

;;; ---------------------------------------------------------------------------
(defun (setf slot-option) (option table the-slot the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{option}}
 \\isa{\\funarg{table}}
      {a hash-table}
 \\isa{\\funarg{the-slot}}
      {a symbol naming a slot of \\funarg{the-class}}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Store the slot-option's value \\funarg{option}\\ of the slot named
 \\funarg{the-slot}\\ of class \\funarg{the-class}\\ to
 \\funarg{table}. The \\funarg{table}\\ argument is a hash
 table allocated for each extra slot option.
\\Seealsolabel
 \\Fcite{slot-option}."

  (let* ((a-list (gethash the-class table))
         (a-cons (assoc the-slot a-list)))
    (if option
        ;; Add/change the attribut to/in a-list:
	(if a-cons
	    (setf (cdr a-cons) option)
	  (progn
	    (setf a-list (acons the-slot option a-list))
	    (setf (gethash the-class table) a-list)))
      ;; Remove the attribut from a-list:
      (if a-cons
        (setf (gethash the-class table)
	      (delete a-cons a-list)))))
  option)

;;; ---------------------------------------------------------------------------
(defun slot-option (table the-slot the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{table}}
      {a hash-table}
 \\isa{\\funarg{the-slot}}
      {a symbol naming a slot of \\funarg{the-class}}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Get the slot-option's value of the slot named \\funarg{the-slot}\\ of
 class \\funarg{the-class}\\ from \\funarg{table}.
 The \\funarg{table}\\ argument is a hash
 table allocated for each extra slot option.
\\Seealsolabel
 \\Fcite{(setf slot-option)}."

  (cdr (assoc the-slot (gethash the-class table))))

;;; ---------------------------------------------------------------------------
;;; Set/get the extent of a class
;;; ---------------------------------------------------------------------------
(defun set-slot-extent (extent the-slot the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{extent}}
      {a (keyword) symbol}
 \\isa{\\funarg{the-slot}}
      {a symbol naming a slot of \\funarg{the-class}}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Store the slot extent of the slot named \\funarg{the-slot}\\ of
 class \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{*class->slot-extent-table*};
 \\fcite{get-slot-extent}."

  (setf (slot-option *class->slot-extent-table* the-slot the-class)
        extent))

;;; ---------------------------------------------------------------------------
(defun get-slot-extent (the-slot the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{table}}
      {a hash-table}
 \\isa{\\funarg{the-slot}}
      {a symbol naming a slot of \\funarg{the-class}}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Get the slot extent of the slot named \\funarg{the-slot}\\ of
 class \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{*class->slot-extent-table*};
 \\fcite{set-slot-extent}."

  (slot-option *class->slot-extent-table* the-slot the-class))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-extent) (extent the-class)
  (setf (gethash the-class *class->extent-table*) extent))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-extent) :after (extent the-class)
  (setf (mismatch-p the-class) t))

;;; ---------------------------------------------------------------------------
(defmethod class-extent (the-class)
  (let ((extent (gethash the-class *class->extent-table*)))
    (unless extent
      (let* ((slots (class-slots the-class))
             (all-slot-extent (when slots
                                (get-slot-extent (elt slots 0) the-class))))
        (when (every #'(lambda (slot)
			 (eq (get-slot-extent slot the-class) all-slot-extent))
		     slots)
	  ;; All slots have the same :extent:
	  (setf extent all-slot-extent))))
    (unless extent
	  (setf extent
	    (package-extent (symbol-package (class-name the-class)))))
    extent))

;;; ---------------------------------------------------------------------------
;;; Set the extent of a slot
;;; ---------------------------------------------------------------------------

(defconstant +standard-class-class+ (find-class 'standard-class)
  #+:lisp-doc "The \\clsmo\\ of class \\class{standard-class}.")

;;; ---------------------------------------------------------------------------
(defconstant +no-slot-error-prompt+ "~A is no slot of ~A."
  #+:lisp-doc "Prompt which is shown in the error message concerning invalid slot names.")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod class-slots ((class-of-structure structure-class))
  #+:lisp-doc "Returns the slots of structure with class
 \\funarg{class-of-structure}\\ as a list of symbols.
\\Remarkslabel
 \\sysdep{method}

 Hint for \\lw/\\allegro:
 \\lw\\ returns a list of symbols here, whereas \\allegro\\ returns
 a list of instances of class
 \\class{clos::structure-effective-slot-definition}\\ from the 
 built-in method (the better solution, IMHO)."

  (assert-structure-class class-of-structure)
  (structure::structure-class-slot-names class-of-structure))

;;; ---------------------------------------------------------------------------
(defun has-slot-p (the-slot the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-slot}}
      {either a symbol or a \\sltmo}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Returns \\nonnil\\ if \\funarg{the-slot}\\ names an \\eff\\ \\slt\\ of
 \\funarg{the-class}, \\lispnil\\ otherwise.
\\Remarkslabel
 The search is always done by name, i.e.\\ if a \\sltmo\\ was passed as
 the \\funarg{the-slot}\\ argument, the name of the slot is used."

  (when (and (not (class-finalized-p the-class))
	     (subtypep (class-of the-class) +standard-class-class+))
    (finalize-inheritance the-class))
  (find (if (symbolp the-slot)
	    the-slot
	  (slot-definition-name the-slot))
        (class-slots the-class)
	:key #'(lambda (slot)
		 (if (symbolp slot)
		     slot
		   (slot-definition-name slot)))))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-extent) (extent (the-slot symbol)
                                      (the-class class))
  (unless (has-slot-p the-slot the-class)
    (error +no-slot-error-prompt+ the-slot the-class))
  (set-slot-extent extent the-slot the-class)
  extent)

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-extent) (extent (slot slot-definition)
                                      (the-class class))
  (setf (slot-extent (slot-definition-name slot) the-class) extent))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-extent) :after (extent (slot slot-definition)
                                             (the-class class))
  (setf (mismatch-p the-class) t))

;;; ---------------------------------------------------------------------------
(defmethod slot-extent ((slot slot-definition) (the-class class))
  (slot-extent (slot-definition-name slot) the-class))

;;; ---------------------------------------------------------------------------
(defun search-extent (the-slot the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-slot}}
      {a symbol naming a slot of \\funarg{the-class}}
 \\isacls{\\funarg{the-class}}
\\Valueslabel
 Returns an extent for \\funarg{the-slot}.
\\Purposelabel
 Tries to find an extent for the slot named \\funarg{the-slot} by
 looking for the class extent of \\funarg{the-class}. If
 \\funarg{the-class}\\ has no class extent set, the search is continued
 in the superclasses of \\funarg{the-class}."

  (when (has-slot-p the-slot the-class)
    (let ((extent (get-slot-extent the-slot the-class)))
      (if extent
	  extent
	(let ((extent (class-extent the-class)))
	  (if extent
	      extent
	    (dolist (class (class-direct-superclasses the-class))
	      (let ((extent (search-extent the-slot class)))
		(when extent
		  (return extent))))))))))

;;; ---------------------------------------------------------------------------
(defmethod slot-extent ((the-slot symbol) (the-class class))
  (let ((extent (search-extent the-slot the-class)))
    (if extent
        extent
      (class-extent the-class))))

;;; ---------------------------------------------------------------------------
;;; Set/get the deferred option of a slot
;;; ---------------------------------------------------------------------------

(defvar *class->slot-deferred-table* (make-hash-table :test #'eq)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping \\clsmo{}s to slot deferred options.
\\Seealsolabel
 \\Fcite{(setf slot-deferred)};
 \\fcite{slot-deferred}.")

;;; --------------------------------------------------------------------------
(defmethod (setf slot-deferred) (deferred the-slot the-class)
  (setf (slot-option *class->slot-deferred-table* the-slot the-class)
        deferred))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-deferred) :after (deferred the-slot the-class)
  (setf (mismatch-p the-class) t))

;;; ---------------------------------------------------------------------------
(defmethod slot-deferred (the-slot the-class)
  (slot-option *class->slot-deferred-table* the-slot the-class))

;;; ---------------------------------------------------------------------------
;;; Set/get the dependent flag of a class
;;; ---------------------------------------------------------------------------

(defgeneric class-dependent (the-class)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isastrcls{\\funarg{the-class}}
\\Purposelabel
 Get the dependent flag of \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{(setf class-dependent)}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-dependent) (dependent-flag the-class)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isa{\\funarg{dependent-flag}}
      {one of the symbols \\lisp{nil}, \\lisp{:read},
       \\lisp{:write}\\ or \\lisp{:read-write}}
 \\isacls{\\funarg{the-class}}
\\Valueslabel
 \\retarg{\\funarg{dependent-flag}}
\\Purposelabel
 Set the dependent flag of \\funarg{the-class}\\ to
 \\funarg{dependent-flag}. The dependent flag influences what happens
 if an object references another object which is marked as being
 dependent. When an instance of a class is marked as \\lisp{:read}\\ and
 another object referencing the marked object is to be transferred from
 the server to the client upon a read request from the client, the marked
 object will be transferred too. This reduces the time necessary for
 transmitting an object state and the state of the objects it references.
 The dependent
 flag is evaluated on object locking, the \\lisp{:read}\\ flag on
 lock for reading, the \\lisp{:write}\\ flag on lock for writing,
 \\lisp{:read-write}\\ on lock for reading or writing. Using the
 \\lisp{:read}\\ flag might speed up loading of objects which have
 a value-like character with few slots, e.g.\\ instances of classes
 representing (goemetric) points. Too exhaustive use of the dependent
 flag might result in big transitive closures being transmitted.

 To Do:
 A better idea would be to do some more bookkeeping on the server side
 on objects cached by the client. The server marks each object which
 has been cached already by the client to avoid a re-transmit. The
 client would request an object not found in its internal cache; it
 sends the request to the server. When the server knows which objects
 are in the client's cache, the server can decide on its own what
 objects in the transitive closure of the requested object are still
 missing at the client.
\\Seealsolabel
 \\Fcite{class-dependent}."))

;;; ---------------------------------------------------------------------------
(defvar *class->dependent-table* (make-hash-table :test #'eq)
  #+:lisp-doc "
\\Purposelabel
 A variable for mapping structure classes to dependent
 symbols.
\\Seealsolabel
 \\Fcite{(setf class-dependent)};
 \\fcite{class-dependent}.")

;;; ---------------------------------------------------------------------------
(defmethod (setf class-dependent) (dependent the-class)
  (setf (gethash the-class *class->dependent-table*)
        dependent))

;;; ---------------------------------------------------------------------------
(defmethod (setf class-dependent) :after (dependent the-class)
  (setf (mismatch-p the-class) t))

;;; ---------------------------------------------------------------------------
(defmethod class-dependent (the-class)
  (multiple-value-bind (dependent found)
      (gethash the-class *class->dependent-table*)
    (cond
     (found dependent)
     ((next-method-p) (call-next-method)))))

;;; ---------------------------------------------------------------------------
;;; Set/get the initarg of a slot
;;; ---------------------------------------------------------------------------

(defvar *class->slot-initarg-table* (make-hash-table :test #'eq)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping \\clsmo{}s to slot initialization
 arguments.
\\Seealsolabel
 \\Fcite{(setf slot-initarg)};
 \\fcite{slot-initarg}.")

;;; ---------------------------------------------------------------------------
(defun (setf slot-initarg) (initarg the-slot the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{initarg}}
      {a keyword symbol}
 \\isa{\\funarg{the-slot}}
      {a symbol naming a slot of \\funarg{the-class}}
 \\isacls{\\funarg{the-class}}
\\Valueslabel
 \\retarg{\\funarg{initarg}}
\\Purposelabel
 Sets the initialization argument of the slot described by
 \\funarg{the-slot}\\ of \\funarg{the-class}\\ to
 \\funarg{initarg}. The initialization argument is used when the
 transient representation of a persistent object is created by
 \\plob\\ when loading a persistent object;
 it is used as a keyword to initialize the transient slots at
 object creation time similar to the \\lisp{:initarg}\\ slot option
 used in the \\lisp{defclass}-statement of \\clos.
\\Exampleslabel
 This call changes the slot initialization argument of the slot named
 \\lisp{system::kind}\\ of class \\class{hash-table}\\ from its default
 value of \\lisp{:kind}\\ (i.e.\\ the slot name interned into the
 \\lisp{:keyword}\\ package) to \\lisp{:test}:
 \\begin{lispcode}
(setf (slot-initarg 'system::kind (find-class 'hash-table)) :test)
 \\end{lispcode}
\\Seealsolabel
 \\Fcite{slot-initarg}."

  (setf (mismatch-p the-class) t)
  (setf (slot-option *class->slot-initarg-table* the-slot the-class)
        initarg))

;;; ---------------------------------------------------------------------------
(defun slot-initarg (the-slot the-class)
"
\\Argumentslabel
 \\isa{\\funarg{the-slot}}
      {a symbol naming a slot of \\funarg{the-class}}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 The value of the initialization argument of the slot named
 \\funarg{the-slots}\\ in \\funarg{the-class}\\ is returned.
\\Seealsolabel
 \\Fcite{(setf slot-initarg)}."

  (slot-option *class->slot-initarg-table* the-slot the-class))

;;; ---------------------------------------------------------------------------
;;; Set/get the location of a slot
;;; ---------------------------------------------------------------------------
(defgeneric (setf slot-location) (location the-slot the-class)
  #+:lisp-doc (:documentation
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{location}}
      {either \\lispnil\\ or a number}
 \\isa{\\funarg{the-slot}}
      {either a symbol naming a slot of \\funarg{the-class}\\ or a \\sltmo}
 \\isacls{\\funarg{the-class}}
\\Valueslabel
 \\retarg{\\funarg{location}}
\\Purposelabel
 Sets the location of the slot described by
 \\funarg{the-slot}\\ of \\funarg{the-class}\\ to
 \\funarg{location}. This is the location used in the low-level
 functions for storing the slot's state.
\\Seealsolabel
 \\Fcite{slot-location}."))

;;; ---------------------------------------------------------------------------
(defgeneric slot-location (the-slot the-class)
  #+:lisp-doc (:documentation
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-slot}}
      {either a symbol naming a slot of \\funarg{the-class}\\ or a \\sltmo}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Return the location of the slot described by
 \\funarg{the-slot}\\ of \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{(setf slot-location)}."))

;;; ---------------------------------------------------------------------------
(defvar *class->slot-location-table* (make-hash-table :test #'eq)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping \\clsmo{}s to slot location options.
\\Seealsolabel
 \\Fcite{(setf slot-location)};
 \\fcite{slot-location}.")

;;; --------------------------------------------------------------------------
(defmethod (setf slot-location) (location (the-slot symbol) the-class)
  (setf (slot-option *class->slot-location-table* the-slot the-class)
        location))

;;; --------------------------------------------------------------------------
(defmethod (setf slot-location) (location the-slot the-class)
  (setf (slot-option *class->slot-location-table*
		     (slot-definition-name the-slot) the-class)
    location))

;;; ---------------------------------------------------------------------------
(defmethod (setf slot-location) :after (location the-slot the-class)
  (setf (mismatch-p the-class) t))

;;; ---------------------------------------------------------------------------
(defun search-location (the-slot the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-slot}}
      {a symbol naming a slot of \\funarg{the-class}}
 \\isacls{\\funarg{the-class}}
\\Valueslabel
 Returns an location for \\funarg{the-slot}.
\\Purposelabel
 Tries to find an location for the slot named \\funarg{the-slot}. If
 there is no location set in \\funarg{the-class}, the search is
 continued in the superclasses of \\funarg{the-class}."

  (when (has-slot-p the-slot the-class)
    (let ((location
	   (slot-option *class->slot-location-table* the-slot the-class)))
      (if location
	  location
	(dolist (class (class-direct-superclasses the-class))
	  (let ((location (search-location the-slot class)))
	    (when location
	      (return location))))))))

;;; ---------------------------------------------------------------------------
(defmethod slot-location ((the-slot symbol) (the-class structure-class))
  (search-location the-slot the-class))

;;; ---------------------------------------------------------------------------
(defmethod slot-location (the-slot (the-class structure-class))
  (search-location (slot-definition-name the-slot) the-class))


;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
