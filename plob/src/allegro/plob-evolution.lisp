;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-evolution.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	1.03.94
;;;; Description	PLOB schema evolution support
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
(use-package :clos)

;;; ---------------------------------------------------------------------------
;;; Schema evolution support
;;; ---------------------------------------------------------------------------

(defgeneric (setf schema-evolution) (evolution the-class)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{evolution}}
      {one of the symbols
       \\lisp{:no-evolution}, \\lisp{:write-back},
       \\lisp{:write-back-allow-identity-change},
       \\lisp{:write-back-deny-identity-change}\\ or
       \\lisp{:transform}}
 \\isacls{\\funarg{the-class}}
\\Valueslabel
 \\retarg{\\funarg{evolution}}
\\Purposelabel
 Set the schema evolution of \\funarg{the-class}\\ to
 \\funarg{evolution}.

 The schema evolution of a class determines what to do with the
 persistent instances of that class if the class definition in
 the transient \\cl\\ runtime environment and the persistent
 \\plob\\ environment differ, e.g.\\ by adding oder removing
 slots to the \\lisp{defstruct}- or \\lisp{defclass}-statement,
 changing the extent of slots etc.

 Schema evolution is supported for instances of
 \\lisp{defstruct}\\ and `real' (i.e.\\ non-built-in)
 \\clos\\ classes.
 The possible values for
 \\funarg{evolution}\\ have following effects:
 \\begin{description}
 \\item[\\lisp{:no-evolution}]
  Deny any request of schema evolution to \\funarg{the-class};
  this one is used for persistent classes which must not change,
  e.g.\\ the \\plob\\ base classes loaded at bootstrap.
  If \\plob\\ detects changes to \\funarg{the-class}, an error is
  raised.
 \\item[\\lisp{:write-back}]
  Try to transform an `old' object into a `new' object and to
  write back the `new' object to the persistent heap preserving
  object identity;
  if the identity cannot be preserved, an error is signalled.
  This error may only be raised for \\lisp{defstruct}\\ instances since
  their number of slots cannot be extented.
  It never raises an error for \\clos\\ objects because their persistent
  class can change without loosing the persistent object's identity.
 \\item[\\lisp{:write-back-allow-identity-change}]
  transforms an `old' object into a `new'
  object and writes it back to the \\sh; the identity of the object
  changes iff this cannot be avoided (e.g. if persistent structure slots are
  added, the identity of a structure object cannot be preserved).
  Because \\clos\\ instances never change their identity on changes done
  to the class definition, this mode is handled like
  \\lisp{:write-back}\\ for \\clos\\ instances.
 \\item[\\lisp{:write-back-deny-identity-change}]
  transforms an `old' object into a `new'
  object and writes it back to the \\sh\\ iff the identity of the
  object would not change; if the identity would change, the object is
  handled like \\lisp{:transform}\\ mode.
  Because \\clos\\ instances never change their identity on
  changes done to the class definition this mode is handled like
  \\lisp{:write-back}\\ for \\clos\\ instances.
 \\item[\\lisp{:transform}]
  Makes \\plob\\ transform the object from its `old' to its `new'
  class each time the object is loaded into transient memory. Slots
  with equal names are inherited from `old' to `new' classes; other slots
  are initialized according to their init values\\footnote{Mode
  \\lisp{:transform}\\ not yet implemented}.
 \\end{description}

 \\plob\\ does a `lazy schema evolution'. The representation of a
 persistent object whose class was modified is changed at the latest
 possible moment; normally, this is when either a slot of such an
 object is referenced (by a call to
 \\fcite{slot-value}\\ resp.\\ \\fcite{(setf slot-value)})
 or when the whole object is loaded from or stored to the
 persistent heap (by a call to
 \\fcite{load-object}\\ resp. \\fcite{store-object}).

 This functions is meant to be called from top-level to declare the
 schema evolution of predefined \\cl\\ system classes or of classes
 of other user-defined systems whose classes where created without
 the \\lisp{(:metaclass persistent-metaclass)}\\ class option.
\\Remarkslabel
 \\lw\\ has only limited support for structure instance evolution
 (see also \\stcite{473, X3J13 vote of January 1989 \\lt{}56\\gt}).
 When a \\lisp{defstruct}\\ was changed, referencing an obsolete
 structure instance raises an error. \\plob\\ has full support
 of schema evolution also for structure classes; to avoid problems
 with obsolete structure instances after a structure redefinition
 call the \\fcite{clear-cache}\\ to remove all references from
 \\plob\\ to any obsolete structure instances.
\\Exampleslabel
 This denies schema evolution for class
 \\class{structure-description}; when \\plob\\ encounters a changed class
 definition for \\class{structure-description}, an error is raised:
 \\begin{lispcode}
(setf (schema-evolution (find-class 'structure-description))
      :no-evolution)
 \\end{lispcode}
\\Seealsolabel
 \\Fcite{schema-evolution}."))

;;; ---------------------------------------------------------------------------
(defgeneric schema-evolution (the-class)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Return the schema evolution of \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{(setf schema-evolution)}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf mismatch-p) (mismatch the-class)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isabool{\\funarg{mismatch}}
 \\isacls{\\funarg{the-class}}
\\Valueslabel
  \\retarg{\\funarg{mismatch}}
\\Purposelabel
 Set the mismatch marker of \\funarg{the-class}\\ to \\funarg{mismatch}.
 This should be set when anything in the class
 definition of \\funarg{the-class}\\ changed.
 It is reset to \\lispnil\\ when the new class definition
 was written to the persistent store.
\\Remarkslabel
 Is used in conjunction with schema evolution. The class is marked
 as mismatched in
 \\fcite{reinitialize-instance :after (structure-class)}\\ resp.\\ %
 \\fcite{initialize-instance :before (standard-class)}\\ resp.\\ %
 \\fcite{reinitialize-instance :after (standard-class)};
 the mismatch is then later detected
 by \\plob\\ by requesting with \\fcite{mismatch-p}\\ if the
 mismatch marker was set.
\\Seealsolabel
 \\Fcite{mismatch-p};
 \\fcite{reinitialize-instance :after (structure-class)};
 \\fcite{initialize-instance :before (standard-class)};
 \\fcite{reinitialize-instance :after (standard-class)}."))

;;; ---------------------------------------------------------------------------
(defgeneric mismatch-p (the-class)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Check if \\funarg{the-class}\\ was redefined since the last call to
 \\fcite{(setf mismatch-p)}.
\\Seealsolabel
 \\Fcite{(setf mismatch-p)}."))

;;; ---------------------------------------------------------------------------
;;; Methods
;;; ---------------------------------------------------------------------------

(defvar *class->schema-evolution-table* (make-hash-table :test #'eq)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping \\clsmo{}s to their
 schema evolution.
\\Seealsolabel
 \\Fcite{(setf schema-evolution)};
 \\fcite{schema-evolution}.")

;;; ---------------------------------------------------------------------------
(defmethod (setf schema-evolution) (evolution the-class)
  (setf (gethash the-class *class->schema-evolution-table*) evolution)
  evolution)

;;; ---------------------------------------------------------------------------
(defmethod schema-evolution (the-class)
  (gethash the-class *class->schema-evolution-table*))

;;; ---------------------------------------------------------------------------

(defvar *class->mismatch-table* (make-hash-table :test #'eq)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping \\clsmo{}s to their
 mismatch marker.
\\Seealsolabel
 \\Fcite{(setf mismatch-p)};
 \\fcite{mismatch-p}.")

;;; ---------------------------------------------------------------------------
(defmethod (setf mismatch-p) (mismatch the-class)
  (setf (gethash the-class *class->mismatch-table*) mismatch)
  mismatch)

;;; ---------------------------------------------------------------------------
(defmethod mismatch-p (the-class)
  (gethash the-class *class->mismatch-table*))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
