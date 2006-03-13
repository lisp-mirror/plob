;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-metaclass.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	9.3.94
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

;;; ---------------------------------------------------------------------------
;;; Class metaclass: persistent-metaclass
;;; ---------------------------------------------------------------------------
(defclass persistent-metaclass (standard-class)
  (
   (t-extent
    :accessor class-extent
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The extent of the class.
 This is the value of the class option \\lisp{:extent}\\ as specified
 in the \\lisp{defclass}\\ statement. For possible values and their
 meaning, see \\fcite{(setf slot-extent)}.")

   (t-schema-evolution
    :accessor schema-evolution
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The kind of schema-evolution to use for instances of the class.
 This is the value of the class option
 \\lisp{:schema-evolution}\\ as specified in the
 \\lisp{defclass}\\ statement. For possible values and their
 meaning, see \\fcite{(setf schema-evolution)}.")

   (t-constructor
    :accessor class-constructor
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The constructor function to use to create instances of the class.
 This is the value of the class option
 \\lisp{:constructor}\\ as specified in the
 \\lisp{defclass}\\ statement. For possible values and their
 meaning, see \\fcite{(setf class-constructor)}.")

   (t-dependent
    :accessor class-dependent
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The dependent option to use for instances of the class.
 This is the value of the class option
 \\lisp{:dependent}\\ as specified in the
 \\lisp{defclass}\\ statement. For possible values and their
 meaning, see \\fcite{(setf class-dependent)}.")

   (t-mismatch-p
    :accessor mismatch-p
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 A flag indicating that the class definition might have changed
 and needs to be updated in the stable heap.
 This flag is set when {\\bf [re]initialize-instance}\\ onto
 the class object was called; see also \\fcite{(setf mismatch-p)}.")

   (t-class-description
    :accessor class-description-of
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The class's \\plob\\ class description, i.e.\\ this
 is an instance of \\fcite{class-description}.")

   (t-finalized-p
    :accessor class-description-finalized-p
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 A flag indicating if the class description is finalized in the sense
 of \\plob. \\note\\ In \\lw, this finalization takes place
 {\\sl after} the \\clos\\ finalization because \\plob\\ must patch the
 class object (see \\fcite{patch-class-metaobject}) to handle a
 non-\\lw-standard slot allocation and access.")
   )
  #+:lisp-doc (:documentation "

\\Purposelabel

 A \\clsmc\\ for classes with additional class- and slot-options;
 it extends the behavior of the \\clsmc\\ \\class{standard-class}.

 Persistence in \\plob\\ is not bound to the property of an
 instance that its class is inheriting from a class which
 implements persistence\\footnote{This would be the
 pure-\\clos\\ view of implementing a property (like persistence)
 by a class and its methods: The property would be inherited via
 subclassing. The class in question for persistence if done so
 would be the \\fcite{persistent-clos-object}.}.
 The overall task of \\class{persistent-metaclass}\\ is not to offer
 the ability of persistency for instances of its instances
 (the first one are `normal' instances, the latter are \\clsmo{}s)
 since persistence is orthogonal in \\plob\\ but to provide
 some additional options on class- and slot-level
 w.r.t.\\ persistence for its instances, i.e.\\ for classes
 whose class is \\class{persistent-metaclass}.

 These are the class options which are handled additionally
 for classes being an instance of \\class{persistent-metaclass}:
 \\begin{description}

 \\item[\\lisp{:constructor}]
  This option takes as argument an instance constructor
  function as specified for argument
  \\funarg{new-constructor}\\ of
  \\fcite{(setf class-constructor)}.

 \\item[\\lisp{:dependent}]
  This option takes as argument a dependent option
  \\funarg{new-dependent}\\ of
  \\fcite{(setf class-dependent)}.

 \\item[\\lisp{:extent}]
  This option takes as argument a class default slot extent
  as specified for argument \\funarg{extent}\\ of
  \\fcite{(setf class-extent)}.

 \\item[\\lisp{:schema-evolution}]
  This option takes as argument a schema evolution
  as specified for argument \\funarg{evolution}\\ of
  \\fcite{(setf schema-evolution)}.

 \\end{description}

 These are the slot options which are handled additionally
 for classes being an instance of \\class{persistent-metaclass}:
 \\begin{description}

 \\item[\\lisp{:extent}]
  This option takes as argument a slot extent
  as specified for argument \\funarg{extent}\\ of
  \\fcite{(setf slot-extent)}.

 \\item[\\lisp{:deferred}]
  This option takes as argument a slot deferred option
  as specified for argument \\funarg{deferred}\\ of
  \\fcite{(setf slot-deferred)}.

 \\item[\\lisp{:index}]
  This option takes as argument an expression which defines
  an index for the slot value of all instances. The index
  maps a slot value to an instance; it is inherited to
  subclasses. See section \\fcite{index ...}\\ for details
  on indices.

  Currently, there are BTrees available: A slot option
  \\lisp{:index (btree :test \\textsl{\\lt{}test-mode\\gt}\\/)}
  creates a
  BTree with test mode \\textsl{\\lt{}test-mode\\gt} on the slot;
  see \\fcite{make-btree}, argument \\keyarg{test}\\ for
  details of \\textsl{\\lt{}test-mode\\gt}.

 \\item[\\lisp{:location}]
  \\emph{This option is  only for usage by \\plob's internal
   classes:} 

  Internally, the slot option \\lisp{:location}\\ is used with classes
  describing \\plob's internal classes. Compared with \\clos, these
  classes are the \\mc[es]\\ of \\plob; since they differ from \\clos'
  \\mc[es]\\ in some respects, they are called \\dec[es].  The slot option
  takes a fixnum number as argument which determines the position of the
  slot in the persistent object's layout in persistent memory. This
  handling is needed to ensure that the layers below the LISP layer of
  \\plob\\ use the same slot positions into a description object as the
  high level LISP layer, since the lower layers do not have the
  sophisticated slot access methods that \\clos\\ has (simply for
  performance reasons).

 \\end{description}

 The \\lisp{:extent}\\ option can be specified not only on
 class level but also on slot level. Furthermore, for
 computing the effective extent and the effective schema
 evolution for a slot resp.\\ a class there can be option
 values specified on the `package'-level (see
 \\fcite{(setf package-extent)}). The rule is that the most
 specific found option's value will be used; if no option's
 value is given on any level, \\plob\\ tries to find an
 option value by employing a \\clos-like class inheritance
 search. If no value can be found by this search too,
 an appropiate default value will be used.
 The order from least to most specific level for the first
 search is package $\\rightarrow$ class $\\rightarrow$ slot.
 The order of the inheritance search used for the second search
 is given by the class's precedence list.
 The appropiate default values are the constants
 {\\bf *default-\\ldots-\\{extent,schema-evolution\\}*}.

 Once defined, these options and their values should be
 held constant as long as possible.
 Adding an option on class- or slot-level or changing the
 value of one of these options in the
 \\lisp{defclass}\\ statement is a `significant' change
 which will be promoted to the \\sh\\ by updating
 the class description (topic schema-evolution).

 The `linking' of a class to \\class{persistent-metaclass}\\ is
 done with the class option
 \\lisp{(:metaclass persistent-metaclass)}.

\\Exampleslabel

 This is the definition of a class with additional options:
 \\begin{lispcode}
(defclass person ()
  ((first-name :extent :cached)
   (last-name  :extent :cached)
   (soc-sec-#  :index (btree :test equal)))
  (:extent :persistent)
  {\\bf(:metaclass persistent-metaclass)})
 \\end{lispcode}

\\Seealsolabel

 Section \\fcite{index ...};
 \\Fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defmethod validate-superclass ((class persistent-metaclass)
				(superclass standard-class))
  t)

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
