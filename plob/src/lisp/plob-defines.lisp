;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	18.11.93; heavily extended at 06.12.93
;;;; Description	PLOB - Persistent Lisp OBjects
;;;;                           =          =    ==
;;;;		Main 'header' file
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
;;; Types
;;; ---------------------------------------------------------------------------
(defmacro p-immediatep (type-tag)
  #+:lisp-doc "Check if \\funarg{type-tag}\\ denominates an immediate type."
 `(/= (logand ,type-tag ,+tag-mask+) 0))
(defmacro p-markerp (type-tag)
  #+:lisp-doc "Check if \\funarg{type-tag}\\ denominates a marker type."
 `(= (logand ,type-tag ,+tag-mask+) ,+marker-type-tag+))
(defun p-markerp-fn (type-tag)
  (p-markerp type-tag))

(deftype p-marker-type ()
  #+:lisp-doc "Marker type definition."
 `(satisfies p-markerp-fn))

;;; ---------------------------------------------------------------------------
;;; Package extents
;;; ---------------------------------------------------------------------------
(setf (package-extent (find-package :clos)) :transient)
(setf (package-extent (find-package :plob)) :cached)

;;; ---------------------------------------------------------------------------
;;; Opening/closing of persistent heaps
;;; ---------------------------------------------------------------------------
(defgeneric open-heap (&optional (url *database-url*))
  #+:lisp-doc (:documentation "
\\Purposelabel
 Open the \\sh\\ for usage. When the \\sh\\ is found to be empty
 (i.e.\\ opened the very first time), {\\bf format-plob-root}
 is called. The description objects (comparable to
 the \\mo[s]\\ of \\clos) are loaded from the \\sh.

 There is no need to call function {\\bf open-heap} explicit
 (although this is possible); this will happen the first time the
 \\sh\\ is needed. Its main purpose is to specialise
 \\lisp{:after}\\ methods on \\textbf{open-heap}\\ to load persistent
 objects into a transient cache.
\\Remarkslabel
 This still needs some more work to be done, because there can be
 only one \\lisp{:after}\\ method on generic functions without
 arguments which could be specialised any further.
\\Seealsolabel
 \\Fcite{close-heap};
 \\fcite{open-session};
 \\fcite{format-plob-root};
 section \\fcite{bootstrap ...}."))

;;; ---------------------------------------------------------------------------
(defgeneric close-heap (&optional (with-garbage-collection t))
  #+:lisp-doc (:documentation "
\\Purposelabel
 Close the \\sh.
 Its purpose is also to have specialised
 \\lisp{:after}\\ methods on \\textbf{open-heap}\\ to invalidate
 the transient representation of cached persistent objects.
\\Remarkslabel
 This still needs some more work to be done, because there can be
 only one \\lisp{:after}\\ method on generic functions without
 arguments which could be specialised any further."))

;;; ---------------------------------------------------------------------------
;;; Persistent objects
;;; ---------------------------------------------------------------------------

(defgeneric persistent-object-objid (p-object)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isa{\\funarg{p-object}}
      {a persistent object}
\\Purposelabel
 Return the \\objid\\ of \\funarg{p-object}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf persistent-object-objid) (the-objid p-object)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isanobjid{\\funarg{the-objid}}
 \\isa{\\funarg{p-object}}
      {a persistent object}
\\Purposelabel
 Set the \\objid\\ of \\funarg{p-object}\\ to \\funarg{the-objid}."))

;;; ---------------------------------------------------------------------------
(defstruct (persistent-object
            ;; For creating persistent objects do not use
            ;; make-persistent-object-internal but the function
            ;; make-persistent-object:
            (:conc-name persistent-object-internal-)
            (:constructor make-persistent-object-internal (&optional objid))
            (:print-function (lambda (object stream depth)
			       (declare (ignore depth))
			       (print-persistent-immediate-object
				object +short-objid-tag+ stream))))
  #+:lisp-doc "
\\Purposelabel
 A structure for representing persistent objects in the transient
 \\cl\\ system. This structure can represent only non-immediate objects.

 The specified \\lisp{:print-function}\\ will not print the `naked'
 numeric \\objid\\ of a persistent object but a more elaborate form.
\\Remarkslabel
 \\basecls{persistent-object}"

  (objid
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The (numeric) \\objid\\ of the persistent object."))

;;; ---------------------------------------------------------------------------
;;; Mark persistent-object as transient:
;;; ---------------------------------------------------------------------------
(setf (class-extent (find-class 'persistent-object)) :transient)

;;; ---------------------------------------------------------------------------
;;; Persistent heaps
;;; ---------------------------------------------------------------------------

(defstruct (persistent-heap
            (:include persistent-object)
            (:constructor make-persistent-heap (&optional objid)))
  #+:lisp-doc "
\\Purposelabel
 A structure for representing persistent heaps in the transient
 \\cl\\ system. Persistent heaps are used for two purposes:
 \\begin{enumerate}

 \\item For the \\plob\\ C level functions, instances of
  \\class{persistent-heap}\\ represent the calling \\cl\\ session,
  e.g.\\ session suspend resp.\\ wakeup requests used for lock
  conflict resolution are sent to the \\cl\\ level from the C
  level by identifying the \\cl\\ session
  to suspend resp.\\ wakeup by its instance of
  \\class{persistent-heap}.
  %% \\ (see \\fcite{sh-suspend-callback},
  %% \\fcite{sh-wakeup-callback})%
  1996/10/30 HK: Suspend and wakeup are now done by a busy wait
  on the lock to get granted to the object which should be locked.

 \\item For the \\cl\\ session, instances of
  \\class{persistent-heap}\\ represent the `logical' heap which
  is used to store and load the
  persistent objects, e.g.\\ the transaction handling and locking
  for a \\cl\\ session is done by an instance of
  \\class{persistent-heap}, i.e.\\ by the persistent heap object
  contained in the session-local variable
  {\\bf *default-persistent-heap*}.

 \\end{enumerate}
\\Remarkslabel
 \\basecls{persistent-heap}
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{*default-persistent-heap*};
 %% \\fcite{sh-suspend-callback};
 %% \\fcite{sh-wakeup-callback};
 \\fcite{begin-transaction}."

  (in-transaction
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 A boolean flag indicating if the heap is currently in a transaction.")

  (suspended-p
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 A boolean flag indicating if the \\cl\\ session to which the heap belongs
 is suspended. Once upon a time, this flag was set by a call to
 \\textbf{sh-suspend-callback}\\ and reset by a call to
 \\textbf{sh-wakeup-callback})\\ with the \\objid\\ of the
 session-identifying persistent heap as argument. In the current
 client/server version of \\plob, suspending the LISP process is not
 supported; a busy wait is done where a suspend would be appropiate.")

  (pid
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The \\cl\\ PID of the current session as returned by
 \\fcite{process-pid}."))

;; Mark persistent-heap as transient:
(setf (class-extent (find-class 'persistent-heap)) :transient)

;;; ---------------------------------------------------------------------------
(defstruct (cached-heap
            (:include persistent-heap)
            (:constructor make-cached-heap (&optional objid)))
  #+:lisp-doc "
\\Purposelabel
 A structure for cached persistent heaps.
\\Remarkslabel
 Useful only for internal purposes; the only instance of
 \\class{cached-heap}\\ used is contained in variable
 {\\bf *root-persistent-heap*}.
\\Seealsolabel
 \\Fcite{*root-persistent-heap*}."

  (objid->object-cache
   (make-hash-table :test #'eql :size 100000 :rehash-size 2.0)
   #+:lisp-doc :documentation #+:lisp-doc "
 Cache containing all \\cl\\ objects seen so far.
 Key is an \\objid, associated data is the transient \\cl\\ object
 representing the persistent object referenced by that \\objid.
 Should be a hash table with `weak pointers' for both keys and values
 (but \\lwcl\\ does not offer this).")

  (object->objid-cache
   (make-hash-table :test #'eq :size 100000 :rehash-size 2.0)
   #+:lisp-doc :documentation #+:lisp-doc "
 This is the inverse for {\\bf objid->object-cache}. Should be
 a hash table with `weak pointers' for both keys and values (but
 \\lwcl\\ does not offer this)."))

;;; ---------------------------------------------------------------------------
;; Mark persistent-heap as transient:
(setf (class-extent (find-class 'cached-heap)) :transient)

;;; ---------------------------------------------------------------------------
;;; BTrees
;;; ---------------------------------------------------------------------------
(defstruct (persistent-btree
            (:include persistent-object)
            (:constructor make-persistent-btree (&optional objid)))
  #+:lisp-doc "
\\Purposelabel
 A structure for representing persistent BTrees in the transient
 \\cl\\ system.
\\Seealsolabel
 \\Fcite{make-btree};
 \\fcite{cached-btree}.")

;;; ---------------------------------------------------------------------------
;; Mark persistent-btree as transient:
(setf (class-extent (find-class 'persistent-btree)) :transient)

;;; ---------------------------------------------------------------------------
(defstruct (persistent-btree-mapper
            (:include persistent-object)
            (:constructor make-persistent-btree-mapper (&optional objid)))
  #+:lisp-doc "
\\Purposelabel
 A structure for representing persistent BTree mappers in the transient
 \\cl\\ system.
\\Seealsolabel
 \\Fcite{make-btree-mapper}.")

;;; ---------------------------------------------------------------------------
;; Mark persistent-btree-mapper as transient:
(setf (class-extent (find-class 'persistent-btree-mapper)) :transient)

;;; ---------------------------------------------------------------------------
(defstruct (cached-btree
            (:include persistent-btree)
            (:constructor make-cached-btree (&optional objid)))
  #+:lisp-doc "
\\Purposelabel
 A structure for representing persistent BTrees with an associated
 transient cache.
\\Remarkslabel
 To create a cached persistent BTree, call \\fcite{make-btree}\\ with the
 \\keyarg{cached}\\ argument set to \\nonnil.
\\Seealsolabel
 \\Fcite{make-btree};
 \\fcite{persistent-btree}."

  (key->data-cache
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 A cache mapping a BTree key to its data item.
 This is a hash table created in \\fcite{make-btree}\\ with a test mode
 as given by the \\keyarg{test}\\ argument of {\\bf make-btree};
 the hash table key is an \\objid, the data is an instance of
 \\fcite{btree-cache-entry}."))

;;; ---------------------------------------------------------------------------
;; Mark cached-btree as transient:
(setf (class-extent (find-class 'cached-btree)) :transient)

;;; ---------------------------------------------------------------------------
;;; Global variables
;;; ---------------------------------------------------------------------------
(defvar *root* (make-persistent-object-internal)
  #+:lisp-doc "
\\Purposelabel
 The \\plob\\ root object. Try loading it into the \\lw\\ inspector,
 you'll like it \\ldots")

;;; ---------------------------------------------------------------------------
(defvar *root-persistent-heap* nil
  #+:lisp-doc "
\\Purposelabel
 The persistent heap object which is used at bootstrap in
 \\fcite{open-heap}\\ and whose cache is used for caching all
 transient \\cl\\ objects seen by \\plob.
 {\\bf *root-persistent-heap*} contains an instance of
 \\fcite{cached-heap}.

 This heap is also used for storing and loading of metaobjects.
\\Seealsolabel

 \\Fcite{cached-heap};
 \\fcite{open-heap}.")

;;; ---------------------------------------------------------------------------
(defvar *root-persistent-heap-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the
 \\fcite{*root-persistent-heap*}.")

;;; ---------------------------------------------------------------------------
(setf *root-persistent-heap* (make-cached-heap *root-persistent-heap-objid*))

;;; ---------------------------------------------------------------------------
(defvar *root-persistent-heap-objid->object-cache* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the {\\bf objid->object-cache} cache of the value of the
 variable {\\bf *root-persistent-heap*}\\ directly for faster access.
\\Seealsolabel
 \\Fcite{*root-persistent-heap*}.")

;;; ---------------------------------------------------------------------------
(setf *root-persistent-heap-objid->object-cache*
      (cached-heap-objid->object-cache *root-persistent-heap*))

;;; ---------------------------------------------------------------------------
(defvar *root-persistent-heap-object->objid-cache* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the {\\bf object->objid-cache} cache of the value of the
 variable {\\bf *root-persistent-heap*}\\ directly for faster access.
\\Seealsolabel
 \\Fcite{*root-persistent-heap*}.")

;;; ---------------------------------------------------------------------------
(setf *root-persistent-heap-object->objid-cache*
      (cached-heap-object->objid-cache *root-persistent-heap*))

;;; ---------------------------------------------------------------------------
(defvar *hash-table->persistent-hash-table*
    (make-hash-table :test 'eq)
  #+:lisp-doc "
\\Purposelabel
 A hash table mapping transient hash tables to persistent hash tables.")

;;; ---------------------------------------------------------------------------
(defvar *default-persistent-heap* nil
  #+:lisp-doc "
\\Purposelabel
 The persistent heap object which is used as default.
 The variable {\\bf *default-persistent-heap*} is a session-local variable.
 For all \\plob\\ `high-level' functions, the value of
 {\\bf *default-persistent-heap*} is passed as an {\\opt} parameter and
 serves for the purposes explained at \\fcite{persistent-heap}.
\\Seealsolabel
 \\Fcite{persistent-heap};
 \\fcite{make-process-variable}.")

;;; ---------------------------------------------------------------------------
(setf *default-persistent-heap* (make-persistent-heap nil))

;;; ---------------------------------------------------------------------------
(defvar *name->package-table* (make-cached-btree)
  #+:lisp-doc "
\\Purposelabel
 The persistent package table.
 Maps a package name string to a persistent package object.
\\Seealsolabel
 \\Fcite{p-find-package}.")

;;; ---------------------------------------------------------------------------
;;(eval-when (:compile-toplevel :execute)
;;  (setf *name->package-table* (make-cached-btree)))

;;; ---------------------------------------------------------------------------
(defvar *symbol->class-table* (make-cached-btree)
  #+:lisp-doc "
\\Purposelabel
 The persistent class table.
 Maps a persistent symbol to a persistent class description object.
\\Seealsolabel
 \\Fcite{p-find-class}.")

;;; ---------------------------------------------------------------------------
;;(eval-when (:compile-toplevel :execute)
;;  (setf *symbol->class-table* (make-cached-btree)))

;;; ---------------------------------------------------------------------------
(defvar *plob-base-objects* ()
  #+:lisp-doc "
\\Purposelabel
 Contains the \\plob\\ objects which {\\sl must} be under all
 circumstances be found in the \\plob\\ root cache.
 It contains a list with a-cons'es
 \\lisp{(\\objid\\ .
 \\textrm{\\textsl{\\lt{}transient \\cl\\ object\\gt}}\\/)}.
\\Seealsolabel
 \\Fcite{*root-persistent-heap-objid->object-cache*};
 \\fcite{*root-persistent-heap-object->objid-cache*}.")

;;; ---------------------------------------------------------------------------
(defun print-persistent-immediate-object
    (object type-tag stream &optional (p-heap *default-persistent-heap*))
  (let ((objid (persistent-object-objid object))
        (immediatep (p-immediatep type-tag)))
    (if (or immediatep (sh-objid-valid-p objid))
	(let ((p-heap-objid (persistent-object-objid p-heap)))
	  (unless p-heap-objid
	    (setf p-heap-objid +null-objid+))
          (unless immediatep
            (setf type-tag +short-objid-tag+))
          (format stream "~A" (sh-pprint-objid p-heap-objid objid type-tag)))
      (format stream "#<~A short-objid=~A type-tag=~A>"
	      (class-name (class-of object))
	      objid type-tag))))

;;; ---------------------------------------------------------------------------
(defstruct (persistent-immediate-object
            (:include persistent-object)
            (:constructor make-persistent-immediate-object
			  (&optional objid (type-tag +short-objid-tag+)))
            (:print-function (lambda (object stream depth)
			       (declare (ignore depth))
			       (print-persistent-immediate-object
				object (p-type-tag-of object) stream))))
  #+:lisp-doc "
\\Purposelabel
 A structure for representing persistent objects in the transient
 \\cl\\ system. This structure can represent non-immediate and 
 immediate objects.
\\Remarkslabel
 \\basecls{persistent-immediate-object}"

  (type-tag
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The (numeric) \\typetag\\ of the persistent object."))

;;; ---------------------------------------------------------------------------
;;; Mark persistent-immediate-object as transient:
;;; ---------------------------------------------------------------------------
(setf (class-extent (find-class 'persistent-immediate-object)) :transient)

;;; ---------------------------------------------------------------------------
;;; Generic functions
;;; ---------------------------------------------------------------------------

;;; --- Low-level creation/accessor functions ---------------------------------

;;; postore object record layout used by plob:
;;;
;;;        -----------------------------------------------------------
;;;       | Lock- | Number of | Size | Type | (m)  | (n-m-3) Non-keys |
;;;       | Word  | Keys (=m) | (=n) | tag  | Keys | (i.e. values)    |
;;;        -----------------------------------------------------------
;;;                                    Key index:    Value index:
;;;                                    [0]    [1..m] [0..n-m-2]
;;;
;;;         persistent-heap accessor functions:
;;;
;;;         (p-lock ...)
;;;         (setf (p-lock ...) ...)
;;;
;;;                 (p-objid-size ...)
;;;
;;;                             (p-value-size ...)
;;;
;;;                                    (p-type-tag-of ...)
;;;
;;;                                           (p-fixnum ...)
;;;                                           (setf (p-fixnum ...) ...)
;;;                                           (p-marker ...)
;;;                                           (setf (p-marker ...) ...)
;;;                                           (p-objid ...)
;;;                                           (setf (p-objid ...) ...)
;;;
;;;                                                  (p-values ...)
;;;                                                  (setf (p-values ...) ...)
;;;
;;; This structure is called sh-vector (stable-heap-vector). The
;;;                                     =      =    ======
;;; address of a sh-vector is called its objid. In the low-low-level
;;; functions in modules c-plob*.lisp and POSTORE itself an objid is
;;; called a 'key'. There is no explicit concept for a sh-vector in
;;; postore; in the man pages of postore it is simply called
;;; 'object format'.

;;; --- Transactions on persistent heaps --------------------------------------

;;; --- Registering of objects to persistent heaps caches & tables ------------

;;; --- Storing and loading of objects to and from persistent heaps -----------

;;; ---------------------------------------------------------------------------

(defgeneric t-object-to-p-objid (t-object depth to-p-heap)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isanobject{\\funarg{t-object}}
\\Valueslabel
 Two values are returned:
 \\begin{enumerate}

 \\item The \\objid\\ of the stored object \\funarg{t-object}.

 \\item The \\typetag\\ of the stored object \\funarg{t-object}.
  If the second value is \\lispnil, the first value references a
  non-immediate persistent object; its
  \\typetag\\ can therefore assumed to be {\\bf +short-objid-tag+}.

 \\end{enumerate}
\\Purposelabel
 The workhorse for \\fcite{store-object}: Stores
 \\funarg{t-object}\\ to the stable heap.
\\Seealsolabel
 \\Fcite{store-object};
 \\fcite{t-object-to-p-objid-using-class};
 \\fcite{p-objid-to-t-object}."))

;;; ---------------------------------------------------------------------------
(defgeneric t-object-to-p-objid-using-class (t-object t-class depth p-heap)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isacls{\\funarg{t-class}}
 Rest see \\fcite{t-object-to-p-objid}.
\\Valueslabel
 See \\fcite{t-object-to-p-objid}.
\\Purposelabel
 Workhorse for \\fcite{t-object-to-p-objid}; it is called when no
 other method for \\fcite{t-object-to-p-objid}\\ was found.
 The \\funarg{t-class}\\ argument is the class object of
 \\funarg{t-object}.
\\Seealsolabel
 \\Fcite{t-object-to-p-objid}."))

;;; ---------------------------------------------------------------------------
(defgeneric p-objid-to-t-object (p-objid p-objid-type-tag depth p-heap)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{p-objid}}
      {either a numeric immediate value or an \\objid}
 \\isatypetag{\\funarg{p-objid-type-tag}}
\\Valueslabel
 A transient representation of the persistent object referenced by
 \\funarg{p-objid}\\ is returned.
\\Purposelabel
 The workhorse for \\fcite{load-object}: Loads the persistent object
 referenced by \\funarg{p-objid}\\ from the \\sh\\ into a transient
 representation directly usable by \\cl.
\\Seealsolabel
 \\Fcite{load-object};
 \\fcite{t-object-to-p-objid}."))

;;; ---------------------------------------------------------------------------
(defgeneric load-instance-for-class
    (t-class-name p-class-descr p-objid depth p-heap &optional t-into-object)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isasymbol{\\funarg{t-class-name}}
 \\isa{\\funarg{p-class-descr}}
      {either a structure class description or a \\clos\\ class description}
 \\isa{\\funarg{p-objid}}
      {either a numeric immediate value or an \\objid}
\\Valueslabel
 See \\fcite{p-objid-to-t-object}.
\\Purposelabel
 Load an instance from the \\sh\\ into the transient memory.

 This generic function can be used to specialize load methods on
 structure and \\clos\\ classes. 
\\Seealsolabel
 \\Fcite{p-objid-to-t-object}."))

;;; ---------------------------------------------------------------------------
(defgeneric load-instance-for-redefined-class
    (t-class-name p-class-descr-old p-class-descr-new p-objid depth p-heap
     &optional t-into-object)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isasymbol{\\funarg{t-class-name}}
 \\isa{\\funarg{p-class-descr-old}}
      {either a structure class description or a \\clos\\ class description}
 \\isa{\\funarg{p-class-descr-new}}
      {either a structure class description or a \\clos\\ class description}
 \\isa{\\funarg{p-objid}}
      {either a numeric immediate value or an \\objid}
\\Valueslabel
 See \\fcite{p-objid-to-t-object}.
\\Purposelabel
 Load an instance from the \\sh\\ into the transient memory
 for an instance whose class definition was changed from
 \\funarg{p-class-descr-old}\\ to \\funarg{p-class-descr-new}.

 This generic function can be used to specialize load methods on
 structure and \\clos\\ classes. 
\\Seealsolabel
 \\Fcite{p-objid-to-t-object}."))

;;; ---------------------------------------------------------------------------
(defgeneric load-object-into (p-objid into-t-object depth p-heap)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isanobject{\\funarg{into-t-object}}
\\Valueslabel
 \\retarg{\\funarg{into-t-object}}
\\Purposelabel
 Load a persistent object referenced by
 \\funarg{p-objid}\\ destructively into \\funarg{into-t-object}.
 This generic function is for internal use only;
 it has specialized methods only for a few
 \\funarg{into-t-object}'s classes.
\\Seealsolabel
 \\Fcite{p-objid-to-t-object}."))

;;; ---------------------------------------------------------------------------
;;; Generic functions for reading class- and slot-descriptions.
;;; ---------------------------------------------------------------------------

(defgeneric class-description-next-generation (class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Return the next generation of \\funarg{class-description}.
\\Seealsolabel
 Slot {\\bf p-next-generation} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric class-description-of (class)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isacls{\\funarg{class}}
\\Purposelabel
 Return the class description of \\funarg{class}.
\\Seealsolabel
 \\Fcite{structure-description};
 \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric class-description-persistent-slot-numbers (class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Return the number of persistent slots of \\funarg{class-description}.
\\Seealsolabel
 Slot {\\bf p-persistent-slot-numbers} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric class-description-schema-evolution (class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Return the schema evolution of \\funarg{class-description}.
\\Seealsolabel
 Slot {\\bf p-schema-evolution} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric class-description-slot-numbers (class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Return the total number of slots of \\funarg{class-description}.
\\Seealsolabel
 Slot {\\bf p-slot-numbers} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric class-description-time-stamp (class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Return the time stamp of \\funarg{class-description}.
\\Seealsolabel
 Slot {\\bf p-time-stamp} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric class-description-version-number (class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Returns the version number of \\funarg{class-description}.
\\Seealsolabel
 Slot {\\bf p-version-number} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric class-description-name->slot-cache (class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Return the slot name to slot description cache of
 \\funarg{class-description}.
\\Seealsolabel
 Slot {\\bf p-name->slot-cache} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric slot-description-extent (slot-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{slot-description}}
      {a structure-slot-description or a slot-description}
\\Purposelabel
 Returns the extent of \\funarg{slot-description}.
\\Seealsolabel
 Slot {\\bf p-extent} of \\fcite{structure-slot-description}\\ resp.\\ %
 \\fcite{slot-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric slot-description-deferred (slot-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{slot-description}}
      {a structure-slot-description or a slot-description}
\\Purposelabel
 Returns the deferred option of \\funarg{slot-description}.
\\Seealsolabel
 Slot {\\bf p-deferred} of \\fcite{structure-slot-description}\\ resp.\\ %
 \\fcite{slot-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric slot-description-index (slot-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{slot-description}}
      {a structure-slot-description or a slot-description}
\\Purposelabel
 Returns the index of \\funarg{slot-description}.
\\Seealsolabel
 Slot {\\bf p-index} of \\fcite{slot-description}."))

;;; ---------------------------------------------------------------------------
;;; Predefined structure: persistent structure
;;; ---------------------------------------------------------------------------

(defstruct (persistent-structure
            (:include persistent-object))
  #+:lisp-doc "
\\Purposelabel
 A transient structure representing persistent structures.
 All other persistent structures inherit implicit or explicit
 from \\class{persistent-structure}.
\\Remarkslabel
 \\basecls{persistent-structure}"

  (p-descr
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 An instance of \\class{structure-description}\\ in
 transient \\cl\\ resp.\\ an \\objid\\ referencing a persistent
 instance of \\class{structure-description}\\ in the stable heap
 which describes the stucture of the structure."))

;;; ---------------------------------------------------------------------------
;;; Mark slots of persistent-structure as persistent without demand loading:
;;; ---------------------------------------------------------------------------
(setf (slot-extent 'objid (find-class 'persistent-structure)) :transient)
(setf (slot-extent 'p-descr (find-class 'persistent-structure)) :cached)

;;; ---------------------------------------------------------------------------
;;; Set the slot's locations of persistent-structure:
;;; ---------------------------------------------------------------------------
(setf (slot-location 'p-descr (find-class 'persistent-structure))
  +structure-location-description+)

;;; ---------------------------------------------------------------------------
;;; Predefined structure: persistent structure description
;;; ---------------------------------------------------------------------------

(defstruct (structure-description
	    (:constructor make-structure-description-internal)
            (:copier copy-structure-description-internal)
            (:include persistent-structure))
  #+:lisp-doc "
\\Purposelabel
 A transient structure representing persistent structure descriptions.
 In the sense of \\clos, this class corresponds roughly to the
 \\std\\ \\clsmc\\ \\class{standard-class}.
\\Remarkslabel
 \\basecls{structure-description}"

  (p-name nil
	  #+:lisp-doc :documentation #+:lisp-doc "
 The name of the structure as a symbol.")

  (p-version-number 100
		    :type fixnum
		    #+:lisp-doc :documentation #+:lisp-doc "
 A schema evolution version number; 100 means version 1.00.
 The number is incremented by 1 for each new version.")

  (p-time-stamp (floor (get-universal-time) 60)
		:type integer
		#+:lisp-doc :documentation #+:lisp-doc "
 A time stamp when the structure description was created. The time stamp
 is in \\cl\\ Universal Time divided by 60, i.e.\\ the time is in
 minutes, not in seconds.")

  (p-schema-evolution *default-structure-schema-evolution*
		      #+:lisp-doc :documentation #+:lisp-doc "
 A symbol describing the type of schema evolution to use for the
 structure;
 see \\fcite{schema-evolution}\\ and
 \\fcite{(setf schema-evolution)}.")

  (p-next-generation nil
		     #+:lisp-doc :documentation #+:lisp-doc "
 The next generation of the structure description;
 this is either \\lispnil\\ or a pointer to an instance of class
 \\class{structure-description}\\ with a higher version number.")

  (p-constructor nil
		 #+:lisp-doc :documentation #+:lisp-doc "
 The name of the structure constructor function as a symbol;
 see \\fcite{class-constructor}\\ and
 \\fcite{(setf class-constructor)}.")

  (p-dependent nil
	       #+:lisp-doc :documentation #+:lisp-doc "
 The dependent flag of the structure as a symbol;
 see \\fcite{class-dependent}\\ and
 \\fcite{(setf class-dependent)}.")

  (p-persistent-slot-numbers 0
			     :type fixnum
			     #+:lisp-doc :documentation #+:lisp-doc "
 The number of persistent slots of the described structure.")

  (p-slot-numbers 0
		  :type fixnum
		  #+:lisp-doc :documentation #+:lisp-doc "
 The total number of slots of the described structure.")

  (p-slots nil
	   #+:lisp-doc :documentation #+:lisp-doc "
 A vector with instances of
 \\fcite{structure-slot-description}\\ for each slot of the
 described structure.")

  (t-name->slot-cache (make-hash-table :test #'eq)
		      #+:lisp-doc :documentation #+:lisp-doc "
 A transient slot with a hash table mapping slot names to
 instances of \\fcite{slot-description}. It is used for fast
 mapping of a slot name to its corresponding instance of
 \\fcite{structure-slot-description}\\ in various methods
 of \\fcite{slot-value-using-class}\\ and
 \\fcite{(setf slot-value-using-class)}."))

;;; ---------------------------------------------------------------------------
;;; Mark most slots of structure-description as persistent
;;; without demand loading:
;;; ---------------------------------------------------------------------------
(setf (slot-extent 'p-name (find-class 'structure-description))
      :cached)
(setf (slot-extent 'p-version-number (find-class 'structure-description))
      :cached)
(setf (slot-extent 'p-time-stamp (find-class 'structure-description))
      :cached)
(setf (slot-extent 'p-schema-evolution (find-class 'structure-description))
      :cached)
(setf (slot-extent 'p-next-generation (find-class 'structure-description))
      :cached)
(setf (slot-extent 'p-constructor (find-class 'structure-description))
      :cached)
(setf (slot-extent 'p-dependent (find-class 'structure-description))
      :cached)
(setf (slot-extent 'p-persistent-slot-numbers
		   (find-class 'structure-description))
      :cached)
(setf (slot-extent 'p-slot-numbers (find-class 'structure-description))
      :cached)
(setf (slot-extent 'p-slots (find-class 'structure-description))
      :cached)
(setf (slot-extent 't-name->slot-cache (find-class 'structure-description))
      :transient)

;;; ---------------------------------------------------------------------------
;;; Set the slot's locations of structure-description:
;;; ---------------------------------------------------------------------------
(setf (slot-location 'p-name (find-class 'structure-description))
  +structure-description-location-name+)
(setf (slot-location 'p-version-number (find-class 'structure-description))
  +structure-description-location-version-number+)
(setf (slot-location 'p-time-stamp (find-class 'structure-description))
  +structure-description-location-time-stamp+)
(setf (slot-location 'p-schema-evolution (find-class 'structure-description))
  +structure-description-location-schema-evolution+)
(setf (slot-location 'p-next-generation (find-class 'structure-description))
  +structure-description-location-next-generation+)
(setf (slot-location 'p-constructor (find-class 'structure-description))
  +structure-description-location-constructor+)
(setf (slot-location 'p-dependent (find-class 'structure-description))
  +structure-description-location-dependent+)
(setf (slot-location 'p-persistent-slot-numbers
		     (find-class 'structure-description))
  +structure-description-location-persistent-slot-numbers+)
(setf (slot-location 'p-slot-numbers (find-class 'structure-description))
  +structure-description-location-slot-numbers+)
(setf (slot-location 'p-slots (find-class 'structure-description))
  +structure-description-location-slot-descriptions+)

(setf (class-constructor (find-class 'structure-description))
  'make-structure-description-internal)

;;; ---------------------------------------------------------------------------
;;; structure-description is a PLOB base class;
;;; deny schema evolution for it:
;;; ---------------------------------------------------------------------------
(setf (schema-evolution (find-class 'structure-description)) :no-evolution)

;;; ---------------------------------------------------------------------------
(defvar *structure-description-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the variable
 {\\bf *structure-description*}.
\\Seealsolabel
 \\Fcite{*structure-description*}.")

;;; ---------------------------------------------------------------------------
(defvar *structure-description* nil
  #+:lisp-doc "
\\Purposelabel
 Contains a persistent object of
 \\fcite{structure-description}\\ describing the
 \\fcite{structure-description}.
\\Seealsolabel
 \\Fcite{*structure-description-objid*}.")

;;; ---------------------------------------------------------------------------
;;; Predefined structure: persistent structure slot description
;;; ---------------------------------------------------------------------------

(defstruct (structure-slot-description
	    (:constructor make-structure-slot-description-internal)
            (:include persistent-structure))
  #+:lisp-doc "
\\Purposelabel
 A transient structure representing persistent structure slot
 descriptions.
 In the sense of \\clos, this class corresponds roughly to the
 \\std\\ \\sltmc\\ \\class{standard-effective-slot-definition}.
\\Remarkslabel
 \\basecls{structure-slot-description}"

  (p-name
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The name of the slot as a symbol.")

  (p-initarg
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The initialization argument for initializing the slot
 at instance creation;
 default is the {\\bf p-name} of the slot interned
 to the \\lisp{:keyword}\\ package. For non-standard
 initialization arguments see \\fcite{(setf slot-initarg)}.")

  (p-reader
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The slot reader function's symbol.")

  (p-location
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The index of the slot into the stable heap vector.")

  (p-init
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The init-form of the slot. This is the {\\sl default-init}
 of the slot as specified for \\fcite{defstruct},
 definition of structure slots.")

  (p-type
   t
   #+:lisp-doc :documentation #+:lisp-doc "
 The type of the slot. This is the value of the
 \\lisp{:type}\\ structure slot option as specified for
 \\fcite{defstruct}, definition of structure slots.")

  (p-extent
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The extent of the slot;
 see \\fcite{slot-extent}\\ and, for possible values,
 \\fcite{(setf slot-extent)}.")

  (p-deferred
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The deferred option of the slot;
 see \\fcite{slot-deferred}\\ and, for possible values,
 \\fcite{(setf slot-deferred)}.")

  (t-writer
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The slot writer function's symbol."))

;;; ---------------------------------------------------------------------------
;;; Mark slots of structure-description as persistent without demand loading:
;;; ---------------------------------------------------------------------------
(setf (slot-extent 'p-name (find-class 'structure-slot-description))
      :cached)
(setf (slot-extent 'p-initarg (find-class 'structure-slot-description))
      :cached)
(setf (slot-extent 'p-reader (find-class 'structure-slot-description))
      :cached)
(setf (slot-extent 'p-location (find-class 'structure-slot-description))
      :cached)
(setf (slot-extent 'p-init (find-class 'structure-slot-description))
      :cached)
(setf (slot-extent 'p-type (find-class 'structure-slot-description))
      :cached)
(setf (slot-extent 'p-extent (find-class 'structure-slot-description))
      :cached)
(setf (slot-extent 'p-deferred (find-class 'structure-slot-description))
      :cached)
(setf (slot-extent 't-writer (find-class 'structure-slot-description))
      :transient)

(setf (class-dependent (find-class 'structure-slot-description))
      :read)

(setf (class-constructor (find-class 'structure-slot-description))
  'make-structure-slot-description-internal)

;;; ---------------------------------------------------------------------------
;;; Set the slot's locations of structure-slot-description:
;;; ---------------------------------------------------------------------------
(setf (slot-location 'p-name (find-class 'structure-slot-description))
      +structure-slot-description-location-name+)
(setf (slot-location 'p-initarg (find-class 'structure-slot-description))
      +structure-slot-description-location-initarg+)
(setf (slot-location 'p-reader (find-class 'structure-slot-description))
      +structure-slot-description-location-reader+)
(setf (slot-location 'p-location (find-class 'structure-slot-description))
      +structure-slot-description-location-location+)
(setf (slot-location 'p-init (find-class 'structure-slot-description))
      +structure-slot-description-location-init+)
(setf (slot-location 'p-type (find-class 'structure-slot-description))
      +structure-slot-description-location-type+)
(setf (slot-location 'p-extent (find-class 'structure-slot-description))
      +structure-slot-description-location-extent+)
(setf (slot-location 'p-deferred (find-class 'structure-slot-description))
      +structure-slot-description-location-deferred+)

;;; ---------------------------------------------------------------------------
;;; structure-slot-description is a PLOB base class;
;;; deny schema evolution for it:
;;; ---------------------------------------------------------------------------
(setf (schema-evolution (find-class 'structure-slot-description))
      :no-evolution)

;;; ---------------------------------------------------------------------------
(defvar *structure-slot-description-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the
 \\fcite{*structure-slot-description*}.
\\Seealsolabel
 \\Fcite{*structure-slot-description*}.")

;;; ---------------------------------------------------------------------------
(defvar *structure-slot-description* nil
  #+:lisp-doc "
\\Purposelabel
 Contains a persistent object of
 \\fcite{structure-description}\\ describing the
 \\fcite{structure-slot-description}.
\\Seealsolabel
 \\Fcite{*structure-slot-description-objid*}.")

;;; ---------------------------------------------------------------------------
;;; Predefined structure: persistent package
;;; ---------------------------------------------------------------------------
(defstruct (persistent-package
            (:include persistent-structure))
  #+:lisp-doc "
\\Purposelabel
 A transient structure representing persistent packages.
 In the sense of \\cl, this class corresponds roughly to the
 class \\class{package}.
\\Remarkslabel
 \\basecls{persistent-package}"

  (p-package-name
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 The package name as a string; comparable with the returned
 value from
 \\fcite{package-name}.")

  (p-internals
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 A table (i.e.\\ BTree) with the internal symbols of the package.
 Currently all persistent \\plob\\ symbols are intern symbols of their
 persistent package; there are no export, use-package etc.\\ functions
 defined in \\plob. Instead, the transient symbols found in the
 \\cl\\ system are mapped to the appropiate \\plob\\ symbols
 (i.e.\\ a transient \\cl\\ package is mapped to a persistent
 \\plob\\ p-package and a transient \\cl\\ symbol is mapped to a
 persistent \\plob\\ p-symbol; the mapping is in `both ways'). 
 The conflict resolution necessary for exporting
 symbols and using packages is done in the transient \\cl\\ system,
 so there is (currently) no need for \\plob\\ to do this.")

  (p-externals
   nil
   #+:lisp-doc :documentation #+:lisp-doc "
 A table (i.e.\\ BTree) with the external symbols of the package.
 This table is not used in the moment."))

;;; ---------------------------------------------------------------------------
;;; Mark slots of persistent-package as persistent without demand loading:
;;; ---------------------------------------------------------------------------
(setf (slot-extent 'p-package-name (find-class 'persistent-package)) :cached)
(setf (slot-extent 'p-internals (find-class 'persistent-package)) :cached)
(setf (slot-extent 'p-externals (find-class 'persistent-package)) :cached)

;;; ---------------------------------------------------------------------------
;;; Set the slot's locations of persistent-package:
;;; ---------------------------------------------------------------------------
(setf (slot-location 'p-package-name (find-class 'persistent-package))
  +package-location-name+)
(setf (slot-location 'p-internals (find-class 'persistent-package))
  +package-location-internals+)
(setf (slot-location 'p-externals (find-class 'persistent-package))
  +package-location-externals+)

;;; ---------------------------------------------------------------------------
;;; persistent-package is a PLOB base class
;;; deny schema evolution for it:
;;; ---------------------------------------------------------------------------
(setf (schema-evolution (find-class 'persistent-package)) :no-evolution)

;;; ---------------------------------------------------------------------------
(defvar *package-description-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the
 \\fcite{*package-description*}.
\\Seealsolabel
 \\Fcite{*package-description*}.")

;;; ---------------------------------------------------------------------------
(defvar *package-description* nil
  #+:lisp-doc "
\\Purposelabel
 Contains a persistent object of
 \\fcite{structure-description}\\ describing the
 \\fcite{persistent-package}.
\\Seealsolabel
 \\Fcite{*package-description-objid*}.")

;;; ---------------------------------------------------------------------------
;;; Predefined structure: persistent lisproot
;;; ---------------------------------------------------------------------------
(defstruct (persistent-lisproot
            (:include persistent-structure))
  #+:lisp-doc "
\\Purposelabel
 A transient structure representing persistent LISP root objects.
\\Remarkslabel
 \\basecls{persistent-lisproot}"

  (p-version nil
	     #+:lisp-doc :documentation #+:lisp-doc "
 The lisproot version represented by a fixnum.")

  (p-formatted nil
	       #+:lisp-doc :documentation #+:lisp-doc "
 The LISP system which formatted the LISP root
 represented by a keyword symbol.")

  (p-time nil
	  #+:lisp-doc :documentation #+:lisp-doc "
 The time and date the LISP root was formatted
 represented in Common LISP Universal Time, i.e. a bignum.")

  (p-name->package-table nil
			 #+:lisp-doc :documentation #+:lisp-doc "
 A table mapping names to packages.")

  (p-symbol->class-table nil
			 #+:lisp-doc :documentation #+:lisp-doc "
 A table mapping symbols to
 structure resp. class descriptions.")

  (p-structure-description nil
			   #+:lisp-doc :documentation #+:lisp-doc "
 The structure description object.")

  (p-structure-slot-description nil
				#+:lisp-doc :documentation #+:lisp-doc "
 The structure slot description object.")

  (p-package-description nil
			 #+:lisp-doc :documentation #+:lisp-doc "
 The package description object.")

  (p-plob-description nil
		      #+:lisp-doc :documentation #+:lisp-doc "
 The plob description object.")

  (p-class-description nil
		       #+:lisp-doc :documentation #+:lisp-doc "
 The class description object.")

  (p-slot-description nil
		      #+:lisp-doc :documentation #+:lisp-doc "
 The slot description object.")

  (p-direct-slot-description nil
			     #+:lisp-doc :documentation #+:lisp-doc "
 The direct slot description object.")

  (p-effective-slot-description nil
				#+:lisp-doc :documentation #+:lisp-doc "
 The effective slot description object.")

  (p-method-description nil
			#+:lisp-doc :documentation #+:lisp-doc "
 The method description object.")

  (p-pfs nil
	 #+:lisp-doc :documentation #+:lisp-doc "
 The object representing the Persistent File System."))

;;; ---------------------------------------------------------------------------
(defparameter *lisproot-version* 101
  #+:lisp-doc "Contains the actual version number of the LISP root object. Must be
 a fixnum.")

;;; ---------------------------------------------------------------------------
;;; Mark slots of persistent-lisproot as persistent without demand loading:
;;; ---------------------------------------------------------------------------
(setf (slot-extent 'p-version (find-class 'persistent-lisproot)) :cached)
(setf (slot-extent 'p-formatted (find-class 'persistent-lisproot)) :cached)
(setf (slot-extent 'p-time (find-class 'persistent-lisproot)) :cached)
(setf (slot-extent 'p-name->package-table (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-symbol->class-table (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-structure-description (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-structure-slot-description
                   (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-package-description (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-plob-description (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-class-description (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-slot-description (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-direct-slot-description
                   (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-effective-slot-description
                   (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-method-description
                   (find-class 'persistent-lisproot))
      :cached)
(setf (slot-extent 'p-pfs (find-class 'persistent-lisproot)) :cached)

;;; ---------------------------------------------------------------------------
;;; Set the slot's locations of persistent-lisproot:
;;; ---------------------------------------------------------------------------
(setf (slot-location 'p-version (find-class 'persistent-lisproot))
  +root-location-version+)
(setf (slot-location 'p-formatted (find-class 'persistent-lisproot))
  +root-location-lisp-formatted+)
(setf (slot-location 'p-time (find-class 'persistent-lisproot))
  +root-location-time-formatted+)
(setf (slot-location 'p-name->package-table(find-class 'persistent-lisproot))
  +root-location-name->package-table+)
(setf (slot-location 'p-symbol->class-table (find-class 'persistent-lisproot))
  +root-location-symbol->class-table+)
(setf (slot-location 'p-structure-description
		     (find-class 'persistent-lisproot))
  +root-location-structure-description+)
(setf (slot-location 'p-structure-slot-description
		     (find-class 'persistent-lisproot))
  +root-location-structure-slot-description+)
(setf (slot-location 'p-package-description
		     (find-class 'persistent-lisproot))
  +root-location-package-description+)
(setf (slot-location 'p-plob-description
		     (find-class 'persistent-lisproot))
  +root-location-plob-description+)
(setf (slot-location 'p-class-description
		     (find-class 'persistent-lisproot))
  +root-location-class-description+)
(setf (slot-location 'p-slot-description
		     (find-class 'persistent-lisproot))
  +root-location-slot-description+)
(setf (slot-location 'p-direct-slot-description
		     (find-class 'persistent-lisproot))
  +root-location-direct-slot-description+)
(setf (slot-location 'p-effective-slot-description
		     (find-class 'persistent-lisproot))
  +root-location-effective-slot-description+)
(setf (slot-location 'p-method-description
		     (find-class 'persistent-lisproot))
  +root-location-method-description+)
(setf (slot-location 'p-pfs (find-class 'persistent-lisproot))
  +root-location-pfs+)

;;; ---------------------------------------------------------------------------
;;; persistent-lisproot is a PLOB base class
;;; deny schema evolution for it:
;;; ---------------------------------------------------------------------------
(setf (schema-evolution (find-class 'persistent-lisproot)) :no-evolution)

;;; ---------------------------------------------------------------------------
(defvar *lisproot-description-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the
 \fcite{*lisproot-description*}.
\\Seealsolabel
 \\Fcite{*lisproot-description*}.")

;;; ---------------------------------------------------------------------------
(defvar *lisproot-description* nil
  #+:lisp-doc "
\\Purposelabel
 Contains a persistent object of
 \\fcite{structure-description}\\ describing the
 \\fcite{persistent-lisproot}.
\\Seealsolabel
 \\Fcite{*lisproot-description-objid*}.")

;;; ---------------------------------------------------------------------------
;;; Structure: slot-load-on-demand
;;; ---------------------------------------------------------------------------

(defstruct (slot-load-on-demand
            (:include persistent-immediate-object)
            (:constructor make-slot-load-on-demand
			  (objid &optional (type-tag +short-objid-tag+))))
  #+:lisp-doc "
\\Purposelabel
 A transient structure for slots to be loaded on demand.
 A transient structure slot to be loaded on demand is filled
 with an instance of this class and the transient structure slot
 accessor function is modified to `remember' these instances;
 when a structure accessor function is called, the
 {\\bf slot-load-on-demand}\\ instance loads the object from the
 stable heap.
\\Remarkslabel
 This structure is only for \\plob\\ internal use.
\\Seealsolabel
 \\Fcite{load-structure-slot}.")

;; Mark all slots of slot-load-on-demand as transient:
(setf (class-extent (find-class 'slot-load-on-demand)) :transient)

;;; ---------------------------------------------------------------------------
;;; Class: persistent-clos-object
;;; ---------------------------------------------------------------------------
(defclass persistent-clos-object ()

  ((objid
    :accessor persistent-object-objid
    ;; persistent-clos-object is the only class where the class option
    ;; :extent cannot be handled properly because of bootstrap reasons;
    ;; but it works if a :extent slot option is used:
    :extent :transient
    :initarg :objid
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 The (numeric) \\objid\\ of the persistent object."))

  (:metaclass persistent-metaclass)
  (:extent :transient)
  #+:lisp-doc (:documentation
   "
\\Purposelabel
 This is the class from which all classes with their class being
 \\fcite{persistent-metaclass}\\ (by using the class option
 \\lisp{(:metaclass persistent-metaclass)}\\ in the
 \\lisp{defclass}\\ statement) inherit.
\\Seealsolabel
 \\Fcite{persistent-metaclass}."))

;;; ---------------------------------------------------------------------------
(defmethod print-object ((the-object persistent-clos-object) to-stream)
  (let ((p-heap-objid (persistent-object-objid *default-persistent-heap*))
        (objid (persistent-object-objid the-object)))
    (unless p-heap-objid
      (setf p-heap-objid +null-objid+))
    (if (sh-objid-valid-p objid)
	(write-string
	 (sh-pprint-objid p-heap-objid objid +short-objid-tag+)
	 to-stream)
      (call-next-method))))

;;; ---------------------------------------------------------------------------
;;; Class description class: plob-description
;;; ---------------------------------------------------------------------------
(defclass plob-description (persistent-clos-object)
  ()
  (:extent :cached)
  (:metaclass persistent-metaclass)
  (:schema-evolution :no-evolution)
  #+:lisp-doc (:documentation
   "
\\Purposelabel
 An abstract superclass for class- and slot-descriptions defined
 for \\plob. This class roughly corresponds to the
 \\clos\\ \\mc\\ \\class{metaobject}.
\\Remarkslabel
 \\basecls{plob-description}"))

;;; ---------------------------------------------------------------------------
(defvar *plob-description-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the variable
 {\\bf *plob-description*}.
\\Seealsolabel
 \\Fcite{*plob-description*}.")

;;; ---------------------------------------------------------------------------
(defvar *plob-description* nil
  #+:lisp-doc "
\\Purposelabel
 Contains a persistent object of
 \\fcite{class-description}\\ describing the
 \\fcite{plob-description}.
\\Seealsolabel
 \\Fcite{*plob-description-objid*}.")

;;; ---------------------------------------------------------------------------
;;; Class description class: class-description
;;; ---------------------------------------------------------------------------
(defclass class-description (plob-description)

  ((p-name
    :initform nil
    :accessor class-description-name
    :reader class-name
    :type symbol
    :location +class-description-location-name+
    #+:lisp-doc :documentation #+:lisp-doc "
 The name of the class as a symbol; comparable with the returned
 value from
 \\fcite{class-name}.")

   (p-superclasses
    :initform nil
    :accessor class-description-superclasses
    :reader class-direct-superclasses
    :type vector
    :location +class-description-location-superclasses+
    #+:lisp-doc :documentation #+:lisp-doc "
 A vector of instances of \\class{class-description}\\ with the
 direct superclasses of the class described by this class
 description; comparable with the returned value from
 \\fcite{class-direct-superclasses}.")

   (p-precedence-list
    :initform nil
    :accessor class-description-precedence-list
    :reader class-precedence-list
    :type vector
    :location +class-description-location-precedence-list+
    #+:lisp-doc :documentation #+:lisp-doc "
 A vector of instances of \\class{class-description}\\ containing
 the class precedence list of the class
 described by this class description; comparable with the
 returned value from
 \\fcite{class-precedence-list}.")

   (p-metaclass
    :initform nil
    :accessor class-description-metaclass
    :location +class-description-location-metaclass+
    #+:lisp-doc :documentation #+:lisp-doc "
 The class as an instance of \\class{class-description}\\ of
 the class described by this class description;
 comparable with the returned value from
 \\lisp{(class-of \\textsl{\\lt{}described class\\gt}\\/)}.")

   (p-version-number
    :initform 100
    :type fixnum
    :accessor class-description-version-number
    :location +class-description-location-version+
    #+:lisp-doc :documentation #+:lisp-doc "
 A schema evolution version number; 100 means version 1.00.
 The number is incremented by 1 for each new version.")

   (p-time-stamp
    :initform (floor (get-universal-time) 60)
    :accessor class-description-time-stamp
    :location +class-description-location-time-stamp+
    #+:lisp-doc :documentation #+:lisp-doc "
 A time stamp when the class description was created. The time stamp
 is in \\cl\\ Universal Time divided by 60, i.e.\\ the time is in
 minutes, not in seconds.")
  
   (p-schema-evolution
    :accessor class-description-schema-evolution
    :initarg :schema-evolution
    :initform *default-clos-schema-evolution*
    :type symbol
    :location +class-description-location-schema-evolution+
    #+:lisp-doc :documentation #+:lisp-doc "
 A symbol describing the type of schema evolution to use for the
 structure;
 see \\fcite{schema-evolution}\\ and
 \\fcite{(setf schema-evolution)}.")

   (p-next-generation
    :accessor class-description-next-generation
    :location +class-description-location-next-generation+
    #+:lisp-doc :documentation #+:lisp-doc "
 The next generation of the structure description;
 this is either \\lispnil\\ or a pointer to an instance of class
 \\class{class-description}\\ with a higher version number.")

   (p-direct-methods
    :accessor class-description-direct-methods
    :reader class-direct-methods
    :initform nil
    :location +class-description-location-direct-methods+
    #+:lisp-doc :documentation #+:lisp-doc "
 A vector of instances of \\class{method-description}\\ containing
 the direct methods of the class
 described by this class description; comparable with the
 returned value from
 \\fcite{class-direct-methods}.
 \\note\\ Although defined, \\plob\\ stores no method
 descriptions at the moment; see comment at
 \\fcite{method-description}.")

   (p-persistent-slot-numbers
    :accessor class-description-persistent-slot-numbers
    :initform 0
    :location +class-description-location-persistent-slot-numbers+
    #+:lisp-doc :documentation #+:lisp-doc "
 The number of persistent slots of the described class.")

   (p-slot-numbers
    :accessor class-description-slot-numbers
    :initform 0
    :location +class-description-location-slot-numbers+
   #+:lisp-doc :documentation #+:lisp-doc "
 The total number of slots of the described structure, i.e.\\ the sum
 of the number of persistent slots and the number of transient slots.")

   (p-direct-slots
    :accessor class-description-direct-slots
    :reader class-direct-slots
    :initform nil
    :location +class-description-location-direct-slots+
    #+:lisp-doc :documentation #+:lisp-doc "
 A vector with instances of
 \\fcite{direct-slot-description}\\ for each direct slot of the
 described class; comparable with the
 returned value from
 \\fcite{class-direct-slots}.")

   (p-effective-slots
    :accessor class-description-effective-slots
    :reader class-slots
    :initform nil
    :location +class-description-location-effective-slots+
    #+:lisp-doc :documentation #+:lisp-doc "
 A vector with instances of
 \\fcite{effective-slot-description}\\ for each effective slot of the
 described class; comparable with the
 returned value from
 \\fcite{class-slots}.")

   (p-constructor
    :accessor class-description-constructor
    :reader class-constructor
    :initform nil
    :location +class-description-location-constructor+
    #+:lisp-doc :documentation #+:lisp-doc "
 The name of the instance constructor function as a symbol;
 see \\fcite{class-constructor}\\ and
 \\fcite{(setf class-constructor)}.")
   
   (p-dependent
    :accessor class-description-dependent
    :reader class-dependent
    :initform nil
    :location +class-description-location-dependent+
    #+:lisp-doc :documentation #+:lisp-doc "
 The instance dependent flag; see \\fcite{class-dependent}\\ and
 \\fcite{(setf class-dependent)}.")
   
   (p-plist
    :accessor class-description-plist
    :reader class-plist
    :initform nil
    :location +class-description-location-plist+
    #+:lisp-doc :documentation #+:lisp-doc "
 The class property list of the described class. For transient
 \\clsmo{}s in \\lw, it is used to store class-specific
 informations of variable length.")

   (p-prototype
    :accessor class-description-prototype
    :initform nil
    :location +class-description-location-prototype+
    #+:lisp-doc :documentation #+:lisp-doc "
 A persistent prototype object of the class
 described by this class description; comparable with the
 returned value from
 \\fcite{class-prototype}.")
   
   (t-name->slot-cache
    :accessor class-description-name->slot-cache
    :initform (make-hash-table :test #'eq)
    :extent :transient
    #+:lisp-doc :documentation #+:lisp-doc "
 A transient slot with a hash table mapping slot names to
 instances of \\fcite{effective-slot-description}.
 It is used for fast
 mapping of a slot name to its corresponding instance of
 \\fcite{effective-slot-description}\\ in various methods
 of \\fcite{slot-value-using-class}\\ and
 \\fcite{(setf slot-value-using-class)}.")

   (t-class
    :accessor class-description-class
    :extent :transient
    :initform nil
    #+:lisp-doc :documentation #+:lisp-doc "
 A transient slot with the \\clsmo\\ of the class
 described by this class description."))

  (:extent :cached)
  (:metaclass persistent-metaclass)
  (:schema-evolution :no-evolution)
  #+:lisp-doc (:documentation "

\\Purposelabel
 A class for describing classes in the stable heap.
 It holds only enough information to check if the persistent and transient
 class definitions are equal.
 In the sense of \\clos, this class corresponds roughly to the
 \\std\\ \\clsmc\\ \\class{standard-class}.
\\Remarkslabel
 \\basecls{class-description}

 Close the persistent heap by calling
 \\fcite{close-heap}\\ before evaluating this class definition;
 otherwise an infinite recursion will occure because of the
 self-descriptive property of class \\class{class-description}."))

;;; ---------------------------------------------------------------------------
(defvar *class-description-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the variable
 {\\bf *class-description*}.
\\Seealsolabel
 \\Fcite{*class-description*}.")

;;; ---------------------------------------------------------------------------
(defvar *class-description* nil
  #+:lisp-doc "
\\Purposelabel
 Contains a persistent object of
 \\fcite{class-description}\\ describing the
 \\fcite{class-description}.
\\Seealsolabel
 \\Fcite{*class-description-objid*}.")

;;; ---------------------------------------------------------------------------
;;; Class description class: slot-description
;;; ---------------------------------------------------------------------------
(defclass slot-description (plob-description)

  ((p-name
    :accessor slot-description-name
    :reader slot-definition-name
    :initform nil
    :type symbol
    :location +slot-description-location-name+
    #+:lisp-doc :documentation #+:lisp-doc "
 The name of the slot as a symbol; comparable with the returned
 value from
 \\fcite{slot-definition-name}.")

   (p-initargs
    :accessor slot-description-initargs
    :reader slot-definition-initargs
    :initform nil
    :location +slot-description-location-init-args+
    #+:lisp-doc :documentation #+:lisp-doc "
 The initialization arguments of the slot as a list of keywords;
 comparable with the returned value from
 \\fcite{slot-definition-initargs}.")

   (p-initform
    :accessor slot-description-initform
    :reader slot-definition-initform
    :initform nil
    :location +slot-description-location-init-form+
    #+:lisp-doc :documentation #+:lisp-doc "
 The initialization form of the slot;
 comparable with the returned value from
 \\fcite{slot-definition-initform}.")

   (p-initfunction
    :accessor slot-description-initfunction
    :reader slot-definition-initfunction
    :initform nil
    :location +slot-description-location-init-function+
    #+:lisp-doc :documentation #+:lisp-doc "
 The initialization function of the slot;
 comparable with the returned value from
 \\fcite{slot-definition-initfunction}.")

   (p-type
    :accessor slot-description-type
    :reader slot-definition-type
    :initform t
    :location +slot-description-location-type+
    #+:lisp-doc :documentation #+:lisp-doc "
 The type of the slot;
 comparable with the returned value from
 \\fcite{slot-definition-type}.")

   (p-allocation
    :accessor slot-description-allocation
    :reader slot-definition-allocation
    :initform :instance
    :location +slot-description-location-allocation+
    #+:lisp-doc :documentation #+:lisp-doc "
 The allocation of the slot;
 comparable with the returned value from
 \\fcite{slot-definition-allocation}.")

   (p-extent
    :accessor slot-description-extent
    :initform nil
    :location +slot-description-location-extent+
    #+:lisp-doc :documentation #+:lisp-doc "
 The extent of the slot;
 see \\fcite{slot-extent}\\ and, for possible values,
 \\fcite{(setf slot-extent)}.")

   (p-deferred
    :accessor slot-description-deferred
    :initform nil
    :location +slot-description-location-deferred+
    #+:lisp-doc :documentation #+:lisp-doc "
 The deferred option of the slot;
 see \\fcite{slot-deferred}\\ and, for possible values,
 \\fcite{(setf slot-deferred)}.")

   (p-index
    :accessor slot-description-index
    :initarg :index
    :initform nil
    :location +slot-description-location-index+
    #+:lisp-doc :documentation #+:lisp-doc "
 The index to maintain for the slot; this slot is only bound for
 class descriptions of classes whose class is
 \\fcite{persistent-metaclass} (see the remarks concerning the
 additional slot option
 \\lisp{:index}\\ for instances of that \\clsmc\\ for details)."))

  (:dependent :read)
  (:extent :cached)
  (:metaclass persistent-metaclass)
  (:schema-evolution :no-evolution)
  #+:lisp-doc (:documentation
   "
\\Purposelabel
 A transient class representing persistent slot descriptions.
 In the sense of \\clos, this class corresponds roughly to the
 \\std\\ \\sltmc\\ \\class{slot-definition}.
\\Remarkslabel
 \\basecls{slot-description}"))

;;; ---------------------------------------------------------------------------
(defvar *slot-description-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the variable
 {\\bf *slot-description*}.
\\Seealsolabel
 \\Fcite{*slot-description*}.")

;;; ---------------------------------------------------------------------------
(defvar *slot-description* nil
  #+:lisp-doc "
\\Purposelabel
 Contains a persistent object of
 \\fcite{class-description}\\ describing the
 \\fcite{slot-description}.
\\Seealsolabel
 \\Fcite{*slot-description-objid*}.")

;;; ---------------------------------------------------------------------------
(defclass direct-slot-description (slot-description)

  ((p-readers
    :accessor slot-description-readers
    :reader slot-definition-readers
    :initform nil
    :location +slot-description-location-readers+
    #+:lisp-doc :documentation #+:lisp-doc "
 A vector of slot reader function symbols;
 comparable with the returned value from
 \\fcite{slot-definition-readers}.")

   (p-writers
    :accessor slot-description-writers
    :reader slot-definition-writers
    :initform nil
    :location +slot-description-location-writers+
    #+:lisp-doc :documentation #+:lisp-doc "
 A vector of slot writer function symbols;
 comparable with the returned value from
 \\fcite{slot-definition-writers}."))

  (:dependent :read)
  (:extent :cached)
  (:metaclass persistent-metaclass)
  (:schema-evolution :no-evolution)
  #+:lisp-doc (:documentation
   "
\\Purposelabel
 A transient class representing persistent direct slot descriptions.
 In the sense of \\clos, this class corresponds roughly to the
 \\std\\ \\sltmc\\ \\class{standard-direct-slot-definition}.
\\Remarkslabel
 \\basecls{direct-slot-description}"))

;;; ---------------------------------------------------------------------------
(setf (class-dependent (find-class 'direct-slot-description)) :read)

;;; ---------------------------------------------------------------------------
(defvar *direct-slot-description-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the variable
 {\\bf *direct-slot-description*}.
\\Seealsolabel
 \\Fcite{*direct-slot-description*}.")

;;; ---------------------------------------------------------------------------
(defvar *direct-slot-description* nil
  #+:lisp-doc "
\\Purposelabel
 Contains a persistent object of
 \\fcite{class-description}\\ describing the
 \\fcite{direct-slot-description}.
\\Seealsolabel
 \\Fcite{*direct-slot-description-objid*}.")

;;; ---------------------------------------------------------------------------
(defclass effective-slot-description (slot-description)

  ((p-location
    :accessor slot-description-location
    :reader slot-definition-location
    :initform nil
    :location +slot-description-location-location+
    #+:lisp-doc :documentation #+:lisp-doc "
 The location of the slot; comparable with the returned value from
 \\fcite{slot-definition-location}.

 For persistent slots without an \\lisp{:allocation :class}, the
 {\\bf location} is the postion of the slot in the persistent
 object.

 For persistent slots with an \\lisp{:allocation :class}, the
 {\\bf location} contains a persistent cons cell whose car
 is a symbol naming the slot (only for documention purpose) and
 whose cdr holds the slot value. This cons cell is inherited
 to subclasses; the additional indirection assures that by accessing
 the cdr of the cell all classes share the same (i.e.\\ \\lisp{eq})
 value."))

  (:dependent :read)
  (:extent :cached)
  (:metaclass persistent-metaclass)
  (:schema-evolution :no-evolution)
  #+:lisp-doc (:documentation
   "
\\Purposelabel
 A transient class representing persistent effective slot descriptions.
 In the sense of \\clos, this class corresponds roughly to the
 \\std\\ \\sltmc\\ \\class{standard-effective-slot-definition}.
\\Remarkslabel
 \\basecls{effective-slot-description}"))

;;; ---------------------------------------------------------------------------
(setf (class-dependent (find-class 'effective-slot-description)) :read)

;;; ---------------------------------------------------------------------------
(defvar *effective-slot-description-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the variable
 {\\bf *effective-slot-description*}.
\\Seealsolabel
 \\Fcite{*effective-slot-description*}.")

;;; ---------------------------------------------------------------------------
(defvar *effective-slot-description* nil
  #+:lisp-doc "
\\Purposelabel
 Contains a persistent object of
 \\fcite{class-description}\\ describing the
 \\fcite{effective-slot-description}.
\\Seealsolabel
 \\Fcite{*effective-slot-description-objid*}.")

;;; ---------------------------------------------------------------------------
;;; Class description class: method-description
;;; ---------------------------------------------------------------------------
(defclass method-description (plob-description)

  ((p-name
    :accessor method-description-name
    :initform nil
    :type symbol
    :location +method-description-location-name+
    #+:lisp-doc :documentation #+:lisp-doc "
 The name of the method's generic function as a symbol.")

   (p-function
    :accessor method-description-function
    :reader method-function
    :initform nil
    :location +method-description-location-function+
    #+:lisp-doc :documentation #+:lisp-doc "
 The function of the method;
 comparable with the returned value from
 \\fcite{method-function}.
 For the restrictions on storing functions in \\plob,
 see \\fcite{p-function}\\ and \\fcite{(setf p-function)}.")

   (p-lambda-list
    :accessor method-description-lambda-list
    :reader method-lambda-list
    :initform nil
    :location +method-description-location-lambda-list+
    #+:lisp-doc :documentation #+:lisp-doc "
 The unspecialized $\\lambda$-list of the method;
 comparable with the returned value from
 \\fcite{method-lambda-list}.")

   (p-specializers
    :accessor method-description-specializers
    :reader method-specializers
    :initform nil
    :location +method-description-location-specializers+
    #+:lisp-doc :documentation #+:lisp-doc "
 The list of specializers of the method;
 comparable with the returned value from
 \\fcite{method-specializers}.")

   (p-qualifiers
    :accessor method-description-qualifiers
    :reader method-qualifiers
    :initform nil
    :location +method-description-location-qualifiers+
    #+:lisp-doc :documentation #+:lisp-doc "
 The list of qualifiers of the method;
 comparable with the returned value from
 \\fcite{method-qualifiers}."))

  (:extent :cached)
  (:metaclass persistent-metaclass)
  (:schema-evolution :no-evolution)
  #+:lisp-doc (:documentation
   "
\\Purposelabel
 A transient class representing persistent method descriptions.
 In the sense of \\clos, this class corresponds roughly to the
 \\std\\ \\mtdmc\\ \\class{standard-method}.
\\Remarkslabel
 \\basecls{method-description}

 Since \\plob\\ cannot store function code (see comment of
 \\fcite{(setf p-function)}), instances of
 \\class{method-description}\\ have more or less documentary
 properties; therefore, no
 method-descriptions are generated or stored in a class-description.
 To store method-descriptions along with class-descriptions,
 re-compile \\plob\\ with the keyword
 \\lisp{:store-methods}\\ pushed into the {\\bf *features*}-list."))

;;; ---------------------------------------------------------------------------
(defvar *method-description-objid* nil
  #+:lisp-doc "
\\Purposelabel
 Contains the \\objid\\ of the value of the variable
 {\\bf *method-description*}.
\\Seealsolabel
 \\Fcite{*method-description*}.")

;;; ---------------------------------------------------------------------------
(defvar *method-description* nil
  #+:lisp-doc "
\\Purposelabel
 Contains a persistent object of
 \\fcite{class-description}\\ describing the
 \\fcite{method-description}.
\\Seealsolabel
 \\Fcite{*method-description-objid*}.")

;;;; Local variables:
;;;; buffer-file-coding-system: raw-text-unix
;;;; End:
