;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-database.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	11.4.94
;;;; Description	PLOB simple database functions
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
#+(and :lisp-doc :document-api)
(:defdoc
 "index ..."
 "Index Administration"
 "
\\Purposelabel
 An index table maps a slot value to a persistent
 \\clos\\ instance which has that value in that slot. An index
 can be defined on a per-slot basis; the object used for
 maintaining the index is hold in the slot
 {\\bf p-index} of the slot's effective-slot-description
 object.

 The index table management has interfaces to two sides:
 \\begin{itemize}

 \\item One interface is used for administrating the
  index-representing data. It consists of the generic functions
  {\\bf clrindex}, {\\bf getindex}, {\\bf (setf getindex)},
  {\\bf getindex-by-objid}, {\\bf (setf getindex-by-objid)},
  {\\bf remindex} and  {\\bf mapindex}. With these generic functions
  the index table can be cleared, elements can be read,
  written or deleted and a mapping over the elements of an index
  table can be accomplished without having to know which
  class is used for representing the index table.

 \\item The other interface consists of the functions
  {\\bf getindex-by-tag} and {\\bf remindex-by-tag}. These
  are called to insert a mapping from a slot value to its
  persistent \\clos\\ instance with that slot value
  resp.\\ to remove the mapping; they are called from the
  \\fcite{(setf slot-value-using-class) (t standard-class t %
effective-slot-description)}\\ to insert resp.\\ overwrite
  index table elements and from
  \\fcite{slot-makunbound-using-class (standard-class t %
effective-slot-description)}\\ to remove elements from the
  index table.

 \\end{itemize}

\\Seealsolabel
 \\Fcite{clrindex};
 \\fcite{getindex};
 \\fcite{(setf getindex)};
 \\fcite{getindex-by-objid};
 \\fcite{(setf getindex-by-objid)};
 \\fcite{remindex};
 \\fcite{mapindex};
 \\fcite{getindex-by-tag};
 \\fcite{remindex-by-tag};
 \\fcite{(setf slot-value-using-class) (t standard-class t %
effective-slot-description)};
 \\fcite{slot-makunbound-using-class (standard-class t %
effective-slot-description)};
 \\fcite{p-select};
 slot {\\bf p-index} of \\fcite{slot-description}.")


;;; ---------------------------------------------------------------------------
;;; Generic functions
;;; ---------------------------------------------------------------------------

(defgeneric clrindex (the-table &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isaidxtable{\\funarg{the-table}}
\\Purposelabel
 \\idxadmfn:
 Delete all keys (i.e.\\ slot values) contained in \\funarg{the-table}.
\\Seealsolabel
 Section \\fcite{index ...}."))

;;; ---------------------------------------------------------------------------
(defgeneric getindex (key-slot-value
                      the-table
		      &optional (depth *default-depth*)
		      (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "

\\Argumentslabel
 \\isanobject{\\funarg{key-slot-value}}
 \\isaidxtable{\\funarg{the-table}}
\\Purposelabel
 \\idxadmfn:
 Search \\funarg{key-slot-value}\\ in \\funarg{the-table}\\ and
 return a found persistent \\clos\\ instance;
 if the search was not successfull, \\lispnil\\ is returned.
\\Seealsolabel
 \\Fcite{(setf getindex)};
 \\fcite{getindex-by-objid};
 section \\fcite{index ...}."))

;;; ---------------------------------------------------------------------------
(defgeneric getindex-by-objid (key-slot-value
                               the-table
			       &optional (depth *default-depth*)
			       (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isanobjid{\\funarg{key-slot-value}}
 \\isaidxtable{\\funarg{the-table}}
\\Purposelabel
 \\idxadmfn:
 Search \\funarg{key-slot-value}\\ in \\funarg{the-table}\\ and
 return a found persistent \\clos\\ instance;
 if the search was not successfull, \\lispnil\\ is returned.
\\Seealsolabel
 \\Fcite{getindex};
 section \\fcite{index ...}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf getindex) (data-persistent-instance
                             key-slot-value the-table
			     &optional (depth *default-depth*)
			     (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isa{\\funarg{data-persistent-instance}}
      {a persistent \\clos\\ instance}
 \\isanobject{\\funarg{key-slot-value}}
 \\isaidxtable{\\funarg{the-table}}
\\Purposelabel
 \\idxadmfn:
 Insert \\funarg{data-persistent-instance}\\ associated to
 \\funarg{key-slot-value}\\ into \\funarg{the-table}.
\\Seealsolabel
 \\Fcite{getindex};
 \\fcite{(setf getindex-by-objid)};
 section \\fcite{index ...}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf getindex-by-objid) (data-persistent-instance
                                      key-slot-value the-table
                                      &optional (depth *default-depth*)
                                      (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isa{\\funarg{data-persistent-instance}}
      {a persistent \\clos\\ instance}
 \\isanobjid{\\funarg{key-slot-value}}
 \\isaidxtable{\\funarg{the-table}}
\\Purposelabel
 \\idxadmfn:
 Insert \\funarg{data-persistent-instance}\\ associated to
 \\funarg{key-slot-value}\\ into \\funarg{the-table}.
\\Seealsolabel
 \\Fcite{(setf getindex)};
 section \\fcite{index ...}."))

;;; ---------------------------------------------------------------------------
(defgeneric remindex (key-slot-value
                      the-table
		      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isanobject{\\funarg{key-slot-value}}
 \\isaidxtable{\\funarg{the-table}}
\\Purposelabel
 \\idxadmfn:
 Remove \\funarg{key-slot-value}\\ from \\funarg{the-table}.
\\Seealsolabel
 \\Fcite{remindex-by-objid};
 section \\fcite{index ...}."))

;;; ---------------------------------------------------------------------------
(defgeneric remindex-by-objid
     (key-slot-value the-table &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isanobjid{\\funarg{key-slot-value}}
 \\isaidxtable{\\funarg{the-table}}
\\Purposelabel
 \\idxadmfn:
 Remove objid \funarg{key-slot-value}\\ from \\funarg{the-table}.
\\Seealsolabel
 \\Fcite{remindex};
 section \\fcite{index ...}."))

;;; ---------------------------------------------------------------------------
(defgeneric mapindex (map-function the-table &rest rest-args)

  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isa{\\funarg{map-function}}
      {a function which accepts two arguments}
 \\isaidxtable{\\funarg{the-table}}
 \\isa{\\funarg{rest-args}}
      {a list of arguments which will be passed to the called
       mapping function}
\\Purposelabel
 \\idxadmfn:
 For each entry in \\funarg{the-table}\\ matching the
 \\funarg{rest-args}, the \\funarg{map-function}\\ on
 two arguments should be called: the key (i.e.\\ a slot value) and
 associated data (i.e.\\ a persistent \\clos\\ object) of the index entry.

 The called \\funarg{map-function}\\ has to return with \\nonnil\\ to
 continue the mapping; if \\funarg{map-function}\\ returns
 \\lispnil\\, the mapping is stopped.

\\Seealsolabel
 Section \\fcite{index ...}."))

;;; ---------------------------------------------------------------------------
(defgeneric get-index-table (class name)

  #+:lisp-doc (:documentation "

\\Argumentslabel
 \\isa{\\funarg{class}}
      {either a \\clsmo\\ with persistent instances or a symbol
       naming such a class}
 \\isa{\\funarg{name}}
      {a symbol naming a slot of \\funarg{class}\\ which has an
       index defined on it, i.e.\\ whose slot definition in the
       \\lisp{defclass}\\ statement had an \\lisp{:index}\\ slot
       option}
\\Purposelabel
  Return the object representing the index table of the slot named
  \\funarg{name}\\ in \\funarg{class}. If no index has been declared
  on the slot, \\lispnil\\ is returned.
\\Seealsolabel
 Section \\fcite{index ...}."))

;;; ---------------------------------------------------------------------------
;;; Auxiliary functions
;;; ---------------------------------------------------------------------------

(defconstant +index-kind->constructor+
  `((hash-table . ,#'make-hash-table)
    (btree . ,#'make-btree))
  #+:lisp-doc "
\\Purposelabel
 Maps the \\lisp{car}\\ of the \\lisp{:index}\\ slot option's value
 to a function which creates the object which holds the index.
\\Seealsolabel
 \\Fcite{make-index}")

;;; ---------------------------------------------------------------------------
(defun make-index (index-kind-and-create-options)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{index-kind-and-create-options}}
      {the value of the \\lisp{:index}\\ slot option as specified
       in the slot definition in the \\lisp{defclass}\\ statement}
\\Valueslabel
 Returns the object which maintains the index, for now this
 is always a BTree.
\\Purposelabel
 Create an index-maintaining object from the value of the
 \\lisp{:index}\\ slot option as specified in the slot definition
 in the \\lisp{defclass}\\ statement; see comment of
 \\fcite{persistent-metaclass}, additional slot option
 \\lisp{:index}, for details.
\\Remarkslabel
 For now persistent BTree objects are supported as index-maintaining
 objects.
\\Seealsolabel
 \\Fcite{persistent-metaclass};
 section \\fcite{index ...}."

  (let* ((index-kind-and-create-options-cons-p
          (consp index-kind-and-create-options))
         (index-kind
          (if index-kind-and-create-options-cons-p
              (car index-kind-and-create-options)
            index-kind-and-create-options))
         (create-options
	  (when index-kind-and-create-options-cons-p
	    (cdr index-kind-and-create-options)))
         (constructor (cdr (assoc index-kind +index-kind->constructor+))))
    (unless constructor
      (error "Unknown index kind ~S requested." index-kind))
    (apply constructor create-options)))

;;; ---------------------------------------------------------------------------
(defun getindex-by-tag (key-slot-value-objid
                        key-slot-value-type-tag
			the-table
			&optional (depth *default-depth*)
			(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{key-slot-value-objid}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{key-slot-value-type-tag}}
 \\isaidxtable{\\funarg{the-table}}
\\Valueslabel
 See \\fcite{getindex}\\ resp.\\ \\fcite{getindex-by-objid}.
\\Purposelabel
 The action done by this function depends on the value of the 
 \\funarg{key-slot-value-type-tag}\\ argument, i.e.\\ if it
 references an immediate or a non-immediate \\plob\\ type:
 \\begin{description}

 \\item [immediate type]
  In this case \\funarg{key-slot-value-objid}\\ is not an
  \\objid\\ but an immediate value; {\\bf getindex} is called
  with that value as key argument.

 \\item [non-immediate type]
  In this case \\funarg{key-slot-value-objid}\\ is an
  \\objid; {\\bf getindex-by-objid} is called with that
  \\objid\\ as key argument.

 \\end{description}
\\Seealsolabel
 \\Fcite{getindex};
 \\fcite{getindex-by-objid};
 section \\fcite{index ...}."

  (unless key-slot-value-type-tag
    (setf key-slot-value-type-tag +short-objid-tag+))
  (cond
   ((p-markerp key-slot-value-type-tag)
    (values nil nil))
   ((p-immediatep key-slot-value-type-tag)
    (getindex key-slot-value-objid the-table depth p-heap))
   (t
    (getindex-by-objid key-slot-value-objid the-table depth p-heap))))
    
;;; ---------------------------------------------------------------------------
(defun (setf getindex-by-tag) (data-persistent-instance
                               key-slot-value-objid
                               key-slot-value-type-tag
			       the-table
			       &optional (depth *default-depth*)
			       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{data-persistent-instance}}
      {a persistent \\clos\\ instance}
 \\isa{\\funarg{key-slot-value-objid}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{key-slot-value-type-tag}}
 \\isaidxtable{\\funarg{the-table}}
\\Valueslabel
 See \\fcite{(setf getindex)}\\ resp.\\ %
 \\fcite{(setf getindex-by-objid)}.
\\Purposelabel
 The action done by this function depends on the value of the 
 \\funarg{key-slot-value-type-tag}\\ argument, i.e.\\ if it
 references an immediate or a non-immediate \\plob\\ type:
 \\begin{description}

 \\item [immediate type]
  In this case \\funarg{key-slot-value-objid}\\ is not an
  \\objid\\ but an immediate value; {\\bf (setf getindex)} is called
  with that value as key argument.

 \\item [non-immediate type]
  In this case \\funarg{key-slot-value-objid}\\ is an
  \\objid; {\\bf (setf getindex-by-objid)} is called with that
  \\objid\\ as key argument.

 \\end{description}
\\Seealsolabel
 \\Fcite{(setf getindex)};
 \\fcite{(setf getindex-by-objid)};
 section \\fcite{index ...}."

  (unless key-slot-value-type-tag
    (setf key-slot-value-type-tag +short-objid-tag+))
  (cond
   ((p-markerp key-slot-value-type-tag)
    (values nil nil))
   ((p-immediatep key-slot-value-type-tag)
    (setf (getindex key-slot-value-objid the-table depth p-heap)
          data-persistent-instance))
   (t
    (setf (getindex-by-objid key-slot-value-objid the-table depth p-heap)
          data-persistent-instance))))
    
;;; ---------------------------------------------------------------------------
(defun remindex-by-tag (key-slot-value-objid
                        key-slot-value-type-tag
			the-table
			&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{key-slot-value-objid}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{key-slot-value-type-tag}}
 \\isaidxtable{\\funarg{the-table}}
\\Valueslabel
 See \\fcite{remindex}\\ resp.\\ \\fcite{remindex-by-objid}.
\\Purposelabel
 The action done by this function depends on the value of the 
 \\funarg{key-slot-value-type-tag}\\ argument, i.e.\\ if it
 references an immediate or a non-immediate \\plob\\ type:
 \\begin{description}

 \\item [immediate type]
  In this case \\funarg{key-slot-value-objid}\\ is not an
  \\objid\\ but an immediate value; {\\bf remindex} is called
  with that value as key argument.

 \\item [non-immediate type]
  In this case \\funarg{key-slot-value-objid}\\ is an
  \\objid; {\\bf remindex-by-objid} is called with that
  \\objid\\ as key argument.

 \\end{description}
\\Seealsolabel
 \\Fcite{remindex};
 \\fcite{remindex-by-objid};
 section \\fcite{index ...}."

  (unless key-slot-value-type-tag
    (setf key-slot-value-type-tag +short-objid-tag+))
  (cond
   ((or (null key-slot-value-objid) (p-markerp key-slot-value-type-tag))
    nil)
   ((p-immediatep key-slot-value-type-tag)
    (remindex key-slot-value-objid the-table p-heap))
   (t
    (remindex-by-objid key-slot-value-objid the-table p-heap))))

;;; ---------------------------------------------------------------------------
;;; Methods for BTrees
;;; ---------------------------------------------------------------------------

(defmethod clrindex ((the-table persistent-btree)
                     &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Trapped to \\fcite{clrbtree}."
  (clrbtree the-table p-heap))

;;; ---------------------------------------------------------------------------
(defmethod getindex (key-slot-value
                     (the-table persistent-btree)
		     &optional (depth *default-depth*)
		     (p-heap *default-persistent-heap*))
  #+:lisp-doc "Trapped to \\fcite{getbtree}."
  (getbtree key-slot-value the-table depth p-heap))

;;; ---------------------------------------------------------------------------
(defmethod getindex-by-objid (key-slot-value
                              (the-table persistent-btree)
			      &optional (depth *default-depth*)
			      (p-heap *default-persistent-heap*))
  #+:lisp-doc "Trapped to \\fcite{getbtree-by-objid}."
  (getbtree-by-objid key-slot-value the-table depth p-heap))

;;; ---------------------------------------------------------------------------
(defmethod (setf getindex) (data-persistent-instance
                            key-slot-value
                            (the-table persistent-btree)
			    &optional (depth *default-depth*)
			    (p-heap *default-persistent-heap*))
  #+:lisp-doc "Trapped to \\fcite{(setf getbtree)}."
  (setf (getbtree key-slot-value the-table depth p-heap)
	data-persistent-instance))

;;; ---------------------------------------------------------------------------
(defmethod (setf getindex-by-objid)
     (data-persistent-instance
      key-slot-value (the-table persistent-btree)
      &optional (depth *default-depth*)
      (p-heap *default-persistent-heap*))
  #+:lisp-doc "Trapped to \\fcite{(setf getbtree-by-objid)}."
  (setf (getbtree-by-objid key-slot-value the-table depth p-heap)
        data-persistent-instance))

;;; ---------------------------------------------------------------------------
(defmethod remindex (key-slot-value
                     (the-table persistent-btree)
		     &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Trapped to \\fcite{rembtree}."
  (rembtree key-slot-value the-table p-heap))

;;; ---------------------------------------------------------------------------
(defmethod remindex-by-objid (key-slot-value
                              (the-table persistent-btree)
	                      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Trapped to \\fcite{rembtree-by-objid}."
  (rembtree-by-objid key-slot-value +short-objid-tag+ the-table p-heap))

;;; ---------------------------------------------------------------------------
(defmethod mapindex (map-function
		     (the-table persistent-btree)
		     &rest rest-args)

  #+:lisp-doc "Trapped to \\fcite{mapbtree}."
  
  (apply #'mapbtree (nconc (list map-function the-table) rest-args)))

;;; ---------------------------------------------------------------------------
(defmethod get-index-table ((class persistent-clos-object) name)
  (get-index-table (class-name class) name))

;;; ---------------------------------------------------------------------------
(defmethod get-index-table ((class symbol) (name symbol))
  (let* ((class-descr (ensure-class-description class))
	 (slot (when class-descr (find-effective-slot name class-descr)))
	 (index (when slot (slot-description-index slot))))
    index))
    
;;; ---------------------------------------------------------------------------
;;; Simple database functions
;;; ---------------------------------------------------------------------------

(defun p-select (the-class &key ((:where slot-name))
				((:< less) nil lessp)
				((:<= less-equal) nil less-equal-p)
				((:> greater) nil greaterp)
				((:>= greater-equal) nil greater-equal-p)
				((:= equals-to) nil equals-to-p)
				(depth *default-depth*)
				(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-class}}
      {either a \\clsmo\\ with persistent instances or a symbol
       naming such a class}
 \\isa{\\keyarg{where}}
      {a symbol naming a slot of \\funarg{the-class}\\ which has an
       index defined on it, i.e.\\ whose slot definition in the
       \\lisp{defclass}\\ statement had an \\lisp{:index}\\ slot
       option}
 \\isanobject{\\keyarg{<}\\ resp.\\ \\keyarg{<=}\\ resp.\\ %
              \\keyarg{>}\\ resp.\\ \\keyarg{>=}\\ resp.\\ %
              \\keyarg{=}}
\\Valueslabel
 Either \\lispnil\\ or
 one matching persistent \\clos\\ instance or
 a list with matching persistent \\clos\\ instances.
\\Purposelabel
 Selects persistent \\clos\\ instances of \\funarg{the-class}\\ and
 its subclasses where the value of the slot named
 \\keyarg{where}\\ mets one of the following criterions:
 \\begin{description}

 \\item [If the \\keyarg{=}\\ argument is given,]
  a persistent \\clos\\ instance with that slot's value matching the
  value of the \\keyarg{=}\\ argument is searched; if such an instance
  was found, it is returned, \\lispnil\\ otherwise.

 \\item [If the \\keyarg{>=}\\ argument is given,]
  all persistent \\clos\\ instances with that slot's value being equal
  or greater than the value of the \\keyarg{>=}\\ argument are returned,
  \\lispnil\\ if no persistent \\clos\\ instance matched.

 \\item [If the \\keyarg{>}\\ argument is given,]
  all persistent \\clos\\ instances with that slot's value being greater
  than the value of the \\keyarg{>}\\ argument are returned,
  \\lispnil\\ if no persistent \\clos\\ instance matched.

 \\item [If the \\keyarg{<}\\ argument is given,]
  all persistent \\clos\\ instances with that slot's value being
  less than the value of the \\keyarg{<}\\ argument are returned,
  \\lispnil\\ if no persistent \\clos\\ instance matched.

 \\item [If the \\keyarg{<=}\\ argument is given,]
  all persistent \\clos\\ instances with that slot's value being
  less than or equal to the value of the \\keyarg{<}\\ argument
  are returned, \\lispnil\\ if no persistent \\clos\\ instance matched.

 \\item [If none of these arguments is given,]
  all persistent \\clos\\ instances of \\funarg{the-class}\\ and
  its subclasses are returned.

 \\end{description}
 For the index being a BTree (the one and only possibility for now),
 the kind of matching performed depends on the value of the
 \\keyarg{test}\\ argument which was passed in the
 \\lisp{:index}\\ slot option's value in the slot definition given
 in the \\lisp{defclass}\\ statement; see comment of
 \\fcite{persistent-metaclass}, additional slot option
 \\lisp{:index}, for details.
\\Exampleslabel
 The class \\class{person}\\ has an index defined on slot {\\bf soc-sec-\\#}:
\\begin{lispcode}
 (defclass person ()
  ((first-name :initarg :first-name)
   (last-name  :initarg :last-name)
   (soc-sec-\\#  :initarg :soc-sec-\\#
               {\\bf:index (btree :test equal)}))
  (:metaclass persistent-metaclass))

 (defmethod print-object ...)    ; exact definition left as an exercise ;-)

 (make-instance 'person :first-name \"John\" :last-name \"Cleese\" :soc-sec-\\# 123)
 (make-instance 'person :first-name \"Dennis\" :last-name \"Moore\" :soc-sec-\\# 550)
 (make-instance 'person :first-name \"Michael\" :last-name \"Palin\" :soc-sec-\\# 456)

 (p-select 'person :where 'soc-sec-\\# := 123)
        ==> \\#<person John Cleese 123>
 (p-select 'person :where 'soc-sec-\\# :greater-equal 300 :less 500)
        ==> (\\#<person Michael Palin 456>)
 (p-select 'person :where 'soc-sec-\\#)
        ==> (\\#<person John Cleese 123> \\#<person Michael Palin 456>
             \\#<person Dennis Moore 550>)
 \\end{lispcode}
\\Seealsolabel
 \\Fcite{persistent-metaclass}; \\fcite{pp-select};
 \\fcite{mapbtree}; section \\fcite{index ...}."

  (unless slot-name
    (error "Missing :where clause."))
  (let ((index (get-index-table the-class slot-name)))
    (unless index
      (error "No :index defined on slot ~A of ~A specified in :where clause."
	     slot-name the-class))
    (when (symbolp the-class)
      (setf the-class (find-class the-class)))
    (let ((selected ())
	  (subclass-cache (make-hash-table :test #'eq)))
      (if equals-to-p
	  (setf selected
	    (getindex equals-to index *default-depth* p-heap))
	(let ((argument-list
	       `(:key-depth :objid :data-depth :objid
			    :p-heap ,p-heap)))
	  (when lessp
	    (push less argument-list)
	    (push :< argument-list))
	  (when less-equal-p
	    (push less-equal argument-list)
	    (push :<= argument-list))
	  (when greater-equal-p
	    (push greater-equal argument-list)
	    (push :>= argument-list))
	  (when greaterp
	    (push greater argument-list)
	    (push :> argument-list))
	  (push index argument-list)
	  (push #'(lambda (key data)
		    (declare (ignore key))
		    (let ((data-class-name nil))
		      (multiple-value-bind (t-object foundp)
			  (is-registered-objid data)
			(if foundp
			    (setf data-class-name
			      (class-name
			       (class-description-of
				(class-of t-object))))
			  (let* ((p-objid-data-class
				  (p-instance-class-wrapper
				   data :objid p-heap))
				 (class-descr (is-registered-objid
					       p-objid-data-class)))
			    (unless class-descr
			      (setf class-descr
				(p-objid-to-t-object
				 p-objid-data-class
				 +instance-type-tag+
				 :cached
				 *default-persistent-heap*)))
			    (setf data-class-name (class-name class-descr))))
			(let ((data-class (find-class data-class-name nil)))
			  (when data-class
			    (multiple-value-bind (subclassp foundp)
				(gethash data-class-name subclass-cache)
			      (unless foundp
				(setf subclassp
				  (subtypep data-class the-class))
				(setf (gethash data-class-name
					       subclass-cache)
				  subclassp))
			      (when subclassp
				(push-on-end
				 (p-objid-to-t-object data
						      +instance-type-tag+
						      depth
						      p-heap)
				 selected)))))))
		    t)
		argument-list)
	  (apply #'mapindex argument-list)))
      selected)))

;;; ---------------------------------------------------------------------------
(defun pp-select (&rest all-args)
  #+:lisp-doc "Does exactly the same as the \\fcite{p-select},
 but \\lisp{pprint}s the result."
  (pprint (apply #'p-select all-args)))

;;; ---------------------------------------------------------------------------
(defun p-create-database (url)
  #+:lisp-doc "
\\Purposelabel
  Create a new database on \\funarg{url}. For technical reasons,
  a currently opened database is closed before the new database
  is created. Please note that it may take some time until the database
  is created, since the server must start up a new daemon process
  for the fresh created database."
  (close-heap)
  (catch-errors (c-sh-create-database (write-url (effective-url url))))
  (setf *database-url* url))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
