;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-heap.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	30.11.93
;;;; Description	PLOB persistent heap class methods.
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
(use-package :clos)

;;; ---------------------------------------------------------------------------
(defvar *instance->data-vector-cache* (make-hash-table :test #'eql)
  #+:lisp-doc "
\\Purposelabel
 A cache mapping an \\objid\\ of a persistent \\clos\\ instance
 to the \\objid\\ of the data vector of the instance.
\\Seealsolabel
 \\Fcite{p-instance-data-vector}.")

;;; ---------------------------------------------------------------------------
;;; Mapping between PLOB <-> LISP types
;;; ---------------------------------------------------------------------------
(defstruct (type-info
            (:constructor make-type-info (name tag objid-size value-size)))
  #+:lisp-doc "
\\Purposelabel
 A structure containing informations about
 \\plob\\ built-in types."

  (name ""
	#+:lisp-doc :documentation #+:lisp-doc "
 The type name.")

  (tag 0
       #+:lisp-doc :documentation #+:lisp-doc "
 The numeric \\typetag.")

  (objid-size 0
	      #+:lisp-doc :documentation #+:lisp-doc "
 The number of bits of the reference field
 of the type with \\typetag\\ {\\bf tag}.")

  (value-size 0
	      #+:lisp-doc :documentation #+:lisp-doc "
 The number of bits of the value field
 of the type with \\typetag\\ {\\bf tag}."))

;;; ---------------------------------------------------------------------------
(defvar *type-tag->type-info* (make-hash-table :test #'eql)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping \\typetag[s]\\ to type infos.
\\Seealsolabel
 \\Fcite{type-info}.")

;;; ---------------------------------------------------------------------------
(defvar *lisp-type->type-tag* (make-hash-table :test #'eql)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping \\cl\\ types to \\typetag[s]. Used
 in \\fcite{p-upgraded-array-element-tag}.
\\Seealsolabel
 \\Fcite{*lisp-type->subtypes*}.")

;;; ---------------------------------------------------------------------------
(defvar *lisp-type->subtypes* (make-hash-table :test #'eql)
  #+:lisp-doc "
\\Purposelabel
 A variable used for mapping \\cl\\ built-in classes to its
 subtypes. Used in \\fcite{p-upgraded-array-element-tag}.
\\Seealsolabel
 \\Fcite{*lisp-type->type-tag*}")

;;; ---------------------------------------------------------------------------
(defconstant +t-class+ (find-class t)
  #+:lisp-doc "The \\clsmo\\ of class \\class{t}.")

;;; ---------------------------------------------------------------------------
(defun find-nearest-built-in-class (type-symbol)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{type-symbol}}
      {a \\cl\\ type specifier}
\\Purposelabel
 Find the most specific built-in class for \\funarg{type-symbol}.
\\Exampleslabel
 \\begin{lispcode}
(find-nearest-built-in-class '(unsigned-byte 8))
        ==> #<built-in-class integer 102733C0>
 \\end{lispcode}"

  (labels
      ((descend-subclasses
	(the-class)
	(if (ignore-errors (subtypep type-symbol (class-name the-class)))
	    (dolist (subclass (class-direct-subclasses the-class) the-class)
	      (let ((found-subclass (descend-subclasses subclass)))
		(if found-subclass
		    (return found-subclass)))))))
    (let ((found (descend-subclasses +t-class+)))
      (if found
	  found
	+t-class+))))

;;; ---------------------------------------------------------------------------
(defun register-all-type-tags ()
  #+:lisp-doc "
\\Purposelabel
 Read in all \\plob\\ built-in data classes and store them to
 internal tables; these are used later for
 \\fcite{p-upgraded-array-element-tag}.
\\Seealsolabel
 \\Fcite{sh-map-class-info};
 \\fcite{p-upgraded-array-element-tag};
 \\fcite{t-type-tag-of}."

  (let ((type-names nil))
    (clrhash *type-tag->type-info*)
    (clrhash *lisp-type->type-tag*)
    (clrhash *lisp-type->subtypes*)
    (sh-map-class-info
     #'(lambda (type-tag type-name objid-size value-size)
	 (let* ((symbolic-type-name (read-from-string type-name))
		(lisp-type-name (when (symbolp symbolic-type-name)
                                  (find-class symbolic-type-name nil))))
	   (if lisp-type-name
	       (setf lisp-type-name (class-name lisp-type-name))
	     ;; There exists no class with name symbolic-type-name;
             ;; try if there is a valid LISP type name:
	     (let ((super-type-class (find-nearest-built-in-class
				      symbolic-type-name)))
	       (unless (eq super-type-class +t-class+)
		 (setf lisp-type-name symbolic-type-name)
		 (push (class-name super-type-class) type-names))))
	   (when lisp-type-name
	     (setf (gethash type-tag *type-tag->type-info*) 
                   (make-type-info lisp-type-name type-tag
                                   objid-size value-size))
	     (setf (gethash lisp-type-name *lisp-type->type-tag*) type-tag)
	     (push lisp-type-name type-names))
	   t)))
    (dolist (type-name type-names)
      (unless (gethash type-name *lisp-type->subtypes*)
        (let ((sub-types nil))
	  (dolist (sub-type-name type-names)
            (if (not (eq type-name sub-type-name))
		(when (subtypep sub-type-name type-name)
		  (push sub-type-name sub-types))))
          (when sub-types
	      (setf (gethash type-name *lisp-type->subtypes*) sub-types)))))
    (dolist (type-name type-names)
      (setf (gethash type-name *lisp-type->subtypes*)
            (sort (gethash type-name *lisp-type->subtypes*) #'subtypep)))))

;;; ---------------------------------------------------------------------------
(defun p-upgraded-array-element-tag (t-type)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-type}}
      {a \\cl\\ type specifier}
\\Valueslabel
 Returns a \\typetag.
\\Purposelabel
 See \\fcite{upgraded-array-element-type}; the only difference is
 that not a \\cl\\ type specifier but a \\typetag\\ is returned.
\\Seealsolabel
 \\Fcite{t-type-tag-of}."

  (let* ((class-name
          (class-name (find-nearest-built-in-class t-type)))
         (subclasses (gethash class-name *lisp-type->subtypes*)))
    (if subclasses
        ;; Find the most-special subclass:
	(dolist (subclass subclasses)
	  (when (subtypep t-type subclass)
	    (setf class-name subclass)
	    (return))))
    (gethash class-name *lisp-type->type-tag*)))

;;; ---------------------------------------------------------------------------
(defun t-type-tag-of (t-object)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-object}}
\\Purposelabel
 Return the most specific \\typetag\\ for \\funarg{t-object}.
\\Seealsolabel
 \\Fcite{p-upgraded-array-element-tag}."

  (let* ((class-name-of-t-object (class-name (class-of t-object)))
         (subclasses (gethash class-name-of-t-object *lisp-type->subtypes*)))
    (block type-tag-of
      (if subclasses
          ;; Find the most-special subclass:
	  (dolist (subclass subclasses)
	    (when (typep t-object subclass)
              (let ((type-tag (gethash subclass *lisp-type->type-tag*)))
                (unless (p-markerp type-tag)
	          (return-from type-tag-of type-tag))))))
      (gethash class-name-of-t-object *lisp-type->type-tag*))))

;;; ---------------------------------------------------------------------------
(defun get-type-info (type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isatypetag{\\funarg{type-tag}}
\\Purposelabel
 Find an instance of \\fcite{type-info}\\ for the
 \\plob\\ built-in type referenced by \\funarg{type-tag}.
\\Seealsolabel
 \\Fcite{type-info}."

  (gethash type-tag *type-tag->type-info*))

;;; ---------------------------------------------------------------------------
(defconstant +persistent-object-class+ (find-class 'persistent-object)
  #+:lisp-doc "The \\clsmo\\ of \\fcite{persistent-object}.")

;;; ---------------------------------------------------------------------------
(defvar *marker-tag->marker-object* (make-hash-table :test #'equal)
  #+:lisp-doc "A hash table mapping a marker tag to a marker object.")

;;; ---------------------------------------------------------------------------
(defun make-persistent-object (p-objid
                               &optional
                               (type-tag +short-objid-tag+))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isatypetag{\\funarg{type-tag}}
\\Valueslabel
 If \\funarg{type-tag}\\ references an immediate type,
 \\funarg{p-objid}\\ is `loaded' and returned;
 otherwise, an instance of \\fcite{persistent-object}\\ with
 its \\objid\\ set to \\funarg{p-objid}\\ is returned.
\\Purposelabel
 Converts \\funarg{p-objid}\\ into an instance of
 \\fcite{persistent-object}\\ for non-immediate objects,
 i.e.\\ for \\funarg{type-tag}\\ referencing a non-immmediate
 \\typetag;
 otherwise, the object itself is returned.

 The purpose of the returned object is to make it possible
 to recognize it as a persistent object by its type being
 \\class{persistent-object}; this recognition
 is not possible directly for the (numeric)
 \\funarg{p-objid}\\ since its type is \\class{fixnum}.
\\Seealsolabel
 \\Fcite{persistent-object}."

  (unless type-tag
    (setf type-tag +short-objid-tag+))
  (let ((numeric-objid (persistent-object-objid p-objid)))
    (cond
     ((p-markerp type-tag)
      (let ((marker-object
	     (gethash numeric-objid *marker-tag->marker-object*)))
	(unless marker-object
	  (setf marker-object
		(make-persistent-immediate-object numeric-objid type-tag))
	  (setf (gethash numeric-objid *marker-tag->marker-object*)
		marker-object))
	marker-object))
     ((p-immediatep type-tag)
      (make-persistent-immediate-object numeric-objid type-tag))
     (t
      (make-persistent-object-internal numeric-objid)))))

;;; ---------------------------------------------------------------------------
(defmethod persistent-object-objid ((p-object integer))
  #+:lisp-doc "Returns \\funarg{p-object}\\ itself, i.e.\\ \\funarg{p-object}\\ is
 interpreted as a numeric \\objid."

  p-object)

;;; ---------------------------------------------------------------------------
(defmethod persistent-object-objid ((p-object persistent-object))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure reader function
 {\\bf persistent-object-internal-objid}."

  (persistent-object-internal-objid p-object))

;;; ---------------------------------------------------------------------------
(defmethod persistent-object-objid (p-object)
  #+:lisp-doc "Look into the cache for \\funarg{p-object}'s \\objid."
  (is-registered-object p-object))

;;; ---------------------------------------------------------------------------
#|
;; 1997/10/29 HK: No good idea, makes persistent-object-objid rather
;; incanonic:

(defmethod persistent-object-objid ((p-object string))
  #+:lisp-doc "Calls to this method are trapped to
 \\fcite{map-string-to-objid}\\ with argument \\funarg{p-object}.
 The idea is that passing a transient string to
 \\fcite{persistent-object-objid}\\ is interpreted as requesting
 an equal-named persistent symbol."

  (map-string-to-objid p-object))
|#

;;; ---------------------------------------------------------------------------
#|
;; 1997/10/29 HK: No good idea, makes persistent-object-objid rather
;; uncanonic:
(defmethod persistent-object-objid ((p-object symbol))
  #+:lisp-doc "Calls to this method are trapped to
 \\fcite{map-string-to-objid}\\ with argument \\funarg{p-object}.
 The idea is that passing a transient symbol to
 \\fcite{persistent-object-objid}\\ is interpreted as requesting
 an equal-named persistent symbol."

  (map-string-to-objid p-object))
|#

;;; ---------------------------------------------------------------------------
(defmethod (setf persistent-object-objid) (the-objid
                                           (p-object persistent-object))
  #+:lisp-doc "Calls to this method are trapped to the system-generated
 structure writer function
 {\\bf (setf persistent-object-internal-objid)}; see also
 \\fcite{persistent-object}."

  (setf (persistent-object-internal-objid p-object) the-objid))

;;; ---------------------------------------------------------------------------
(defmacro with-read-lock ((p-heap p-objid depth expecting-type-tag force-read)
			  &rest forms)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isatypetag{\\funarg{expecting-type-tag}}
 \\isa{\\funarg{forms}}
      {an implicit \\lisp{progn}\\ expression
       with calls to read from the persistent object
       referenced by \\funarg{p-objid}}
\\Valueslabel
 Returns either the value of evaluating \\funarg{forms}\\ or
 \\lispnil.
\\Purposelabel
 The \\funarg{forms}\\ are only evaluated iff there was no previous
 read-lock set onto the persistent object referenced by
 \\funarg{p-objid}. This is done to prevent a non-terminating
 recursion when loading self-referencing persistent objects:
 the reading done in \\funarg{forms}\\ will stop as soon as
 \\funarg{forms}\\ should be evaluated a second time for an already
 `seen' persistent object.
\\Remarkslabel
 The granted read lock is released at the end of
 the transaction active on \\funarg{p-heap};
 see \\fcite{end-transaction}.
\\Seealsolabel
 \\Fcite{read-lock};
 \\fcite{with-write-lock}."

  `(when (read-lock ,p-heap ,p-objid ,depth ,expecting-type-tag
		    ,force-read +lock-vector-read+)
     ,@forms))

;;; ---------------------------------------------------------------------------
(defmacro with-write-lock ((p-heap p-objid t-object depth
			    expecting-type-tag force-write)
			   &rest forms)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isatypetag{\\funarg{expecting-type-tag}}
 \\isa{\\funarg{forms}}
      {an implicit \\lisp{progn}\\ expression
       with calls to write to the persistent object
       referenced by \\funarg{p-objid}}
\\Valueslabel
 Returns either the value of evaluating \\funarg{forms}\\ or
 \\lispnil.
\\Purposelabel
 The \\funarg{forms}\\ are only evaluated iff there was no previous
 write-lock set onto the persistent object referenced by
 \\funarg{p-objid}. This is done to prevent a non-terminating
 recursion when writing self-referencing persistent objects:
 the writing done in \\funarg{forms}\\ will stop as soon as
 \\funarg{forms}\\ should be evaluated a second time for an
 already `written-to' persistent object.
\\Remarkslabel
 The granted write lock is released at the end of
 the transaction active on \\funarg{p-heap};
 see \\fcite{end-transaction}.
\\Seealsolabel
 \\Fcite{write-lock};
 \\fcite{with-read-lock}."

  `(when (write-lock ,p-heap ,p-objid ,t-object ,depth ,expecting-type-tag
		     ,force-write +lock-vector-write+)
     ,@forms))

;;; --- Low-level creation/accessor functions ---------------------------------

(defun p-allocate (p-heap type-tag
 			  &optional (number-of-extra-references 0)
			  (extra-values-type-tag +null-type-tag+)
			  (number-of-extra-values 0))
  #+:lisp-doc "
\\Argumentslabel
 \\isatypetag{\\funarg{type-tag}}
 \\isa{\\funarg{number-of-extra-objids}\\ resp.\\ %
       \\funarg{number-of-extra-values}}
      {a fixnum}
\\Valueslabel
 The \\objid\\ of the allocated persistent object is returned.
\\Purposelabel
 Allocate a persistent object in the \\sh\\ with a
 \\plob\\ built-in type referenced by \\funarg{type-tag}.

 Table~\\ref{tab:layout} shows the \\sh\\ vector layout
 superposed by \\plob\\ above the \\postore\\ vector layout.
 \\begin{figure}[htbp]\\centering%
 \\begin{tabular}{|c|p{7cm}|l|l|}
 \\hline
 \\tabularheader{Words}
	&\\tabularheader{Description}
		&\\tabularheader{Reader}
			&\\tabularheader{Writer}\\\\
 \\hline\\hline
 $1$
  & Size $m$ of reference field in words
   & {\\bf p-objid-size}
    & ---\\\\
 \\hline
 $1$
  & Size $n$ of value field in words
   & {\\bf p-value-size}
    & ---\\\\
 \\hline
 $1$
  & \\TypeTag, Flags
   & {\\bf p-type-tag-of}
    & ---\\\\
 \\hline
 $1$
  & Lock-Info
   & \\multicolumn{2}{l|}%
     {{\\bf p-insert-lock}, {\\bf p-set-lock}, {\\bf p-unlock}} \\\\
 \\hline
 $m$
  & Reference Field: References to other \\sh\\ vectors
   & {\\bf p-index}
    & {\\bf (setf p-index)} \\\\
 \\hline
 $n$
  & Value field: Uninterpreted values
   & {\\bf p-values-into}
    & {\\bf (setf p-values)}\\\\
 \\hline
 \\end{tabular}%
 \\caption{\\sh\\ persistent object layout}\\label{tab:layout}%
 \\end{figure}%
 The reference resp.\\ value field span $m$
 resp.\\ $n$ memory words.

 The reference field can only be adressed on a per-element
 basis; this is done by passing an element index into the
 reference field additionally to the \\sh\\ vector's
 \\objid; the element index is zero-origined
 w.r.t.\\ the begin of the reference field. It is always checked
 if the passed index is a valid index into the reference field.

 Since no reading or writing of single value elements is necessary,
 the value field can only be adressed as a whole, i.e.\\ a
 complete \\cl\\ immediate vector or string is copied into or
 out of a \\sh\\ vector. The copying is limited to a maximum of
 $n$ words; no error is signalled when a larger number of words
 was passed to {\\bf p-values} resp.\\ {\\bf (setf p-values)}.
\\Remarkslabel
 The \\funarg{type-tag}\\ is checked by the \\plob\\ C level if it is a
 valid \\typetag; the object's size is derived from \\funarg{type-tag}.
 If the class denoted by \\funarg{type-tag}\\ has a variable-length
 reference resp.\\ value field, \\funarg{number-of-extra-objids}\\ %
 resp.\\ \\funarg{number-of-extra-values}\\ extra reference
 resp.\\ value words are additionally allocated; otherwise, the
 parameter \\funarg{number-of-extra-objids}\\ resp.\\ %
 \\funarg{number-of-extra-values}\\ is ignored.
\\Seealsolabel
 \\fcite{p-objid-size};
 \\fcite{p-value-size};
 \\fcite{p-type-tag-of};
 \\fcite{p-insert-lock};
 \\fcite{p-set-lock};
 \\fcite{p-unlock};
 \\fcite{p-index};
 \\fcite{(setf p-index)};
 \\fcite{p-values-into};
 \\fcite{(setf p-values)};
 \\fcite{sh-create-object}."

  (declare (type fixnum type-tag
	         number-of-extra-references number-of-extra-values))
  (assert-open-session-p p-heap)
  (sh-create-object (persistent-object-objid p-heap)
                    type-tag
		    number-of-extra-references
		    extra-values-type-tag
		    number-of-extra-values))
  
;;; ---------------------------------------------------------------------------
(defun p-destroy (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Destroy the persistent object referenced by \\funarg{p-objid}.
\\Remarkslabel
 Currently, the object is not really destroyed; it is marked by
 \\plob\\ as a `zombie' object. This leads to an error message if
 such a `zombie' object is de-referenced later.

 The only way to physically destroy a persistent object is to remove
 all references from other persistent objects onto the object to
 destroy and to perform a garbage collection."

  (assert-open-session-p p-heap)
  (sh-destroy-object (persistent-object-objid p-heap)
                     (persistent-object-objid p-objid)))

;;; ---------------------------------------------------------------------------
(defun p-objid-size (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return the number of words of the reference field
 for the persistent object referenced by \\funarg{p-objid}.

 There is {\\sl no} {\\bf (setf p-objid-size)} because the size of a
 persistent object cannot be changed; it is fix for its lifetime.
\\Seealsolabel
 \\Fcite{p-value-size};
 \\fcite{sh-objid-size};
 \\fcite{p-allocate}."

  ;; (assert-open-session-p p-heap)
  (sh-objid-size (persistent-object-objid p-heap)
                 (persistent-object-objid p-objid)))

;;; ---------------------------------------------------------------------------
(defun p-value-size (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Return the number of words of the value field
 for the persistent object referenced by \\funarg{p-objid}.

 There is no {\\bf (setf p-value-size)} because the size of a
 persistent object cannot be changed; it is fix for its lifetime.

\\Seealsolabel

 \\Fcite{p-objid-size};
 \\fcite{sh-value-size};
 \\fcite{p-allocate}."

  (sh-value-size (persistent-object-objid p-heap)
                 (persistent-object-objid p-objid)))

;;; ---------------------------------------------------------------------------
(defgeneric p-type-tag-of (p-objid
			   &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Return the \\typetag\\ of the persistent object referenced
 by \\funarg{p-objid}.

 There is {\\sl no} {\\bf (setf p-type-tag-of)} because the type of a
 persistent object cannot be changed; it is fix for the lifetime of a
 persistent object.

\\Seealsolabel

 \\Fcite{sh-type-tag-of};
 \\fcite{p-allocate}."))

;;; ---------------------------------------------------------------------------
(defmethod p-type-tag-of ((p-objid persistent-immediate-object)
			  &optional (p-heap *default-persistent-heap*))
  (let ((type-tag (persistent-immediate-object-type-tag p-objid)))
    (unless (and type-tag (not (eql type-tag +short-objid-tag+)))
      (setf type-tag (call-next-method))
      (setf (persistent-immediate-object-type-tag p-objid) type-tag))
    type-tag))

;;; ---------------------------------------------------------------------------
(defmethod p-type-tag-of ((p-objid slot-load-on-demand)
			  &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "\\Fcite{slot-load-on-demand}\\ is a subclass of
 \\fcite{persistent-immediate-object}, but since both are structure
 classes, duplication of methods is necessary."
  (let ((type-tag (slot-load-on-demand-type-tag p-objid)))
    (unless (and type-tag (not (eql type-tag +short-objid-tag+)))
      (setf type-tag (call-next-method))
      (setf (slot-load-on-demand-type-tag p-objid) type-tag))
    type-tag))

;;; ---------------------------------------------------------------------------
(defmethod p-type-tag-of (p-objid &optional (p-heap *default-persistent-heap*))
  (sh-type-tag-of (persistent-object-objid p-heap)
                  (persistent-object-objid p-objid)))

;;; ---------------------------------------------------------------------------
(defun p-class-of
  (p-objid
   &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 Either an instance of \\fcite{structure-description}\\ or
 an instance of \\fcite{class-description}\\ or
 a symbol.
\\Purposelabel
 Returns a class description for \\funarg{p-objid}\\ if available.
 If the class of the object referenced by \\funarg{p-objid}\\ has
 no class description, its type tag converted to a symbol named
 by the string equivalent of the persistent object's type tag
 is returned.
\\Remarkslabel
 1996/10/14 HK: Looks as if p-class-of crashes the server with a
 segmentation violation when working on description objects passed
 as p-objid.
\\Seealsolabel
 \\Fcite{structure-description};
 \\fcite{class-description}."

  ;; (assert-open-session-p p-heap)
  (let ((type-tag (p-type-tag-of p-objid p-heap)))
    (declare (type fixnum type-tag)
	     #-:lispworks4 ;; and hopefully not later
	     (dynamic-extent type-tag))
    (cond
     ((= type-tag +structure-type-tag+)
      (p-structure-descr p-objid depth p-heap))
     ((= type-tag +instance-type-tag+)
      (p-instance-class-wrapper p-objid depth p-heap))
     (t
      (let* ((string-type-tag (string-upcase (sh-type-tag-to-string
					      type-tag)))
	     (symbol-type-tag (find-symbol string-type-tag :common-lisp)))
	(unless symbol-type-tag
	  (setf symbol-type-tag (find-symbol string-type-tag
					     :keyword)))
	(unless symbol-type-tag
	  (setf symbol-type-tag (intern string-type-tag :plob)))
	symbol-type-tag)))))

;;; ---------------------------------------------------------------------------
(defun (setf p-fixnum) (the-fixnum p-heap p-objid at-location
				   &optional
				   (expecting-type-tag +null-type-tag+)
				   (expecting-class +null-objid+))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-fixnum}}
      {a fixnum}
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}}
\\Valueslabel
 \\retarg{\\funarg{the-fixnum}}
\\Purposelabel
 Write to a persistent object's component; the
 slot value of the persistent object referenced by
 \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
 set to \\funarg{the-fixnum}.
\\Remarkslabel
 It is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-objid}.

 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-objid}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 It is checked if a write-lock is set either on `element'-level
 on position \\funarg{at-location}\\ in \\funarg{p-objid}\\ or
 on `vector'-level on \\funarg{p-objid}; if neither lock is
 set, an `element'-level write lock is set on position
 \\funarg{at-location}\\ in \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-fixnum};
 \\fcite{sh-write-fixnum}."

  (declare (type fixnum the-fixnum at-location expecting-type-tag))
  (sh-write-fixnum (persistent-object-objid p-heap)
		   (persistent-object-objid p-objid) at-location
		   expecting-class expecting-type-tag the-fixnum)
  the-fixnum)

;;; ---------------------------------------------------------------------------
(defun p-fixnum (p-heap p-objid
                        at-location
			&optional (expecting-type-tag +null-type-tag+)
			(expecting-class +null-objid+))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}}
 \\isanobjid{\\funarg{expecting-class}}
\\Valueslabel
 Returns the fixnum object located at position
 \\funarg{at-location}\\ in \\funarg{p-objid}.
\\Purposelabel
 Read a persistent object of immediate type fixnum.
 The slot value of the persistent object referenced by
 \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
 returned as a fixnum.
\\Remarkslabel
 It is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-objid}.

 It is checked if the object contained at position
 \\funarg{at-location}\\ in the persistent object referenced by
 \\funarg{p-objid}\\ is an object of type fixnum.

 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-objid}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 It is checked if a read-lock is set either on `element'-level
 on position \\funarg{at-location}\\ in \\funarg{p-objid}\\ or
 on `vector'-level on \\funarg{p-objid}; if neither lock is
 set, an `element'-level read lock is set on position
 \\funarg{at-location}\\ in \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-fixnum)};
 \\fcite{sh-read-fixnum}."

  (declare (type fixnum at-location expecting-type-tag))
  (sh-read-fixnum (persistent-object-objid p-heap)
                  (persistent-object-objid p-objid) at-location
                  expecting-class expecting-type-tag))

;;; ---------------------------------------------------------------------------
(defun (setf p-index) (immediate-value p-heap p-objid at-location
				       &optional
				       (expecting-type-tag +null-type-tag+)
				       (expecting-class +null-objid+)
				       (immediate-type-tag +short-objid-tag+))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{immediate-value}}
      {either an immediate value or an \\objid}
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}\\ %
              resp.\\ \\funarg{immediate-type-tag}}
\\Valueslabel
 \\retoldmode{\\funarg{p-heap}}{\\funarg{p-objid}}
\\Purposelabel
 Write to a persistent object's component; the
 slot value of the persistent object referenced by
 \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
 set to the persistent object which is made up from the
 \\funarg{immediate-value}\\ and
 \\funarg{immediate-type-tag}\\ argument.
 \\Remarkslabel
 It is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-objid}.

 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-objid}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 It is checked if a write-lock is set either on `element'-level
 on position \\funarg{at-location}\\ in \\funarg{p-objid}\\ or
 on `vector'-level on \\funarg{p-objid}; if neither lock is
 set, an `element'-level write lock is set on position
 \\funarg{at-location}\\ in \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-index};
 \\fcite{sh-write-index}."

  (declare (type fixnum at-location expecting-type-tag expecting-class))
  (sh-write-index (persistent-object-objid p-heap)
		  (persistent-object-objid p-objid) at-location
		  expecting-class expecting-type-tag
		  immediate-value immediate-type-tag))

;;; ---------------------------------------------------------------------------
(defun p-index (p-heap p-objid at-location
		       &optional (expecting-type-tag +null-type-tag+)
		       (expecting-class +null-objid+))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}\\ %
              resp.\\ \\funarg{immediate-type-tag}}
\\Valueslabel
 Returns two values:
 \\begin{enumerate}

 \\item The \\objid\\ of the persistent object located at position
  \\funarg{at-location}\\ in \\funarg{p-objid}.

 \\item The \\typetag\\ of the persistent object located at position
  \\funarg{at-location}\\ in \\funarg{p-objid}.

 \\end{enumerate}
\\Purposelabel
 Read a persistent object's component; the
 slot value of the persistent object referenced by
 \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
 returned as an \\objid\\ and a \\typetag.
\\Remarkslabel
 It is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-objid}.

 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-objid}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 It is checked if a read-lock is set either on `element'-level
 on position \\funarg{at-location}\\ in \\funarg{p-objid}\\ or
 on `vector'-level on \\funarg{p-objid}; if neither lock is
 set, an `element'-level lock is set on position
 \\funarg{at-location}\\ in \\funarg{p-objid}.

 The \\funarg{immediate-type-tag}\\ argument is ignored.
\\Seealsolabel
 \\Fcite{(setf p-index)};
 \\fcite{sh-read-index}."

  (declare (type fixnum at-location expecting-type-tag expecting-class))
  (sh-read-index (persistent-object-objid p-heap)
		 (persistent-object-objid p-objid)
                  at-location
                  expecting-class expecting-type-tag))

;;; ---------------------------------------------------------------------------
(defun (setf p-marker) (the-p-marker p-heap p-objid at-location
				     &optional
				     (expecting-type-tag +null-type-tag+)
				     (expecting-class +null-objid+))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-p-marker}}
      {a marker object, i.e.\\ one of the values of the constants
       {\\bf +slot-unbound-type-tag+},
       {\\bf +unbound-type-tag+} or
       {\\bf +unstorable-object-marker+}}
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}\\ %
              resp.\\ \\funarg{immediate-type-tag}}
\\Valueslabel
 \\retoldmode{\\funarg{p-heap}}{\\funarg{p-objid}}
\\Purposelabel
 Write to a persistent object's component; the
 slot value of the persistent object referenced by
 \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
 set to \\funarg{the-p-marker}. The possible values of 
 \\funarg{the-p-marker}\\ have following meanings:
 \\begin{description}

 \\item[{\\bf +slot-unbound-type-tag+}]
  The slot of the persistent object referenced by
  \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
  an unbound slot in the sense of \\clos.

 \\item[{\\bf +unbound-type-tag+}]
  The slot of the persistent object referenced by
  \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
  unbound.

 \\item[{\\bf +unstorable-object-marker+}]
  The slot of the persistent object referenced by
  \\funarg{p-objid}\\ at position \\funarg{at-location}\\ contains
  an unstorable object which cannot be reloaded. These are
  e.g.\\ objects whose class-extent is marked as
  \\lisp{:transient}; if \\plob\\ `stores' such an object
  at position \\funarg{at-location}\\ of the persistent object
  referenced by \\funarg{p-objid}, not the transient
  object's representation is stored but a
  {\\bf +unstorable-object-marker+}.

 \\end{description}
\\Remarkslabel
 It is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-objid}.

 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-objid}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 It is checked if a write-lock is set either on `element'-level
 on position \\funarg{at-location}\\ in \\funarg{p-objid}\\ or
 on `vector'-level on \\funarg{p-objid}; if neither lock is
 set, an `element'-level write lock is set on position
 \\funarg{at-location}\\ in \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-marker}."

  (declare (type fixnum the-p-marker at-location
		 expecting-type-tag expecting-class))
  #-:lispworks4.2
  (assert (typep the-p-marker 'p-marker-type))
  (sh-write-index (persistent-object-objid p-heap)
		  (persistent-object-objid p-objid) at-location
		  expecting-class expecting-type-tag
		  the-p-marker the-p-marker))

;;; ---------------------------------------------------------------------------
(defun p-marker (p-heap p-objid at-location
			&optional (expecting-type-tag +null-type-tag+)
			(expecting-class +null-objid+))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}}
\\Valueslabel
 Returns the marker object located at position
 \\funarg{at-location}\\ in \\funarg{p-objid}; if no marker
 was found, \\lispnil\\ is returned.
\\Purposelabel
 Read a persistent object of (immediate) type marker.
 The slot value of the persistent object referenced by
 \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
 returned as a marker.
\\Remarkslabel

 It is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-objid}.

 If the object contained at position
 \\funarg{at-location}\\ in the persistent object referenced by
 \\funarg{p-objid}\\ is an object of type marker,
 the marker is returned, otherwise \\lispnil.

 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-objid}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 It is checked if a read-lock is set either on `element'-level
 on position \\funarg{at-location}\\ in \\funarg{p-objid}\\ or
 on `vector'-level on \\funarg{p-objid}; if neither lock is
 set, an `element'-level read lock is set on position
 \\funarg{at-location}\\ in \\funarg{p-objid}.

\\Seealsolabel

 \\Fcite{(setf p-marker)}."

  (declare (type fixnum at-location expecting-type-tag expecting-class))
  (multiple-value-bind (the-p-marker the-p-marker-type-tag)
      (sh-read-index (persistent-object-objid p-heap)
                     (persistent-object-objid p-objid) at-location
                     expecting-class expecting-type-tag)
    (declare (type fixnum the-p-marker the-p-marker-type-tag)
             (ignore the-p-marker))
    (when (p-markerp the-p-marker-type-tag)
      the-p-marker-type-tag)))

;;; ---------------------------------------------------------------------------
(defun (setf p-objid) (the-objid p-heap p-id at-location
				 &optional
				 (expecting-type-tag +null-type-tag+)
				 (expecting-class +null-objid+))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{the-objid}\\ resp.\\ \\funarg{p-id}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}}
\\Valueslabel
 \\retoldmode{\\funarg{p-heap}}{\\funarg{p-id}}
\\Purposelabel
 Write to a persistent object's component; the
 slot value of the persistent object referenced by
 \\funarg{p-id}\\ at position \\funarg{at-location}\\ is
 set to \\funarg{the-objid}.
\\Remarkslabel
 It is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-id}.

 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-id}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 It is checked if a write-lock is set either on `element'-level
 on position \\funarg{at-location}\\ in \\funarg{p-id}\\ or
 on `vector'-level on \\funarg{p-id}; if neither lock is
 set, an `element'-level write lock is set on position
 \\funarg{at-location}\\ in \\funarg{p-id}.
\\Seealsolabel
 \\Fcite{p-objid};
 \\fcite{sh-write-objid}."

  (declare (type fixnum at-location expecting-type-tag expecting-class))
  (sh-write-objid (persistent-object-objid p-heap)
		  (persistent-object-objid p-id) at-location
		  expecting-class expecting-type-tag
		  (persistent-object-objid the-objid)))

;;; ---------------------------------------------------------------------------
(defun p-objid (p-heap p-id
                       at-location
		       &optional (expecting-type-tag +null-type-tag+)
		       (expecting-class +null-objid+))

  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-id}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}}

\\Valueslabel

 Returns the \\objid\\ of the object located at position
 \\funarg{at-location}\\ in \\funarg{p-id}.

\\Purposelabel

 Read of a non-immediate persistent object; the
 slot value of the persistent object referenced by
 \\funarg{p-id}\\ at position \\funarg{at-location}\\ is
 returned as an \\objid.

\\Remarkslabel

 It is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-id}.

 It is checked if the object contained at position
 \\funarg{at-location}\\ in the persistent object referenced by
 \\funarg{p-id}\\ is a non-immediate object.

 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-id}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 It is checked if a read-lock is set either on `element'-level
 on position \\funarg{at-location}\\ in \\funarg{p-id}\\ or
 on `vector'-level on \\funarg{p-id}; if neither lock is
 set, an `element'-level read lock is set on position
 \\funarg{at-location}\\ in \\funarg{p-id}.

\\Seealsolabel

 \\Fcite{(setf p-objid)};
 \\fcite{sh-read-objid}."

  (declare (type fixnum at-location expecting-type-tag expecting-class))
  (sh-read-objid (persistent-object-objid p-heap)
		 (persistent-object-objid p-id) at-location
		 expecting-class expecting-type-tag))

;;; ---------------------------------------------------------------------------
(defun (setf p-values) (from-t-object p-heap p-objid element-type-tag
				      number-of-words
			              &optional
			              (expecting-type-tag +null-type-tag+)
				      (expecting-class +null-objid+))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{from-t-object}}
      {a simple vector}
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{number-of-words}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}}
\\Valueslabel
 Returns either a negative error number or the number of words
 actually written.
\\Purposelabel
 Write to the persistent object's value field referenced by
 \\funarg{p-objid}\\ from the transient
 \\funarg{from-t-simple-vector}\\ exact
 \\funarg{number-of-words}\\ words.
\\Remarkslabel
 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-objid}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 It is checked if a write-lock is set
 on `vector'-level on \\funarg{p-objid}; if no lock is
 set, a `vector'-level write lock is set on \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-values-into};
 \\fcite{sh-write-values}."

  (declare (type fixnum number-of-words expecting-type-tag expecting-class))
  (sh-write-values (persistent-object-objid p-heap)
		   (persistent-object-objid p-objid)
		   expecting-class expecting-type-tag
		   element-type-tag from-t-object number-of-words))

;;; ---------------------------------------------------------------------------
(defun p-values-into (p-heap p-objid element-type-tag
			     into-t-object number-of-elements
			     &optional (expecting-type-tag +null-type-tag+)
			     (expecting-class +null-objid+))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{into-t-object}}
      {a simple vector}
 \\isa{\\funarg{number-of-elements}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}}
\\Valueslabel
 Returns either a negative error number or the number of elements
 actually read.
\\Purposelabel
 Read from the persistent object's value field referenced by
 \\funarg{p-objid}\\ destructively into the transient
 \\funarg{into-t-object}\\ a maximum of
 \\funarg{number-of-elements}\\ elements.
\\Remarkslabel
 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-objid}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 It is checked if a read-lock is set
 on `vector'-level on \\funarg{p-objid}; if no lock is
 set, a `vector'-level read lock is set on \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-allocate};
 \\fcite{(setf p-values)};
 \\fcite{sh-read-values};
 \\shcite{function}{SH\\us{}read\\us{}words}{5}."

  (declare (type fixnum number-of-elements expecting-type-tag
		 expecting-type-tag))
  (sh-read-values (persistent-object-objid p-heap)
                  (persistent-object-objid p-objid)
                  element-type-tag into-t-object number-of-elements
                  expecting-class expecting-type-tag))

;;; ---------------------------------------------------------------------------
(defun (setf p-chars) (from-string p-heap p-objid number-of-characters)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{from-string}}
      {a string}
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{number-of-s}}
      {a fixnum}
\\Valueslabel
 Returns either a negative error number or the number of characters
 actually written.
\\Purposelabel
 Write to the persistent object's value field referenced by
 \\funarg{p-objid}\\ from the transient
 \\funarg{from-string}\\ exact
 \\funarg{number-of-characters}\\ characters.
\\Remarkslabel
 It is checked if a write-lock is set
 on `vector'-level on \\funarg{p-objid}; if no lock is
 set, a `vector'-level write lock is set on \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-chars-into};
 \\fcite{sh-write-chars}."

  (declare (type fixnum number-of-characters))
  (sh-write-chars (persistent-object-objid p-heap)
		  (persistent-object-objid p-objid)
		  from-string number-of-characters))

;;; ---------------------------------------------------------------------------
(defun p-chars-into (p-heap p-objid into-string number-of-characters)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{into-string}}
      {a string}
 \\isa{\\funarg{number-of-characters}}
      {a fixnum}
\\Valueslabel
 Returns either a negative error number or the number of characters
 actually read.
\\Purposelabel
 Read from the persistent object's value field referenced by
 \\funarg{p-objid}\\ destructively into the transient
 \\funarg{into-string}\\ a maximum of
 \\funarg{number-of-characters}\\ characters.
\\Remarkslabel
 It is checked if a read-lock is set
 on `vector'-level on \\funarg{p-objid}; if no lock is
 set, a `vector'-level read lock is set on \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-allocate};
 \\fcite{(setf p-chars)};
 \\fcite{sh-read-chars};
 \\shcite{function}{SH\\us{}read\\us{}words}{5}."

  (declare (type fixnum number-of-characters))
  (sh-read-chars (persistent-object-objid p-heap)
		 (persistent-object-objid p-objid)
		 into-string number-of-characters))

;;; --- Locking ---------------------------------------------------------------

#+(and :lisp-doc :document-api)
(:defdoc
 "locking ..."
 "Object Locking"
 "
 \\plob\\ offers multi-level multi-mode locking. Locking an object
 means placing a lock with a certain level and a certain mode by a
 locking object onto a locked object; so at locking there are
 always two objects involved: the {\\sl locking} persistent object
 and the {\\sl locked} persistent object.

 Locking is implemented in the C level of \\plob. The class of the
 locking and the locked object can in principle be any of the
 \\plob\\ built-in classes, but only instances of
 \\fcite{persistent-heap}\\ as locking object's class have
 specialized methods\\footnote{Do not try to locate these in the
 \\cl\\ code; they are implemented in the C level.} to cope
 meaningful with locking another object:
 \\begin{itemize}

 \\item The lock request onto the locked persistent object is
  recorded in the transaction log of the locking
  \\class{persistent-heap}\\ instance; when the transaction is either
  ended or cancelled, the lock placed onto the locked persistent
  object will be released.

 \\item When a write lock is placed onto the locked persistent
  object, its state is recorded in the transaction log of the
  locking \\class{persistent-heap}\\ instance too; in case of
  cancelling the transaction, this information is used to
  {\\sl roll-back} the locked persistent object to its state
  when the transaction was started.

 \\end{itemize}

 Multi-level locking means that a lock can be placed on one
 of the levels specified in figure~\\ref{tab:lock-levels}.
 \\begin{figure}[htbp]\\centering%
 \\begin{tabular}{|c|l|p{9cm}|}%
 \\hline
 \\tabularheader{Level}
	&\\tabularheader{Constant}
		&\\tabularheader{Description}\\\\
 \\hline\\hline
 ---
  & {\\bf +lock-level-nothing+}
   & Lock nothing at all. This level can be used to suppress
     a lock request though one of the locking functions is
     called, i.e.\\ calling one of the locking functions with
     level {\\bf +lock-level-nothing+} does nothing.\\\\
 \\hline
 Element
  & {\\bf +lock-level-element+}
   & Lock one element of a persistent object. The element's
     position to lock is identified by an
     \\funarg{index}\\ parameter passed to the lock and unlock
     functions.\\\\
 \\hline
 Vector
  & {\\bf +lock-level-vector+}
   & Lock the whole persistent object.\\\\
 \\hline
 Store
  & {\\bf +lock-level-store+}
   & Lock the whole \\sh.
     \\note\\ This mode is not yet tested since \\plob\\ does not
     need it up to now.\\\\
 \\hline
 \\end{tabular}
 \\caption{Lock levels}\\label{tab:lock-levels}%
 \\end{figure}
 The possible modes of locks placed onto the locked object
 are shown in figure~\\ref{tab:lock-modes}.
 \\begin{figure}[htbp]\\centering%
 \\begin{tabular}{|c|l|p{8cm}|}
 \\hline
 \\tabularheader{Mode}
	&\\tabularheader{Constant}
		&\\tabularheader{Description}\\\\
 \\hline\\hline
 ---
  & {\\bf +lock-mode-nothing+}
   & Set no lock on passed level; just return the current lock
     mode of the persistent object on the passed level.\\\\
 \\hline
 Read-Only
  & {\\bf +lock-mode-read-only+}
   & Mark the persistent object as read-only; all following attempts to
     set a write lock to the persistent oject will fail.
     \\note\\ To set resp.\\ remove a read-only lock, use the
     \\fcite{(setf p-read-only)}, because using the
     \\fcite{p-set-lock}\\ will result in a read-only lock which will
     be released at the end of the current transaction.\\\\
 \\hline
 Read
  & {\\bf +lock-mode-read+}
   & Set a read lock onto the persistent object for passed level.\\\\
 \\hline
 Write
  & {\\bf +lock-mode-write+}
   & Set a write lock onto the persistent object for passed level.\\\\
 \\hline
 \\end{tabular}
 \\caption{Lock modes}\\label{tab:lock-modes}%
 \\end{figure}

 When a session tries to put its lock on an object locked by another
 session and the requested lock conflicts with the one already set,
 there is a lock conflict. The conflicts which may arise for incompatible
 locks are shown in the compatibility matrix in
 figure~\\ref{tab:lock-conflicts}.
 \\begin{figure}[htbp]\\centering%
 \\def\\coliw{8em}%
 \\def\\pbox#1{\\parbox{\\coliw}{\\centering#1}}%
 \\def\\vvbox{\\pbox{\\checked}}%
 \\def\\XXbox{\\pbox{\\crossed}}%
 \\begin{tabular}{|c|p{\\coliw}|p{\\coliw}|p{\\coliw}|}
 \\cline{2-4}
 \\multicolumn{1}{c|}{}
  &\\pbox{\\tabularheader{Read-Only granted}}
	&\\pbox{\\tabularheader{Read granted}}
		&\\pbox{\\tabularheader{Write granted}}\\\\ \\hline
 %
 %                      RO       R        W
 \\tabularheader{Read-Only requested}
	&\\vvbox &\\vvbox &\\vvbox\\\\ \\hline
 \\tabularheader{Read requested}
	&\\vvbox &\\vvbox &\\XXbox\\\\ \\hline
 \\tabularheader{Write requested}
	&\\XXbox &\\XXbox &\\XXbox\\\\ \\hline
 %
 \\multicolumn{4}{r}{%
  \\checked\\ $\\equiv$\\ Compatible locks\\quad
  \\crossed\\ $\\equiv$\\ Incompatible locks}
 \\end{tabular}
 \\caption{Compatibility Matrix}\\label{tab:lock-conflicts}
 \\end{figure}
 The session which tries to request a conflicting lock is suspended
 either until the lock conflict no longer exists, i.e.\\ up to the
 moment when the other sessions release their locks or the session
 has been suspended for at least
 {\\bf *suspend-timeout*} seconds. The timeout algorithm is used
 because there is no deadlock detection; if there would be no
 timeout, a deadlock would block the involved sessions
 {\\sl ad infinitum}\\/. When a suspended session reaches
 such a timeout condition, an error is raised with a message
 that a lock request failed. When there are multiple sessions
 waiting for a lock, the lock requests are put into a
 priority queue with read-only locks requests having the least
 and write locks having the highest priority, i.e.\\ sessions
 waiting for read-only locks are waked up after sessions waiting
 for write locks. This is done to prevent waiting sessions from
 starvation.

 Since it is not possible to set in one call to
 one of the below specified locking functions more than one lock
 level and one lock mode per call, use
 {\\sl exactly one} of the bitmask constants of
 figure~\\ref{tab:lock-levels} bitwise-or'ed with {\\sl exactly one}
 of the bitmask constants of figure~\\ref{tab:lock-modes} as parameter
 \\funarg{lock-mode}\\ or use {\\sl exactly one} of the
 shorthand bitmask constants explained in figure~\\ref{tab:lock-shands}.
 \\begin{figure}[htbp]\\centering%
 \\begin{tabular}{|l|l|}
 \\hline
 \\tabularheader{Constant}
	&\\tabularheader{Description}\\\\
 \\hline\\hline
 {\\bf +lock-element-read+}
  & Same as bitwise-or of {\\bf +lock-level-element+} with
    {\\bf +lock-mode-read+}.\\\\
 \\hline
 {\\bf +lock-vector-read+}
  & Same as bitwise-or of {\\bf +lock-level-vector+} with
    {\\bf +lock-mode-read+}.\\\\
 \\hline
 {\\bf +lock-element-write+}
  & Same as bitwise-or of {\\bf +lock-level-element+} with
    {\\bf +lock-mode-write+}.\\\\
 \\hline
 {\\bf +lock-vector-write+}
  & Same as bitwise-or of {\\bf +lock-level-vector+} with
    {\\bf +lock-mode-write+}.\\\\
 \\hline
 \\end{tabular}
 \\caption{Lock level and mode shorthands}\\label{tab:lock-shands}%
 \\end{figure}
 Furthermore, zero to all of the values shown
 in figure~\\ref{tab:lock-mods} can be bitwise-or'ed additionally into
 the \\funarg{lock-mode}\\ to modify the behavior of the called
 locking function.
 \\begin{figure}[htbp]\\centering%
 \\begin{tabular}{|l|l|}
 \\hline
 \\tabularheader{Constant}
	&\\tabularheader{Description}\\\\
 \\hline\\hline
 {\\bf +lock-unlock+}
  & Do not set a lock but instead remove a lock with passed
    level and mode.\\\\
 \\hline
 {\\bf +lock-force+}
  & Try to ignore any errors which occure at the locking operation
    as far as possible.\\\\
 \\hline
 \\end{tabular}
 \\caption{Lock modifier}\\label{tab:lock-mods}%
 \\end{figure} 

 There are also `intent' lock modes specified (constants
 {\\bf *lock-mode-\\ldots{}-intent*}), but these lock modes
 are used internally by \\plob. An `intent' lock placed on
 a certain level onto a persistent object means that on one
 of the lower levels someone has placed a corresponding
 `real' lock, e.g.\\ a mode of
 {\\bf +lock-mode-read-intent+} on level
 {\\bf +lock-level-vector+} means that anyone has placed a
 mode {\\bf +lock-mode-read+} on the sub-level
 {\\bf +lock-level-element+} of the persistent object.

\\Remarkslabel

 \\begin{itemize}

 \\item
  When a persistent object is loaded into its transient
  representation, the object should be read locked by the
  persistent object contained in \\fcite{*root-persistent-heap*}.
  The idea is that one \\lw\\ process (in the sense of a
  \\unix\\ process) represented just by {\\bf *root-persistent-heap*}
  shares all transient representations of
  all persistent objects which have been loaded so far.
  Another session which wants to load the same (i.e.\\ \\lisp{eq})
  object too would then not load the object a second time
  (because there is already a read lock on the object, see
  \\fcite{with-read-lock}).

  In the moment, this is not done; loading the same (i.e.\\ \\lisp{eq})
  transient object at the same time by more than one session can lead
  to creation of more than one object in transient memory and to
  unnecessary multiple read operations of the object's state.

 \\item
  For multiuser handling, a non-conflicting lock mode `cached'
  should be defined; then, a session which placed a `cached' lock
  onto a persistent object should be notified when anyone
  changed the state of the persistent object so that the session
  holding the `cached' lock can update the transient representation
  of the persistent object in its cache.

\\end{itemize}

\\Seealsolabel

 \\Fcite{with-read-lock};
 \\fcite{with-write-lock};
 \\fcite{read-lock};
 \\fcite{write-lock};
 \\fcite{p-insert-lock};
 \\fcite{p-set-lock};
 \\fcite{p-unlock};
 \\fcite{p-unlock-all};
 \\fcite{p-unlock-all-all};
 \\fcite{*suspend-timeout*}.")

;;; ---------------------------------------------------------------------------
(defun p-set-lock (p-heap p-objid
			  &optional (expecting-type-tag +null-type-tag+)
			  (lock-mode +lock-vector-read+)
			  (at-location -1))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isatypetag{\\funarg{expecting-type-tag}}
 \\isa{\\funarg{lock-mode}}
      {a fixnum bitwise-or'ed from
       one of the constant values of
       {\\bf *lock-level-\\ldots{}*} (figure~\\ref{tab:lock-levels}%
       %% 1998/01/07 HK: This makes problems with hyperref:
       %% \\fcitepage{\\pageref{tab:lock-levels}}%
       ) with
       one of the constant values of {\\bf *lock-mode-\\ldots{}*}
       (figure~\\ref{tab:lock-modes}%
       %% 1998/01/07 HK: This makes problems with hyperref:
       %% \\fcitepage{\\pageref{tab:lock-modes}}
       )}
 \\isa{\\funarg{at-location}}
      {a fixnum}
\\Valueslabel
 \\retoldmode{\\funarg{p-heap}}{\\funarg{p-objid}}
\\Purposelabel
 Set a lock with the level and mode of \\funarg{lock-mode}\\ to
 \\funarg{p-objid}\\ or, if the passed \\funarg{lock-mode}\\ requests
 an `element'-level lock, to the slot of the persistent object
 referenced by \\funarg{p-objid}\\ at position \\funarg{at-location}.
 If there is already such a lock, the lock counter for the passed
 \\funarg{lock-mode}\\ is incremented.
\\Remarkslabel
 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-objid}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 If the passed \\funarg{lock-mode}\\ requests an `element'-level lock,
 it is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{p-insert-lock};
 \\fcite{p-unlock};
 \\fcite{p-unlock-all};
 \\fcite{p-unlock-all-all};
 \\fcite{sh-set-lock}."

  (declare (type fixnum lock-mode at-location))
  (sh-set-lock (persistent-object-objid p-heap)
	       (persistent-object-objid p-objid)
	       expecting-type-tag
	       lock-mode
               at-location))

;;; ---------------------------------------------------------------------------
(defun p-insert-lock (p-heap
		      p-objid
		      &optional (expecting-type-tag +null-type-tag+)
		      (lock-mode +lock-vector-read+)
		      (at-location -1))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isatypetag{\\funarg{expecting-type-tag}}
 \\isa{\\funarg{lock-mode}}
      {a fixnum bitwise-or'ed from
       one of the constant values of
       {\\bf *lock-level-\\ldots{}*}
       (figure~\\ref{tab:lock-levels}%
       %% 1998/01/07 HK: This makes problems with hyperref:
       %% \\fcitepage{\\pageref{tab:lock-levels}}
       ) with
       one of the constant values of {\\bf *lock-mode-\\ldots{}*}
       (figure~\\ref{tab:lock-modes}%
       %% 1998/01/07 HK: This makes problems with hyperref:
       %% \\fcitepage{\\pageref{tab:lock-modes}}
       )}
 \\isa{\\funarg{at-location}}
      {a fixnum}
\\Valueslabel
 \\retoldmode{\\funarg{p-heap}}{\\funarg{p-objid}}
\\Purposelabel
 If there is no lock as requested by \\funarg{lock-mode}\\ set
 to \\funarg{p-objid},
 set a lock with the level and mode of \\funarg{lock-mode}\\ to
 \\funarg{p-objid}\\ or, if the passed \\funarg{lock-mode}\\ requests
 an `element'-level lock, to the slot of the persistent object
 referenced by \\funarg{p-objid}\\ at position \\funarg{at-location}.
\\Remarkslabel
 If \\funarg{expecting-type-tag}\\ is not \\lisp{equal}\\ to
 \\lisp{+null-type-tag+}, it is checked if
 \\funarg{p-objid}\\ references a persistent object of type
 \\funarg{expecting-type-tag}.

 If the passed \\funarg{lock-mode}\\ requests an `element'-level lock,
 it is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{p-set-lock};
 \\fcite{sh-insert-lock}."

  (declare (type fixnum lock-mode at-location))
  (let ((old-lock-mode (sh-insert-lock (persistent-object-objid p-heap)
		                       (persistent-object-objid p-objid)
		                       expecting-type-tag
		                       lock-mode
		                       at-location)))
    (cond
     ((>= old-lock-mode 0)
      old-lock-mode)
     ((and (<= (logand lock-mode +lock-level-mask+) +lock-level-vector+)
           (p-read-only p-objid (persistent-object-objid p-heap)))
      nil)
     ((<= +lock-conflict-first+ old-lock-mode +lock-conflict-last+)
      (handle-lock-conflict
       #'(lambda (p-heap p-objid)
	   (sh-insert-lock (persistent-object-objid p-heap)
			   (persistent-object-objid p-objid)
			   expecting-type-tag
			   lock-mode
			   at-location))
       (persistent-object-objid p-heap) p-objid old-lock-mode))
     (t (handle-lock-error p-heap p-objid old-lock-mode)))))

;;; ---------------------------------------------------------------------------
(defun (setf p-read-only) (read-only-p
			   p-objid
			   &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabool{\\funarg{read-only-p}}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{read-only-p}}
\\Purposelabel
 For {\\funarg{read-only-p}}\\ being \\lispnil, the read-only lock
 is removed from the persistent object referenced by \\funarg{p-objid};
 otherwise, the persistent object referenced by \\funarg{p-objid}\\ is
 locked read-only.
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{p-read-only};
 \\fcite{sh-read-only}."

  (sh-read-only (persistent-object-objid p-heap)
                (persistent-object-objid p-objid)
                (if read-only-p
                    +read-only+
                  +read-write+))
  read-only-p)

;;; ---------------------------------------------------------------------------
(defun p-read-only (p-objid
		    &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ if the persistent object referenced by
 \\funarg{p-objid}\\ is locked read-only; \\lispt\\ otherwise.
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{(setf p-read-only)};
 \\fcite{sh-read-only}."

  (sh-read-only (persistent-object-objid p-heap)
                (persistent-object-objid p-objid) +read-only-p+))

;;; ---------------------------------------------------------------------------
(defun p-unlock (p-heap p-objid
			&optional
			(lock-mode (logior +lock-vector-read+ +lock-force+))
			(at-location -1))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{lock-mode}}
      {a fixnum bitwise-or'ed from
       one of the constant values of
       {\\bf *lock-level-\\ldots{}*} (figure~\\ref{tab:lock-levels}%
       %% 1998/01/07 HK: This makes problems with hyperref:
       %% \\fcitepage{\\pageref{tab:lock-levels}}
       ) with
       one of the constant values of {\\bf *lock-mode-\\ldots{}*}
       (figure~\\ref{tab:lock-modes}%
       %% 1998/01/07 HK: This makes problems with hyperref:
       %% \\fcitepage{\\pageref{tab:lock-modes}}
       )}
 \\isa{\\funarg{at-location}}
      {a fixnum}
\\Valueslabel
 \\retoldmode{\\funarg{p-heap}}{\\funarg{p-objid}}
\\Purposelabel
 Remove the lock with the level and mode of
 \\funarg{lock-mode}\\ held by \\funarg{p-heap}\\ from
 \\funarg{p-objid}\\ or, if the passed \\funarg{lock-mode}\\ requests
 an `element'-level lock, from the slot of the persistent object
 referenced by \\funarg{p-objid}\\ at position \\funarg{at-location}.
\\Remarkslabel
 If the passed \\funarg{lock-mode}\\ requests an `element'-level unlock,
 it is checked if \\funarg{at-location}\\ is a valid position index
 for the persistent object referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{p-unlock-all};
 \\fcite{p-unlock-all-all};
 \\fcite{p-set-lock};
 \\fcite{p-insert-lock};
 \\fcite{sh-unlock}."

  (declare (type fixnum lock-mode at-location))
  (sh-unlock (persistent-object-objid p-heap)
	     (persistent-object-objid p-objid)
	     lock-mode
	     at-location))

;;; ---------------------------------------------------------------------------
(defun p-unlock-all (p-heap p-objid)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retoldmode{\\funarg{p-heap}}{\\funarg{p-objid}}
\\Purposelabel
 Remove all locks held by \\funarg{p-heap}\\ from
 the persistent object referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{p-unlock};
 \\fcite{p-unlock-all-all};
 \\fcite{sh-unlock-all}."

  (sh-unlock-all (persistent-object-objid p-heap)
	         (persistent-object-objid p-objid)))

;;; ---------------------------------------------------------------------------
(defun p-unlock-all-all (p-objid
			 &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 Returns the number of locks removed.
\\Purposelabel
 Remove all locks held by any object from \\funarg{p-objid}.
 This is a brute-force `emergency exit' if \\funarg{p-objid}\\ is
 locked by another unknown object and this lock blocks any other
 lock requests. Calling this function for \\funarg{p-objid}\\ enables
 locking of the object again.
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{p-unlock};
 \\fcite{p-unlock-all};
 \\fcite{with-handle-lock-conflict};
 \\fcite{sh-unlock-all-all}."

  (sh-unlock-all-all (persistent-object-objid p-heap)
                     (persistent-object-objid p-objid)))

;;; --- Transactions on persistent heaps --------------------------------------

(defun begin-transaction (&optional ignore-error
                                    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabool{\\funarg{ignore-error}}
\\Valueslabel
 Returns a numeric transaction ID iff a transaction was started
 on \\funarg{p-heap}, \\lispnil\\ otherwise.
\\Purposelabel
 Start a transaction on \\funarg{p-heap}.

 All subsequent lock requests and state changes done on persistent
 objects in the `scope' (i.e.\\ \\cl\\ session, see
 \\fcite{*default-persistent-heap*}) of \\funarg{p-heap}\\ are
 written to the transaction log of \\funarg{p-heap}. When the transaction
 on \\funarg{p-heap}\\ is cancelled, all changed objects are
 brought back to their states at the start of the transaction. When
 the transaction on \\funarg{p-heap}\\ is cancelled or ended,
 all lock requests are removed from the objects locked in the
 `scope' of \\funarg{p-heap}.
\\Remarkslabel
 If there is already an active transaction and
 \\funarg{ignore-error}\\ is \\lispnil, an error is signalled;
 if there is already an active transaction and
 \\funarg{ignore-error}\\ is \\nonnil, {\\bf begin-transaction}
 simply returns \\lispnil.
\\Seealsolabel
 \\Fcite{with-transaction};
 \\fcite{cancel-transaction};
 \\fcite{end-transaction};
 \\fcite{in-transaction-p};
 \\fcite{*default-persistent-heap*};
 \\fcite{sh-begin-transaction}."

  (declare (inline begin-transaction))
  (unless (and (persistent-heap-in-transaction p-heap) ignore-error)
    (assert-open-session-p p-heap)
    (let ((transaction-id
	   (sh-begin-transaction (persistent-object-objid p-heap)
				 ignore-error)))
      (setf (persistent-heap-in-transaction p-heap) transaction-id)
      transaction-id)))

;;; ---------------------------------------------------------------------------
(defun cancel-transaction (&optional ignore-error
                                     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabool{\\funarg{ignore-error}}
\\Valueslabel
 Returns the numeric transaction ID of the cancelled transaction
 of \\funarg{p-heap}\\ iff it was cancelled, \\lispnil\\ otherwise.
\\Purposelabel
 Cancel the current transaction and initiates a rollback.
\\Remarkslabel
 If there is no active transaction and
 \\funarg{ignore-error}\\ is \\lispnil, an error is signalled;
 if there is no active transaction and
 \\funarg{ignore-error}\\ is \\nonnil, {\\bf cancel-transaction}
 simply returns \\lispnil.

 Since cancelling a transaction sets the involved persistent
 objects back to its state at the start of the transaction,
 the transient representations of the persistent objects
 should be rolled-back too; this is not yet implemented.
 Instead, the root caches are cleared by a call to
 \\fcite{clear-cache}\\ which will cause a complete
 re-creation of the transient representations of the persistent
 objects when referenced next time.
\\Seealsolabel
 \\Fcite{begin-transaction};
 \\fcite{end-transaction};
 \\fcite{in-transaction-p};
 \\fcite{clear-cache};
 \\fcite{sh-cancel-transaction}."

  (setf (persistent-heap-in-transaction p-heap) nil)
  (clear-cache)
  (sh-cancel-transaction (persistent-object-objid p-heap)
			 ignore-error))

;;; ---------------------------------------------------------------------------
(defun end-transaction (&optional ignore-error
                                  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabool{\\funarg{ignore-error}}
\\Valueslabel
 Returns the numeric transaction ID of the ended transaction
 of \\funarg{p-heap}\\ iff it was ended, \\lispnil\\ otherwise.
\\Purposelabel
 End a transaction on \\funarg{p-heap}; commit all changes done
 since the start of the transaction.
\\Remarkslabel
 If there is no active transaction and
 \\funarg{ignore-error}\\ is \\lispnil, an error is signalled;
 if there is no active transaction and
 \\funarg{ignore-error}\\ is \\nonnil, {\\bf end-transaction}
 simply returns \\lispnil.
\\Seealsolabel
 \\Fcite{begin-transaction};
 \\fcite{cancel-transaction};
 \\fcite{in-transaction-p};
 \\fcite{sh-end-transaction}."

  (when (persistent-heap-p p-heap)
    (setf (persistent-heap-in-transaction p-heap) nil))
  (sh-end-transaction (persistent-object-objid p-heap) ignore-error))

;;; ---------------------------------------------------------------------------
(defun in-transaction-p (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Valueslabel
 Returns the numeric transaction ID iff there is an active transaction
 on \\funarg{p-heap}, \\lispnil\\ otherwise.
\\Purposelabel
 Check if \\funarg{p-heap}\\ is in an active transaction.
\\Seealsolabel
 \\Fcite{begin-transaction};
 \\fcite{cancel-transaction};
 \\fcite{end-transaction};
 \\fcite{sh-in-transaction-p}."

  (let ((p-objid (persistent-object-objid p-heap)))
    (declare (type fixnum p-objid)
             (inline in-transaction-p))
    (setf (persistent-heap-in-transaction p-heap)
          (when p-objid
	    (sh-in-transaction-p p-objid
				 +null-transaction-id+)))))

;;; ---------------------------------------------------------------------------
(defmacro with-transaction ((&optional (p-heap '*default-persistent-heap*))
			    &rest forms)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{forms}}
      {an implicit \\lisp{progn}\\ expression}
\\Valueslabel
 Returns the value of evaluating \\funarg{forms}.
\\Purposelabel
 Embed the evaluation of \\funarg{forms}\\ into an active transaction:

 If there is no transaction running on \\funarg{p-heap},
 a transaction is started, \\funarg{forms}\\ are evaluated and the
 transaction is ended if no error occured at evaluating
 \\funarg{forms}\\ or cancelled if at least one error occured at
 evaluating \\funarg{forms}.

 If there is already an active transaction,
 only \\funarg{forms}\\ are evaluated.

\\Remarkslabel
 The user of this macro promises implicitly that before and after
 evaluating the \\funarg{forms}\\ the \\sh\\ is in a consistent
 state. Only during evaluating \\funarg{forms}\\ a temporary
 inconsistent \\sh\\ state may arise.

\\Seealsolabel
 \\Fcite{begin-transaction};
 \\fcite{cancel-transaction};
 \\fcite{end-transaction}."

  (let ((ph (gensym "P-HEAP-"))
        (tr-id (gensym "TR-ID-"))
        (fn (gensym "FN-"))
        (done (gensym "DONE-"))
        (result (gensym "RESULT-")))
    `(flet ((,fn () (progn ,@forms)))
      (let* ((,ph ,p-heap)
	     (,tr-id (begin-transaction t ,ph)))
	#-:lispworks4 ;; and hopefully not later
	(declare (dynamic-extent ,ph ,tr-id))
	(if ,tr-id
	    (let ((,done nil)
		  (,result nil))
	      #-:lispworks4 ;; and hopefully not later
	      (declare (dynamic-extent ,done))
	      (unwind-protect
		  (progn
		    (setf ,result (multiple-value-list (,fn)))
		    (setf ,done t))
		(if ,done
                    (end-transaction nil ,ph)
		  (progn
		    (cancel-transaction t ,ph)
		    (setf ,result nil))))
	      (values-list ,result))
	  (,fn))))))

;;; --- Registering of objects to persistent heaps caches & tables ------------

(defun register-to-cache (p-objid t-object &optional clobber-old-entry)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isanobject{\\funarg{t-object}}
 \\isabool{\\funarg{clobber-old-entry}}
\\Valueslabel
 Iff \\funarg{p-objid}\\ and \\funarg{t-object}\\ are newly registered,
 \\nonnil\\ is returned, \\lispnil\\ otherwise.
\\Purposelabel
 Associates the persistent object referenced by
 \\funarg{p-objid}\\ as having currently a transient
 representation of \\funarg{t-object}\\ and vice versa;
 this association is done by registering \\funarg{p-objid}\\ and
 \\funarg{t-object} into the root caches.
\\Seealsolabel
 \\Fcite{*root-persistent-heap-object->objid-cache*};
 \\fcite{is-registered-objid};
 \\fcite{*root-persistent-heap-objid->object-cache*};
 \\fcite{is-registered-object}."

  (let* ((new-objid (persistent-object-objid p-objid))
	 (old-objid
          (gethash t-object *root-persistent-heap-object->objid-cache*)))
    (multiple-value-bind (old-object old-object-p)
	(gethash new-objid *root-persistent-heap-objid->object-cache*)
      (when (and old-object-p (not (eq t-object old-object)))
	;; The t-object was already registered and t-object and old-object
	;; are not eq; remove old-object from the cache:
	(when (and (null clobber-old-entry) *verbose* (>= *verbose* 1))
	  (cerror "Replace the old object by the new object."
		  "Under objid ~A there is already a registered object ~A; ~
                   object to register is ~A."
		  new-objid old-object t-object))
	(remhash old-object *root-persistent-heap-object->objid-cache*))
      (when (and old-objid
		 (not (= new-objid old-objid)))
	;; The p-objid was already registered and new-objid and old-objid
	;; are not equal; remove old-objid from the cache:
	(when (and (null clobber-old-entry) *verbose* (>= *verbose* 1))
	  (cerror "Replace the old objid by the new objid."
		  "Object ~A is already registered with objid ~A; ~
                   new objid is ~A."
		  t-object old-objid new-objid))
	(remhash old-objid *root-persistent-heap-objid->object-cache*))
      (setf (gethash new-objid *root-persistent-heap-objid->object-cache*)
	t-object)
      (setf (gethash t-object *root-persistent-heap-object->objid-cache*)
	new-objid)
      (not old-objid))))

;;; ---------------------------------------------------------------------------
(defun register-to-base-cache (p-objid t-object &optional clobber-old-entry)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isanobject{\\funarg{t-object}}
 \\isabool{\\funarg{clobber-old-entry}}
\\Valueslabel
 See \\fcite{register-to-cache}.
\\Purposelabel
 At first, see \\fcite{register-to-cache}.

 Additionally, the persistent object referenced by
 \\funarg{p-objid}\\ and its transient representation given by
 \\funarg{t-object}\\ are registered as being \\plob\\ base objects
 which are always found in the cache, even after a call to
 \\fcite{clear-cache}. This is done to prevent loading these
 objects and their refrenced objects more than once from the
 \\sh, either of problems with endless recursion of
 self-describing persistent objects or of efficency reasons
 (because base objects are used very often and should be loaded
 always from the cache).
\\Seealsolabel
 \\Fcite{register-to-cache}."

  (if (registered-objid-p p-objid)
      (rplacd (assoc (persistent-object-objid p-objid) *plob-base-objects*)
              t-object)
    (push (cons (persistent-object-objid p-objid) t-object)
          *plob-base-objects*))
  (register-to-cache p-objid t-object clobber-old-entry))

;;; ---------------------------------------------------------------------------
(defun registered-objid-p (p-objid)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 Returns two values. If there is a transient object associated to
 \\funarg{p-objid}, \\funarg{p-objid}\\ is returned as first
 and the transient object as second value. If there is no association
 to \\funarg{p-objid}, both values returned ar \\lispnil.
\\Purposelabel
 Check if \\funarg{p-objid}\\ is associated to a transient object.
\\Seealsolabel
 \\Fcite{register-to-cache};
 \\fcite{is-registered-objid};
 \\fcite{is-registered-object}."

  (declare (inline registered-objid-p))
  (let ((numeric-objid (persistent-object-objid p-objid)))
    (multiple-value-bind (t-object t-object-p)
	(gethash numeric-objid *root-persistent-heap-objid->object-cache*)
      (if t-object-p
	  (values numeric-objid t-object)
	(values nil nil)))))

;;; ---------------------------------------------------------------------------
(defun is-registered-objid (p-objid)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns the transient object which is associated to \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{register-to-cache};
 \\fcite{registered-objid-p};
 \\fcite{is-registered-object}."

  (declare (inline is-registered-objid))
  (gethash (persistent-object-objid p-objid)
	   *root-persistent-heap-objid->object-cache*))

;;; ---------------------------------------------------------------------------
(defun is-registered-object (t-object)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-object}}
\\Purposelabel
 Returns the \\objid\\ of the transient object
 \\funarg{t-object}\\ if found in the cache,
 \\lispnil\\ otherwise.
\\Seealsolabel
 \\Fcite{register-to-cache};
 \\fcite{is-registered-objid};
 \\fcite{registered-objid-p}."

  (declare (inline is-registered-object))
  (gethash t-object *root-persistent-heap-object->objid-cache*))

;;; ---------------------------------------------------------------------------
(defun unregister-by-objid (p-objid)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 Returns \\nonnil\\ if the there was an oject found registered under
 \\funarg{p-objid}\\ and removed from the root caches;
 \\lispnil\\ otherwise.
\\Purposelabel
 Remove the transient object associated to \\funarg{p-objid}\\ from
 the root caches.
\\Seealsolabel
 \\Fcite{is-registered-objid};
 \\fcite{unregister-by-object}."

  (let* ((old-objid (persistent-object-objid p-objid)))
    (multiple-value-bind (old-object old-object-p)
	(gethash old-objid *root-persistent-heap-objid->object-cache*)
      (when old-object-p
	(remhash old-object *root-persistent-heap-object->objid-cache*)
	(remhash old-objid *root-persistent-heap-objid->object-cache*)))))

;;; ---------------------------------------------------------------------------
(defun unregister-by-object (t-object)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-object}}
\\Valueslabel
 Returns \\nonnil\\ if \\funarg{t-oject}\\ was registered in
 and removed from the root caches; \\lispnil\\ otherwise.
\\Purposelabel
 Remove the transient object \\funarg{t-object}\\ from
 the root caches.
\\Seealsolabel
 \\Fcite{is-registered-object};
 \\fcite{unregister-by-objid}."

  (let* ((old-objid
          (gethash t-object *root-persistent-heap-object->objid-cache*)))
    (when old-objid
      (remhash t-object *root-persistent-heap-object->objid-cache*)
      (remhash old-objid *root-persistent-heap-objid->object-cache*))))

;;; --- Flush and clear caches of persistent objects --------------------------

(defun flush-object (p-object
		     &optional remove-from-cache-p
			       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 \\Purposelabel
  Flush the object referenced by \\funarg{p-object}.
  If \\funarg{p-object}\\ is \\lispnil, all objects cached for
  \\funarg{p-heap}\\ will be flushed.
 \\Seealsolabel
  \\Fcite{sh-flush-object}."
  (sh-flush-object (when p-heap (persistent-object-objid p-heap))
		   (when p-object (persistent-object-objid p-object))
		   remove-from-cache-p))

;;; --- Flush and clear caches of persistent heaps ----------------------------

(defun plob-flush (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Flush any pending \\sh\\ write requests. After returning,
 it is guranteed that the \\sh\\ is saved to disk.
\\Remarkslabel
 This function should of course be called everytime after a change
 done to a persistent object's state; but since flushing is very
 slow, it has to be called either when the system is idle (see
 \\fcite{flush-mode}) or interactively by the user.
\\Seealsolabel
 \\Fcite{flush-mode};
 \\fcite{sh-stabilise}."

  (when (sh-open-p)
      (sh-stabilise (persistent-object-objid p-heap)))
  p-heap)

;;; ---------------------------------------------------------------------------
(defun clear-cache ()
  #+:lisp-doc "
\\Purposelabel
 Clear the root caches;
 only the base objects remain in the root caches.

 This clearing removes the associations done with
 {\\bf register-to-cache} from an \\objid\\ to its transient
 representation and vice versa; so re-loading a persistent
 object will force a creation of a new transient representation
 compared to the one before the caches were cleared,
 i.e.\\ in this case the identity of the transient representation
 will change.
\\Seealsolabel
 \\Fcite{register-to-cache};
 \\fcite{register-to-base-cache}."

  (let ((table (cached-btree-key->data-cache *symbol->class-table*)))
    (if table
	(clrhash table)
      (setf (cached-btree-key->data-cache *symbol->class-table*)
	    (make-hash-table :test 'eq))))
  (let ((table (cached-btree-key->data-cache *name->package-table*)))
    (if table
	(clrhash table)
      (setf (cached-btree-key->data-cache *name->package-table*)
	    (make-hash-table :test 'equal))))
  (when *hash-table->persistent-hash-table*
    (clrhash *hash-table->persistent-hash-table*))
  (when *instance->data-vector-cache*
    (clrhash *instance->data-vector-cache*))
  (clrhash *root-persistent-heap-objid->object-cache*)
  (clrhash *root-persistent-heap-object->objid-cache*)
  (dolist (o *plob-base-objects*)
    (register-to-cache (car o) (cdr o)))
  (flush-object nil t)
  *root-persistent-heap*)

;;; ---------------------------------------------------------------------------
(defun invalidate-all-globals ()
  #+:lisp-doc "
\\Purposelabel
 Invalidate all global variables. This marks the \\sh\\ as
 `logically closed'.
\\Seealsolabel
 \\Fcite{close-heap}."

  (labels ((invalidate-subclasses
            (the-class)
            (setf (class-description-of the-class) nil)
            (map nil
                 #'invalidate-subclasses
                 (class-direct-subclasses the-class))))

    (setf *root-persistent-heap-objid* nil)
    (setf (persistent-object-objid *root-persistent-heap*)
	  *root-persistent-heap-objid*)
    (setf (persistent-object-objid *default-persistent-heap*) nil)
    (setf *root* (make-persistent-object-internal))
    (setf (persistent-btree-objid *name->package-table*)
          nil)
    (setf (persistent-btree-objid *symbol->class-table*)
	  nil)
    (setf *plob-base-objects* ())
    (setf *structure-description* nil)
    (setf *structure-slot-description-objid* nil)
    (setf *structure-slot-description* nil)
    (setf *package-description-objid* nil)
    (setf *package-description* nil)
    (setf *lisproot-description-objid* nil)
    (setf *lisproot-description* nil)
    (setf *plob-description-objid* nil)
    (setf *plob-description* nil)
    (setf *class-description-objid* nil)
    (setf *class-description* nil)
    (setf *slot-description-objid* nil)
    (setf *slot-description* nil)
    (setf *direct-slot-description-objid* nil)
    (setf *direct-slot-description* nil)
    (setf *effective-slot-description-objid* nil)
    (setf *effective-slot-description* nil)
    (setf *method-description-objid* nil)
    (setf *method-description* nil)
    (when +persistent-clos-object-class+
      (invalidate-subclasses +persistent-clos-object-class+))
    (values)))

;;; --- Storing and loading of objects to and from persistent heaps -----------

(defun read-lock (p-heap p-objid depth expecting-type-tag
		  &optional force-read (lock-mode +lock-vector-read+)
			    (at-location -1))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isatypetag{\\funarg{expecting-type-tag}}
 \\isa{\\funarg{lock-mode}}
      {a fixnum bitwise-or'ed from
       one of the constant values of
       {\\bf *lock-level-\\ldots{}*} (figure~\\ref{tab:lock-levels}%
       %% 1998/01/07 HK: This makes problems with hyperref:
       %% \\fcitepage{\\pageref{tab:lock-levels}}
       ) with
       the constant value of {\\bf +lock-mode-read+}}
\\Valueslabel
 If there was already a read-lock placed  by
 \\funarg{p-heap}\\ onto the persistent object
 referenced by \\funarg{p-objid}, \\lispnil\\ is returned;
 \\nonnil\\ otherwise.
\\Purposelabel
 Set a read lock onto the persistent object referenced by
 \\funarg{p-objid}; the level and mode of locking
 are determined by \\funarg{lock-mode}\\ (see section
 \\fcite{locking ...}\\ for details).
\\Remarkslabel
 \\begin{enumerate}

 \\item This function can only be executed within an active
  transaction on \\funarg{p-heap}, so e.g.\\ embed a call to it
  into an expansion of \\fcite{with-transaction}\\ if appropiate or
  make otherwise sure that at call-time there is an active
  transaction.

 \\item Do not call this function direct; use the
  \\fcite{with-read-lock}\\ instead.

 \\end{enumerate}
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{with-transaction};
 \\fcite{with-read-lock};
 \\fcite{write-lock}."

  (declare (type fixnum expecting-type-tag lock-mode at-location)
           (ignore depth)
           (inline read-lock))
  (let ((old-lock-mode (p-insert-lock p-heap p-objid
				      expecting-type-tag
				      lock-mode
				      at-location)))
    #-:lispworks4 ;; and hopefully not later
    (declare (dynamic-extent old-lock-mode))
    (or force-read
	(and old-lock-mode
	     (or (/= (logand old-lock-mode +lock-level-mask+)
		     +lock-level-vector+)
		 (=  (logand old-lock-mode +lock-mode-read+) 0))))))

;;; ---------------------------------------------------------------------------
(defun read-lock-store (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 Put a read lock onto the whole database. This will speed up object
 locking, but care must be taken since the locking scheme can't be used
 any longer to detect if an object was already read."
  (read-lock p-heap +null-objid+ :nocache +null-type-tag+
             nil +lock-store-read+))

;;; ---------------------------------------------------------------------------
(defun write-lock (p-heap p-objid t-object depth
		   expecting-type-tag
		   &optional force-write (lock-mode +lock-vector-write+)
			     (at-location -1))

  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isatypetag{\\funarg{expecting-type-tag}}
 \\isa{\\funarg{lock-mode}}
      {a fixnum bitwise-or'ed from
       one of the constant values of
       {\\bf *lock-level-\\ldots{}*} (figure~\\ref{tab:lock-levels}%
       %% 1998/01/07 HK: This makes problems with hyperref:
       %% \\fcitepage{\\pageref{tab:lock-levels}}
       ) with the constant value of {\\bf +lock-mode-write+}}
\\Valueslabel
 If there was already a write-lock placed  by
 \\funarg{p-heap}\\ onto the persistent object
 referenced by \\funarg{p-objid}, \\lispnil\\ is returned;
 \\nonnil\\ otherwise.
\\Purposelabel
 Set a write lock onto the persistent object referenced by
 \\funarg{p-objid}; the level and mode of locking
 are determined by \\funarg{lock-mode}\\ (see section
 \\fcite{locking ...}\\ for details).
\\Remarkslabel
 \\begin{enumerate}

 \\item This function can only be executed within an active
  transaction on \\funarg{p-heap}, so e.g.\\ embed a call to it
  into an expansion of \\fcite{with-transaction}\\ if appropiate or
  make otherwise sure that at call-time there is an active
  transaction.

 \\item Do not call this function direct; use the
  \\fcite{with-write-lock}\\ instead.

 \\end{enumerate}
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{with-transaction};
 \\fcite{with-write-lock};
 \\fcite{read-lock}."

  (declare (type fixnum lock-mode at-location)
           (inline write-lock))
  (let ((old-lock-mode
	 (p-insert-lock p-heap p-objid expecting-type-tag lock-mode
			at-location)))
    #-:lispworks4 ;; and hopefully not later
    (declare (dynamic-extent old-lock-mode))
    (when (and (or force-write old-lock-mode)
	       (not (eq depth :nocache)))
      (register-to-cache p-objid t-object))
    (or force-write
	(and old-lock-mode
	     (or (/= (logand old-lock-mode +lock-level-mask+)
		     +lock-level-vector+)
		 (=  (logand old-lock-mode +lock-mode-write+) 0))))))

;;; ---------------------------------------------------------------------------
(defun write-lock-store (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 Put a write lock onto the whole database. This will speed up object
 locking, but care must be taken since the locking scheme can't be used
 any longer to detect if an object was already stored."
  (write-lock p-heap +null-objid+ nil :nocache +null-type-tag+
              nil +lock-store-write+))

;;; ---------------------------------------------------------------------------
(defmacro with-direct-representation ((&optional (depth :object))
				      &rest forms)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{forms}}
      {an implicit \\lisp{progn}\\ expression}
\\Valueslabel
 Returns the value of evaluating \\funarg{forms}.
\\Purposelabel
 The \\funarg{forms}\\ are evaluated in a context which references
 persistent objects directly; no transient representations will be
 generated for persistent objects loaded from the database.
\\Seealsolabel
 \\Fcite{with-transient-representation}."

  `(let ((*default-depth* ,depth))
     (progn ,@forms)))

;;; ---------------------------------------------------------------------------
(defmacro with-transient-representation ((&optional (depth :cached))
					 &rest forms)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{forms}}
      {an implicit \\lisp{progn}\\ expression}
\\Valueslabel
 Returns the value of evaluating \\funarg{forms}.
\\Purposelabel
 The \\funarg{forms}\\ are evaluated in a context which references
 persistent objects indirectly by thier transient representation;
 so, transient representations will be generated for persistent
 objects loaded from the database.
\\Seealsolabel
 \\Fcite{with-direct-representation}."

  `(let ((*default-depth* ,depth))
     (progn ,@forms)))

;;; ---------------------------------------------------------------------------
(defgeneric store-object (t-object &optional (depth *default-depth*)
                                   (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "

\\Argumentslabel
 \\isanobject{\\funarg{t-object}}
 \\isa{\\funarg{depth}}
      {a keyword symbol}
\\Valueslabel
 The \\objid\\ referencing the persistent representation of
 \\funarg{t-object}\\ is returned.
\\Purposelabel
 Store \\funarg{t-object}\\ to the \\sh.
 Argument \\funarg{depth}\\ determines how `deep' the object is
 stored and can take one of the following values:
 \\begin{description}

 \\item[\\lisp{:object}, \\lisp{:objid}]
  Interpret \\funarg{t-object}\\ as an already persistent object.

 \\item[\\lisp{:cached}]
  Store only new allocated or immediate sub-objects.

 \\item[\\lisp{:flat}]
   Store only the top level of \\funarg{t-object}.

 \\item[\\lisp{:deep}]
   Store all subobjects.

 \\end{description}
 The default value for \\funarg{depth}\\ is defined in the
 \\fcite{*default-depth*}.
\\Seealsolabel
 \\Fcite{load-object}."))

;;; ---------------------------------------------------------------------------
(defmethod store-object ((t-object persistent-clos-object)
			 &optional (depth *default-depth*)
			 (p-heap *default-persistent-heap*))
  #+:lisp-doc "Calls to this method are trapped to \\fcite{t-object-to-p-objid}."
  (assert-open-session-p p-heap)
  (t-object-to-p-objid t-object depth p-heap)
  t-object)

;;; ---------------------------------------------------------------------------
(defmethod store-object (t-object &optional (depth *default-depth*)
                                  (p-heap *default-persistent-heap*))
  #+:lisp-doc "Calls to this method are trapped to \\fcite{t-object-to-p-objid}.
 The numeric \\objid\\ received from {\\bf t-object-to-p-objid}
 is put into an instance of \\fcite{persistent-object}\\ and returned;
 see also \\fcite{make-persistent-object}."
  (assert-open-session-p p-heap)
  (multiple-value-bind (stored-object stored-type-tag)
      (t-object-to-p-objid t-object depth p-heap)
    #-:lispworks4 ;; and hopefully not later
    (declare (dynamic-extent stored-object stored-type-tag))
    (if stored-object
        (make-persistent-object stored-object stored-type-tag)
      t-object)))

;;; ---------------------------------------------------------------------------
(defun load-object (p-object
		    &optional (depth *default-depth*)
                    (p-heap *default-persistent-heap* p-heapp))
		    
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-object}}
 \\isa{\\funarg{depth}}
      {a keyword symbol}
\\Valueslabel
 The transient representation of the persistent object referenced
 by \\funarg{p-object}\\ is returned.
\\Purposelabel
 Load the persistent object referenced by
 \\funarg{p-object}\\ from the \\sh.
 Argument \\funarg{depth}\\ determines how `deep' the object is
 loaded and can take one of the following values:
 \\begin{description}

 \\item[\\lisp{:object}, \\lisp{:objid}]
  Return a reference to persistent object \\funarg{p-object}.

 \\item[\\lisp{:cached}]
  Load only new allocated or immediate sub-objects.

 \\item[\\lisp{:flat}]
  Load only the top level of \\funarg{t-object}.

 \\item[\\lisp{:deep}]
  Load all subobjects.

 \\end{description}
 The default value for \\funarg{depth}\\ is defined in the
 \\fcite{*default-depth*}.
\\Seealsolabel
 \\Fcite{store-object}."

  (assert-open-session-p p-heap)
  (p-objid-to-t-object p-object
		       +short-objid-tag+
		       depth
		       p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
