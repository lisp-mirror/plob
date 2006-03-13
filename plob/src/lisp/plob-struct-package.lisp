;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-struct-package.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	23.2.94	Derived from plob-struct.lisp
;;;; Description	PLOB allocate and accessor functions for
;;;;		PLOB packages
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
(defconstant +package-class+ (find-class 'package)
  #+:lisp-doc "The \\clsmo\\ of structure class \\class{package}.")

;;; ---------------------------------------------------------------------------
(defun p-find-package-name (name)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{name}}
      {either a string or a symbol or a package}
\\Purposelabel
 Returns \\funarg{name}\\ mapped to a string.
\\Seealsolabel
 \\Fcite{p-find-name}."

  (let* ((name-string (p-find-name name))
         (found-name
          (cond
           (name-string
            name-string)
           ((packagep name)
            (package-name name))
           ((p-packagep name)
            (p-package-name name))
           (t
            (error "Cannot map ~A to a package name."))))
         (lisp-package (find-package found-name)))
    #-:lispworks4 ;; and hopefully not later
    (declare (dynamic-extent name-string lisp-package))
    (if lisp-package
      (package-name lisp-package)
      found-name)))

;;; ---------------------------------------------------------------------------
(defun p-find-package (t-package
		       &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-package}}
      {either a string or a symbol or a package}
\\Valueslabel
 Returns a persistent-package or \\lispnil.
\\Purposelabel
 Search for a persistent-package named \\funarg{t-package}; iff such a
 package is found, return a package-representing instance of
 \\fcite{persistent-package}; otherwise, return \\lispnil.
\\Seealsolabel
 \\Fcite{p-delete-package};
 \\fcite{persistent-package};
 \\fcite{p-apropos-packages};
 \\fcite{find-package}."

  (let* ((name (p-find-package-name t-package)))
    #-:lispworks4 ;; and hopefully not later
    (declare (dynamic-extent name))
    (getbtree (internal-to-external-name name)
	      *name->package-table* depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-delete-package (t-package
                         &optional
		         (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-package}}
      {either a string or a symbol or a package}
\\Valueslabel
 Returns \\nonnil\\ iff the package was found and removed,
 \\lispnil\\ otherwise.
\\Purposelabel
 Remove the persistent-package named \\funarg{t-package}\\ from
 the \\sh.
\\Remarkslabel
 If there are still symbols of the deleted package reachable
 in the \\sh, the persistent-package will be put back into the
 package table contained in
 \\fcite{*name->package-table*}\\ next time when such a
 symbol is loaded.
\\Seealsolabel
 \\Fcite{p-find-package};
 \\fcite{persistent-package};
 \\fcite{delete-package}."

  (let ((found-package (p-find-package t-package :objid p-heap)))
    #-:lispworks4 ;; and hopefully not later
    (declare (dynamic-extent found-package))
    (when found-package
      (rembtree (p-package-name found-package :object p-heap)
                *name->package-table*)
      (unregister-by-objid found-package)
      t)))

;;; ---------------------------------------------------------------------------
;;; Packages
;;; ---------------------------------------------------------------------------

(defun p-allocate-package (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 package
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{persistent-package};
 \\fcite{p-allocate}."

  (let ((p-objid (p-allocate p-heap
                             +structure-type-tag+
                             +package-size+)))
    (with-transaction (p-heap)
      (setf (p-index p-heap p-objid +structure-location-description+)
	*package-description-objid*)
      (setf (p-index p-heap p-objid +package-location-internals+)
	(p-allocate-btree p-heap))
      (setf (p-index p-heap p-objid +package-location-externals+)
	(p-allocate-btree p-heap)))
    p-objid))

;;; ---------------------------------------------------------------------------
(defun p-packagep (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of class
 persistent-package,
 \\lispnil\\ otherwise.
\\Seealsolabel
 \\Fcite{persistent-package};
 \\fcite{packagep}."

  (and (= (p-type-tag-of p-objid p-heap) +structure-type-tag+)
       (with-transaction (p-heap)
	 (= (p-index p-heap p-objid +structure-location-description+)
	    *package-description-objid*))))

;;; ---------------------------------------------------------------------------
(defun register-package (p-objid
			 &optional
			 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Registers the persistent-package referenced by
 \\funarg{p-objid}\\ to the \\plob\\ package table
 contained in the \\fcite{*name->package-table*}."

  (let ((p-objid-name (p-package-name p-objid :objid p-heap)))
    (setf (getbtree-by-objid p-objid-name *name->package-table*
                             :objid p-heap)
	  p-objid)))

;;; ---------------------------------------------------------------------------
(defun (setf p-package-name) (t-package-name
			      p-objid
			      &optional (depth *default-depth*)
			      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-package-name}}
      {a string}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-name}
 of the persistent-package referenced by
 \\funarg{p-objid}\\ to \\funarg{t-package-name}.
\\Seealsolabel
 \\Fcite{persistent-package}."

  (when (stringp t-package-name)
    (setf t-package-name (internal-to-external-name t-package-name)))
  (t-slot-to-p-objid t-package-name depth p-heap p-objid
		     +package-location-name+
		     #'(lambda (new-value object)
			 (setf (persistent-package-p-package-name object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
#+(and :lisp-doc (not :document-api))
(:defdoc
 "p-package- ..."
 "Persistent Package Accessors"
 "
\\Purposelabel
 All following functions
 {\\bf p-package-\\textsl{\\lt{}slot-name\\gt}} and
 {\\bf (setf p-package-\\textsl{\\lt{}slot-name\\gt}\\/)}
 with \\textsl{\\lt{}slot-name\\gt} being one of the slot names of
 \\fcite{persistent-package}\\ without the `p-' prefix
 access directly a slot of a persistent structure instance
 of \\fcite{persistent-package}\\ in the \\sh.

 The accessor functions defined here are merely used in the
 bootstrap phase. After the bootstrap, the persistent objects of
 \\fcite{persistent-package}\\ are created and accessed as all
 persistent structure objects by employing their
 structure-descriptions, i.e.\\ all information needed to create
 a persistent structure object or to access a persistent structure
 object's slot is contained in its structure-description.
\\Seealsolabel
 \\Fcite{persistent-package};
 section \\fcite{bootstrap ...}.")

;;; ---------------------------------------------------------------------------
(defun p-package-name (p-objid
		       &optional (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-name}
 of the persistent-package referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-package- ...}."

  (let ((name (p-objid-to-t-slot p-objid +package-location-name+ depth p-heap
				 #'(lambda (new-value object)
				     (setf (persistent-package-p-package-name
					    object)
				       new-value))
				 +structure-type-tag+)))
    (if (stringp name)
	(external-to-internal-name name)
      name)))

;;; ---------------------------------------------------------------------------
(defun (setf p-package-internals) (t-package-internals
			           p-objid
			           &optional (depth *default-depth*)
			           (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabtree{\\funarg{t-package-internals}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-internals}
 of the persistent-package referenced by
 \\funarg{p-objid}\\ to \\funarg{t-package-internals}.
\\Seealsolabel
 Section \\fcite{p-package- ...}."

  (t-slot-to-p-objid t-package-internals depth p-heap p-objid
		     +package-location-internals+
		     #'(lambda (new-value object)
			 (setf (persistent-package-p-internals object)
			   new-value))
		     +structure-type-tag+) )

;;; ---------------------------------------------------------------------------
(defun p-package-internals (p-objid
		            &optional (depth *default-depth*)
		            (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-internals}
 of the persistent-package referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-package- ...}."

  (p-objid-to-t-slot p-objid +package-location-internals+ depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-package-p-internals object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-package-externals) (t-package-externals
			           p-objid
			           &optional (depth *default-depth*)
			           (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabtree{\\funarg{t-package-externals}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the value of slot
 {\\bf p-externals}
 of the persistent-package referenced by
 \\funarg{p-objid}\\ to \\funarg{t-package-externals}.
\\Seealsolabel
 Section \\fcite{p-package- ...}."

  (t-slot-to-p-objid t-package-externals depth p-heap p-objid
		     +package-location-externals+
		     #'(lambda (new-value object)
			 (setf (persistent-package-p-externals object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-package-externals (p-objid
		            &optional (depth *default-depth*)
		            (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the value of slot
 {\\bf p-externals}
 of the persistent-package referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{p-package- ...}."

  (p-objid-to-t-slot p-objid +package-location-externals+ depth p-heap
		     #'(lambda (new-value object)
			 (setf (persistent-package-p-externals object)
			   new-value))
		     +structure-type-tag+))

;;; ---------------------------------------------------------------------------
(defun store-package (t-package p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-package}}
      {a transient package}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-package}}
\\Purposelabel
 Store the transient package in
 \\funarg{t-package}\\ to the
 persistent-package referenced by \\funarg{p-objid}.
\\Remarkslabel
 Only the package name is copied from \\funarg{t-package}\\ into
 the persistent-package; the symbol tables etc.\\ are not copied.
\\Seealsolabel
 \\Fcite{p-package};
 \\fcite{persistent-package}."

  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-package))
      (unless p-objid
	(setf p-objid
	  (p-find-package (package-name t-package) :objid p-heap)))
      (unless p-objid
	(setf p-objid (p-allocate-package p-heap))
	(setf (p-package p-objid depth p-heap) t-package)
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-package depth
			       +structure-type-tag+ force-write)
	(let ((name (package-name t-package)))
	  (setf (p-package-name p-objid depth p-heap) name)
	  (register-package p-objid p-heap)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-package)
    (t-package &optional p-objid (depth *default-depth*)
			 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-package}}
      {a transient package}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-package}}
\\Purposelabel
 Store the transient package in
 \\funarg{t-package}\\ to the
 persistent-package referenced by \\funarg{p-objid}.
\\Remarkslabel
 Only the package name is copied from \\funarg{t-package}\\ into
 the persistent-package; the symbol tables etc.\\ are not copied.
\\Seealsolabel
 \\Fcite{p-package};
 \\fcite{persistent-package}."

  (values t-package (store-package t-package p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-package (p-objid
		  &optional (depth *default-depth*)
		  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 package
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-package)};
 \\fcite{persistent-package}."

  (let ((t-package (is-registered-objid p-objid)))
    (unless t-package
      (with-transaction (p-heap)
	(with-read-lock (p-heap p-objid depth +structure-type-tag+ t)
	  (let ((name (p-package-name p-objid :cached p-heap)))
	    (setf t-package (find-package name))
	    (unless t-package
	      (setf t-package (make-package name)))
	    (register-to-cache p-objid t-package)))))
    t-package))

;;; --- find symbol -----------------------------------------------------------

(defun p-find-symbol (t-symbol
                      &key (package *package* packagep)
                      (depth *default-depth*)
                      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-symbol}}
      {a transient string or a transient symbol}
 \\isa{\\keyarg{package}}
      {either a package object or a package name as a string or symbol}
\\Valueslabel
 Returns two values:
 \\begin{enumerate}

 \\item Iff a persistent symbol named \\funarg{t-symbol}\\ was found,
  a transient representation of the persistent symbol (i.e.\\ a
  transient symbol) is returned as the first value,
  \\lispnil\\ otherwise.

 \\item The second value is \\nonnil\\ iff 
  a persistent symbol named \\funarg{t-symbol}\\ was found at all,
  \\lispnil\\ otherwise.

 \\end{enumerate}
\\Purposelabel
 Search for a persistent symbol named \\funarg{t-symbol}.
 If \\keyarg{package}\\ is passed, the symbol is searched in the
 persistent-package named by \\keyarg{package}.
\\Seealsolabel
 \\Fcite{find-symbol}."

  (let ((t-symbol-name nil) (t-symbol-exists nil) (found-symbol nil)
        (package-name nil))
    #-:lispworks4 ;; and hopefully not later
    (declare (dynamic-extent t-symbol-name t-symbol-exists found-symbol
                             package-name))
    (if (symbolp t-symbol)
        (progn
          (setf t-symbol-name (symbol-name t-symbol))
          (setf t-symbol-exists t)
          (setf found-symbol t-symbol)
          (if packagep
              (progn
                (setf package-name (p-find-package-name package))
                (setf package (find-package package-name)))
	    (progn
	      (setf package (symbol-package t-symbol))
	      (when package
		(setf package-name (package-name package))))))
      (progn
        (setf t-symbol-name (p-find-name t-symbol))
        (setf package-name (p-find-package-name package))
        (setf package (find-package package-name))))
    (block nil
      (when package
        (unless t-symbol-exists
          (multiple-value-setq (found-symbol t-symbol-exists)
	    (find-symbol t-symbol-name package-name)))
	(when t-symbol-exists
	  ;; Look if the symbol can be located in the cache:
	  (let ((p-objid (is-registered-object found-symbol)))
	     #-:lispworks4 ;; and hopefully not later
	    (declare (dynamic-extent p-objid))
	    (when p-objid
	      ;; Symbol was found in the cache:
	      (return (values (p-objid-to-t-object p-objid
						   +symbol-type-tag+
						   depth
						   p-heap)
			      t))))))
      ;; Symbol does not exist in transient memory; so it can be still defined
      ;; in the persistent memory:
      (let ((p-objid-package (p-find-package package-name :objid p-heap)))
	#-:lispworks4 ;; and hopefully not later
	(declare (dynamic-extent p-objid-package))
	(if p-objid-package
	    ;; Search symbol in the internal symbol list of the package:
	    (getbtree (internal-to-external-name t-symbol-name)
		      (p-package-internals p-objid-package :objid p-heap)
		      depth p-heap)
	  ;; The persistent package does not exist at all, so the symbol
	  ;; cannot exist:
	  (values nil nil))))))

;;; ---------------------------------------------------------------------------
(defun p-intern (t-symbol
		 &key (package *package* packagep)
		 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-symbol}}
      {a transient string or a transient symbol}
 \\isa{\\keyarg{package}}
      {either a package object or a package name as a string or symbol}
\\Purposelabel
 See \\fcite{intern}.
\\Seealsolabel
 \\Fcite{p-find-symbol}."

  (when (and (symbolp t-symbol) (not packagep))
    (setf package (symbol-package t-symbol)))
  (let* ((package-name (p-find-package-name package))
         (p-objid-package (p-find-package package-name :objid p-heap)))
    (unless p-objid-package
      (when (and *verbose* (>= *verbose* 1))
	(cerror "Create the persistent package."
		"Persistent package ~A does not exist." package-name))
      (setf p-objid-package (p-allocate-package p-heap))
      (setf (p-package-name p-objid-package :cached p-heap) package-name)
      (register-package p-objid-package p-heap))
    (let ((p-objid-symbol (p-find-symbol t-symbol
					 :package package-name
					 :depth :objid
					 :p-heap p-heap)))
      (if p-objid-symbol
          (values p-objid-symbol :internal)
        (let ((symbol-name (p-find-name t-symbol))
	      (p-objid-name nil))
          (setf p-objid-symbol (p-allocate-symbol p-heap))
          (when package
            (multiple-value-bind (symbol symbol-exists)
                (find-symbol (symbol-name t-symbol) package-name)
              (when symbol-exists
	        (setf p-objid-name
		  (t-object-to-p-objid (internal-to-external-name
					(symbol-name symbol))
				       :cached p-heap)))))
	  (unless p-objid-name
	    (setf p-objid-name
		  (t-object-to-p-objid symbol-name :cached p-heap)))
          (with-transaction (p-heap)
	    (t-slot-to-p-objid-in-transaction p-objid-name :objid
					      p-heap p-objid-symbol
					      +symbol-location-name+
					      nil +symbol-type-tag+)
	    (t-slot-to-p-objid-in-transaction p-objid-package :objid
					      p-heap p-objid-symbol
					      +symbol-location-package+
					      nil +symbol-type-tag+)
	    (setf (getbtree-by-objid p-objid-name
				     (p-package-internals p-objid-package
							  :objid p-heap)
				     :objid
				     p-heap)
	      p-objid-symbol))
          (values p-objid-symbol nil))))))

;;; ---------------------------------------------------------------------------
(defun p-unintern (t-symbol
		   &key (package *package* with-package)
		   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-symbol}}
      {a transient string or a transient symbol}
 \\isa{\\keyarg{package}}
      {either a package object or a package name as a string or symbol}
\\Purposelabel
 See \\fcite{unintern}.
\\Seealsolabel
 \\Fcite{p-find-symbol}."

  (let* ((p-symbol (if with-package
                       (p-find-symbol t-symbol
                                      :package package
                                      :depth :objid
                                      :p-heap p-heap)
		     (p-find-symbol t-symbol
				    :depth :objid
				    :p-heap p-heap)))
         (p-package (when p-symbol
                      (p-symbol-package p-symbol :objid p-heap))))
    (when p-symbol
      (with-transaction (p-heap)
	(setf (p-marker p-heap p-symbol +symbol-location-package+)
	  +unbound-type-tag+)
	(when p-package
	  (rembtree-by-objid (p-symbol-name p-symbol :objid p-heap)
			     +short-objid-tag+
			     (p-package-internals p-package :objid))))
      (unregister-by-objid p-symbol)
      t)))

;;; ---------------------------------------------------------------------------
;;; Storing of packages
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object package) depth to-p-heap)
  (let ((p-objid (is-registered-object t-object)))
    (unless p-objid
      (setf p-objid (p-find-package (package-name t-object)
                                    :objid
                                    to-p-heap)))
    (unless p-objid
      (setf p-objid (p-allocate-package to-p-heap))
      (setf (p-package p-objid depth to-p-heap) t-object))
    p-objid))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
