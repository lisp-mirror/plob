;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-method-descr.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	9.3.94
;;;; Description	PLOB allocate and accessor functions for
;;;;		LISP CLOS method descriptions
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

;;; ---------------------------------------------------------------------------
;;; Transient CLOS method description
;;; ---------------------------------------------------------------------------
(defmethod fill-description
     ((the-method method)
      &optional t-method-descr (p-heap *default-persistent-heap*))
  (unless t-method-descr
    (setf t-method-descr (make-instance 'method-description :p-heap p-heap)))
  (let ((generic-function (method-generic-function the-method)))
    (when generic-function
      (setf generic-function (generic-function-name generic-function)))
    (setf (method-description-name t-method-descr) generic-function))
  #+:store-functions
  (setf (method-description-function t-method-descr)
        (method-function the-method))
  (setf (method-description-lambda-list t-method-descr)
        (method-lambda-list the-method))
  (setf (method-description-specializers t-method-descr)
        (method-specializers the-method))
  (setf (method-description-qualifiers t-method-descr)
        (method-qualifiers the-method))
  t-method-descr)

;;; ---------------------------------------------------------------------------
(defun method-description-equal-p (t-method-descr p-method-descr
				   &optional verbose)
  #+:lisp-doc "
\\Argumentslabel

 \\isa{\\funarg{t-method-descr}\\ resp.\\ \\funarg{p-method-descr}}
      {a method description}

\\Purposelabel

 Returns \\nonnil\\ iff the (transient) method description
 \\funarg{t-method-descr}\\ and the (persistent) method description
 \\funarg{p-method-descr}\\ are \\lisp{equal}, \\lispnil\\ otherwise."

  (or (eq t-method-descr p-method-descr)
      (and (equal (method-description-lambda-list t-method-descr)
                  (method-description-lambda-list p-method-descr))
           (equal (method-description-specializers t-method-descr)
                  (method-description-specializers p-method-descr))
           (equal (method-description-qualifiers t-method-descr)
	          (method-description-qualifiers p-method-descr)))))

;;; ---------------------------------------------------------------------------
;;; Persistent CLOS method description
;;; ---------------------------------------------------------------------------

(defun p-allocate-method-description (&optional
				      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 method-description
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{method-description};
 \\fcite{p-allocate}."

  (p-allocate-instance *method-description-objid* p-heap))

;;; ---------------------------------------------------------------------------
(defun p-method-description-p (p-objid
			      &optional
			      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of class
 method-description,
 \\lispnil\\ otherwise.

\\Seealsolabel

 \\Fcite{method-description}."

  (and (= (p-type-tag-of p-objid p-heap) +instance-type-tag+)
       (with-transaction (p-heap)
	 (= (p-index p-heap p-objid +clos-location-class-wrapper+)
	    *method-description-objid*))))

;;; ---------------------------------------------------------------------------
#+(and :lisp-doc (not :document-api))
(:defdoc
 "p-method-description- ..."
 "Method Description Accessors"
 "
\\Purposelabel

 All following functions
 {\\bf p-method-description-\\textsl{\\lt{}slot-name\\gt}} and
 {\\bf (setf p-method-description-\\textsl{\\lt{}slot-name\\gt}\\/)}
 with \\textsl{\\lt{}slot-name\\gt} being one of the slot names of
 \\fcite{method-description}\\ without the `p-' prefix
 access directly a slot of a persistent \\clos\\ instance
 of \\fcite{method-description}\\ in the \\sh.

 The accessor functions defined here are merely used in the
 bootstrap phase. After the bootstrap, the persistent objects of
 \\fcite{method-description}\\ are created and accessed as all
 persistent \\clos\\ instances by employing their
 class-descriptions, i.e.\\ all information needed to create
 a persistent \\clos\\ instance or to access a persistent
 \\clos\\ instance's slot is contained in its class-description.

\\Seealsolabel

 \\Fcite{method-description};
 section \\fcite{bootstrap ...}.")

;;; ---------------------------------------------------------------------------
(defun p-method-description-name
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Get the value of slot
 {\\bf p-name}
 of the persistent method-description referenced by \\funarg{p-objid}.

\\Seealsolabel

 Section \\fcite{p-method-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +method-description-location-name+
	     depth
	     p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-method-description-name)
    (t-name p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isa{\\funarg{t-name}}
      {a symbol}
 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Set the value of slot
 {\\bf p-name}
 of the persistent method-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-name}.

\\Seealsolabel

 Section \\fcite{p-method-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +method-description-location-name+
		   depth p-heap)
         t-name)))

;;; ---------------------------------------------------------------------------
(defun p-method-description-function
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Get the value of slot
 {\\bf p-function}
 of the persistent method-description referenced by \\funarg{p-objid}.

\\Seealsolabel

 Section \\fcite{p-method-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +method-description-location-function+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-method-description-function)
    (t-function p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isa{\\funarg{t-function}}
      {a method function}
 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Set the value of slot
 {\\bf p-function}
 of the persistent method-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-function}.

\\Seealsolabel

 Section \\fcite{p-method-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +method-description-location-function+
		   depth p-heap)
         t-function)))

;;; ---------------------------------------------------------------------------
(defun p-method-description-lambda-list
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Get the value of slot
 {\\bf p-lambda-list}
 of the persistent method-description referenced by \\funarg{p-objid}.

\\Seealsolabel

 Section \\fcite{p-method-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +method-description-location-lambda-list+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-method-description-lambda-list)
     (t-lambda-list p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isa{\\funarg{t-lambda-list}}
      {a $\\lambda$-list}
 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Set the value of slot
 {\\bf p-lambda-list}
 of the persistent method-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-lambda-list}.

\\Seealsolabel

 Section \\fcite{p-method-description- ...}."

  (with-transaction (p-heap)
   (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
	          +method-description-location-lambda-list+
	          depth p-heap)
         t-lambda-list)))

;;; ---------------------------------------------------------------------------
(defun p-method-description-specializers
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Get the value of slot
 {\\bf p-specializers}
 of the persistent method-description referenced by \\funarg{p-objid}.

\\Seealsolabel

 Section \\fcite{p-method-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +method-description-location-specializers+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-method-description-specializers)
    (t-specializers p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isa{\\funarg{t-specializers}}
      {a list of specializer metaobjects}
 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Set the value of slot
 {\\bf p-specializers}
 of the persistent method-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-specializers}.

\\Seealsolabel

 Section \\fcite{p-method-description- ...}."

  (with-transaction (p-heap)
    (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
		   +method-description-location-specializers+
		   depth
		   p-heap)
      t-specializers)))

;;; ---------------------------------------------------------------------------
(defun p-method-description-qualifiers
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Get the value of slot
 {\\bf p-qualifiers}
 of the persistent method-description referenced by \\funarg{p-objid}.

\\Seealsolabel

 Section \\fcite{p-method-description- ...}."

  (with-transaction (p-heap)
    (p-svref (p-instance-data-vector p-objid :objid p-heap)
	     +method-description-location-qualifiers+
	     depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun (setf p-method-description-qualifiers)
     (t-qualifiers p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isa{\\funarg{t-qualifiers}}
      {a list of qualifying (keyword) symbols}
 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Set the value of slot
 {\\bf p-qualifiers}
 of the persistent method-description referenced by
 \\funarg{p-objid}\\ to \\funarg{t-qualifiers}.

\\Seealsolabel

 Section \\fcite{p-method-description- ...}."

  (with-transaction (p-heap)
   (setf (p-svref (p-instance-data-vector p-objid :objid p-heap)
	          +method-description-location-qualifiers+
	          depth p-heap)
         t-qualifiers)))

;;; ---------------------------------------------------------------------------
(defun p-method-description-into
     (t-into-descr p-objid
      &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isa{\\funarg{t-into-descr}}
      {a method-description}
 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Read the persistent method-description referenced by
 \\funarg{p-objid}\\ destructively into \\funarg{t-into-descr}.

\\Seealsolabel

 \\Fcite{p-method-description}."

  (setf (persistent-object-objid t-into-descr) p-objid)
  (with-transaction (p-heap)
    (with-read-lock (p-heap p-objid depth +instance-type-tag+ nil)
      (setf (method-description-name t-into-descr)
	(p-method-description-name p-objid depth p-heap))
      #+:store-functions
      (setf (method-description-function t-into-descr)
	(p-method-description-function p-objid depth p-heap))
      (setf (method-description-lambda-list t-into-descr)
	(p-method-description-lambda-list p-objid depth p-heap))
      (setf (method-description-specializers t-into-descr)
	(p-method-description-specializers p-objid depth p-heap))
      (setf (method-description-qualifiers t-into-descr)
	(p-method-description-qualifiers p-objid depth p-heap))))
  t-into-descr)

;;; ---------------------------------------------------------------------------
(defun p-method-description
    (p-objid
     &optional (depth *default-depth*) (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Return a transient representation for the persistent
 method-description
 referenced by \\funarg{p-objid}.

\\Seealsolabel

 \\Fcite{(setf p-method-description)}."

  (let ((t-object (is-registered-objid p-objid)))
    (unless t-object
      (setf t-object (make-instance 'method-description :p-heap p-heap))
      (register-to-cache p-objid t-object)
      (p-method-description-into t-object p-objid depth p-heap))
    t-object))

;;; ---------------------------------------------------------------------------
(defun store-method-description (t-descr p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a method-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient method-description in
 \\funarg{t-descr}\\ to the
 persistent method-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-method-description}."

  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-descr))
      (unless p-objid
	(setf p-objid (p-allocate-method-description p-heap))
	(setf force-write t)))
    (setf (persistent-object-objid t-descr) p-objid)
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-descr depth
			       +instance-type-tag+ force-write)
      (setf (p-method-description-name p-objid depth p-heap)
	(method-description-name t-descr))
      #+:store-functions
      (setf (p-method-description-function p-objid depth p-heap)
	(method-description-function t-descr))
      (setf (p-method-description-lambda-list p-objid depth p-heap)
	(method-description-lambda-list t-descr))
      (setf (p-method-description-specializers p-objid depth p-heap)
	(method-description-specializers t-descr))
      (setf (p-method-description-qualifiers p-objid depth p-heap)
	(method-description-qualifiers t-descr)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-method-description)
    (t-descr &optional p-objid (depth *default-depth*)
		       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-descr}}
      {a method-description}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-descr}}
\\Purposelabel
 Store the transient method-description in
 \\funarg{t-descr}\\ to the
 persistent method-description referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-method-description}."

  (values t-descr (store-method-description t-descr p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
;;; Storing of CLOS method metaobjects
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid-using-class ((t-object method)
                                            (t-class standard-class)
				            depth p-heap)
  #+:lisp-doc "Stores \\funarg{t-object}\\ as an instance of
 \\fcite{method-description}."

  (store-method-description t-object nil depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
