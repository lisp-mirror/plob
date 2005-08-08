;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-cons.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	10.2.94
;;;; Description	PLOB functions for conses
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
;;; CONS cell
;;; ---------------------------------------------------------------------------

(defun p-allocate-cons (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 cons cell
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (p-allocate p-heap +cons-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-consp (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{consp}."

  (= (p-type-tag-of p-objid p-heap) +cons-type-tag+))

;;; --- cons car --------------------------------------------------------------

(defun (setf p-car) (t-car
                     p-objid
                     &optional (depth *default-depth*)
                     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-car}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{(setf car)}."

  (t-slot-to-p-objid t-car depth p-heap p-objid +cons-location-car+
		     #'(lambda (new-value object)
			 (setf (car object) new-value))
		     +cons-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-car (p-objid
              &optional (depth *default-depth*)
	      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{car}."

  (if (p-null p-objid)
      nil
    (p-objid-to-t-slot p-objid +cons-location-car+ depth p-heap
		       #'(lambda (new-value object)
			   (setf (car object) new-value))
		       +cons-type-tag+)))

;;; --- cons cdr --------------------------------------------------------------

(defun p-setf-cdr (p-heap last-cons-objid next-cons depth)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{last-cons-objid}}
 \\isa{\\funarg{next-cons}}
      {a transient cons cell}
\\Purposelabel
 Tail-recursive function for storing of lists."

  (declare (type ;;; 2005-04-06 hkirschk: Corrected for LispWorks 4.4
		 #-:small-fixnum fixnum
		 #+:small-fixnum integer
		 last-cons-objid)
           (optimize (speed 3) (space 0) (safety 1) (debug 1)
		     (compilation-speed 0)))
  (if (consp next-cons)
      ;; then
      (let ((next-cons-objid (is-registered-object next-cons))
	    (force-write nil))
	#-:lispworks4 ;; and hopefully not later
        (declare (dynamic-extent next-cons-objid))
        (unless next-cons-objid
          ;; next-cons was not registered up to now;
          ;; so allocate it:
          (setf next-cons-objid (p-allocate-cons p-heap))
	  (setf force-write t))
	;; Write the next-cons-objid into the cdr of the plob cons cell
	;; referenced by last-cons-objid:
        (setf (p-index p-heap last-cons-objid +cons-location-cdr+
		       +cons-type-tag+)
	  next-cons-objid)
        ;; Register the next-cons into the cache and transaction table:
        (with-write-lock (p-heap next-cons-objid next-cons depth
				 +cons-type-tag+ force-write)
	  ;; The next cons was not saved up to now.
	  ;; Store the car of the next cons:
	  (t-slot-to-p-objid-in-transaction
	   (car next-cons) depth p-heap next-cons-objid
	   +cons-location-car+ nil +cons-type-tag+)
	  ;; Continue with next cons cell:
	  (p-setf-cdr p-heap next-cons-objid
		      (cdr next-cons) depth)))
    ;; else next-cons is no cons; store it direct in last persistent
    ;; cons cell:
    (t-slot-to-p-objid-in-transaction
     next-cons depth p-heap last-cons-objid
     +cons-location-cdr+ nil +cons-type-tag+)))

;;; ---------------------------------------------------------------------------
(defun (setf p-cdr) (t-cdr p-objid
                           &optional (depth *default-depth*)
                           (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-cdr}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{(setf cdr)}."

  (with-transaction (p-heap)
    (p-setf-cdr p-heap p-objid t-cdr depth))
  t-cdr)

;;; ---------------------------------------------------------------------------
(defun objid-of-cdr (p-heap objid)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{objid}}
\\Purposelabel
 Return the \\objid\\ of the persistent object
 located in the cdr of the persistent cons
 cell referenced by \\funarg{objid}.
\\Seealsolabel
 \\Fcite{p-cdr}."

  (multiple-value-bind (cdr-objid cdr-type-tag)
      (p-index p-heap objid +cons-location-cdr+ +cons-type-tag+)
    (if (= cdr-type-tag +cons-type-tag+)
	cdr-objid
      nil)))

;;; ---------------------------------------------------------------------------
(defun p-get-cdr (p-heap last-cons-objid last-cons depth)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{last-cons-objid}}
 \\isa{\\funarg{last-cons}}
      {a transient cons cell}
\\Purposelabel
 Tail-recursive function for loading of lists."

  (declare (type ;;; 2005-04-06 hkirschk: Corrected for LispWorks 4.4
		 #-:small-fixnum fixnum
		 #+:small-fixnum integer
		 last-cons-objid)
	   (type cons last-cons)
           (optimize (speed 3) (space 0) (safety 1) (debug 1)
		     (compilation-speed 0)))
  ;; Register actual object in the cache tables:
  (register-to-cache last-cons-objid last-cons)
  ;; Load the car object:
  (p-objid-to-t-slot-in-transaction last-cons-objid
				    +cons-location-car+ depth p-heap
				    #'(lambda (new-value object)
					(setf (car object) new-value))
				    +cons-type-tag+)
  ;; Load the cdr object:
  (with-read-lock (p-heap last-cons-objid depth +cons-type-tag+ nil)
    (multiple-value-bind (next-cons-objid next-cons-type-tag)
	(p-index p-heap last-cons-objid +cons-location-cdr+ +cons-type-tag+)
      #-:lispworks4 ;; and hopefully not later
     (declare (dynamic-extent next-cons-objid next-cons-type-tag))
     (if (= next-cons-type-tag +cons-type-tag+)
         (multiple-value-bind (next-cons next-cons-p)
	     (is-registered-objid next-cons-objid)
           (if next-cons-p
	       (setf (cdr last-cons) next-cons)
             (progn
               (setf next-cons (list nil))
	       (setf (cdr last-cons) next-cons)
	       (p-get-cdr p-heap next-cons-objid next-cons depth))))
       (setf (cdr last-cons)
	 (p-objid-to-t-object next-cons-objid next-cons-type-tag
			      depth p-heap))))))

;;; ---------------------------------------------------------------------------
(defun p-cdr (p-objid
              &optional (depth *default-depth*)
              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{cdr}."

  (cond
   ((p-null p-objid) nil)
   ((with-direct-representation-p depth)
    (p-objid-to-t-slot p-objid +cons-location-cdr+ depth p-heap
		       nil +cons-type-tag+))
   (t
    (with-transaction (p-heap)
      (multiple-value-bind (cdr-objid cdr-type-tag)
	  (p-index p-heap p-objid +cons-location-cdr+ +cons-type-tag+)
	#-:lispworks4 ;; and hopefully not later
	(declare (dynamic-extent cdr-objid cdr-type-tag))
	(let ((t-cdr nil) (t-cdr-p nil))
	  (if (= cdr-type-tag +cons-type-tag+)
	      (progn
		(multiple-value-setq (t-cdr t-cdr-p)
		  (is-registered-objid cdr-objid))
		(unless t-cdr-p
		  (setf t-cdr (list nil))
		  (p-get-cdr p-heap cdr-objid t-cdr depth)))
	    (setf t-cdr
	      (p-objid-to-t-object cdr-objid cdr-type-tag depth p-heap)))
	  t-cdr))))))

;;; --- cons list -------------------------------------------------------------

(defun store-list (t-list p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-list}}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-list}}
\\Purposelabel
 Make \\lisp{(car \\funarg{t-list})}\\ the car and
 \\lisp{(cdr \\funarg{t-list})}\\ the cdr of
 the persistent cons cell referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-list}."

  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-list))
      (unless p-objid
	(setf p-objid (p-allocate-cons p-heap))
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-list depth
			       +cons-type-tag+ force-write)
	(setf (p-car p-objid depth p-heap) (car t-list))
	(setf (p-cdr p-objid depth p-heap) (cdr t-list)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-list)
    (t-list &optional p-objid (depth *default-depth*)
		      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-list}}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-list}}
\\Purposelabel
 Make \\lisp{(car \\funarg{t-list})}\\ the car and
 \\lisp{(cdr \\funarg{t-list})}\\ the cdr of
 the persistent cons cell referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-list}."

  (values t-list (store-list t-list p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-list (p-objid
	       &optional (depth *default-depth*)
			 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient cons cell with its car being the transient
 representation of the persistent object located in the car and
 its cdr being the transient representation of the persistent object
 located in the cdr of the persistent cons cell referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-list)}."

  (multiple-value-bind (object object-p)
      (is-registered-objid p-objid)
    (unless object-p
      (setf object (list nil))
      (register-to-cache p-objid object)
      (with-transaction (p-heap)
	(with-read-lock (p-heap p-objid depth +cons-type-tag+ t)
	  (setf (car object) (p-car p-objid depth p-heap))
	  (setf (cdr object) (p-cdr p-objid depth p-heap)))))
    object))

;;; ---------------------------------------------------------------------------
(defun p-cons (the-car the-cdr
	       &optional (depth *default-depth*)
			 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Allocate a persistent cons cell and fill it from
 \\funarg{the-car}\\ and \\funarg{the-cdr}.
\\Seealsolabel
 \\Fcite{cons}."

  (let ((p-objid-cons (p-allocate-cons p-heap)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid-cons nil :nocache +cons-type-tag+ t)
	(setf (p-car p-objid-cons depth p-heap) the-car)
	(setf (p-cdr p-objid-cons depth p-heap) the-cdr)))
    (p-objid-to-t-object p-objid-cons +cons-type-tag+ depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-null (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Check if \\funarg{p-objid}\\ references either
 \\lispnil\\ or the persistent symbol \\lispnil."
  (or (null p-objid)
      (eql (persistent-object-objid p-objid)
	   (p-find-symbol nil :depth :objid :p-heap p-heap))))

;;; ---------------------------------------------------------------------------
;;; Storing of CONSes
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object cons) depth to-p-heap)
  (store-list t-object nil depth to-p-heap))

;;; ---------------------------------------------------------------------------
;;; Loading of CONSes
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +cons-type-tag+))
				depth p-heap)
  (p-list p-objid depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: raw-text-unix
;;;; End:
