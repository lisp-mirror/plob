;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-numeric.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	10.2.94
;;;; Description	PLOB functions for numeric objects
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
;;; Double float
;;; ---------------------------------------------------------------------------

(defun p-allocate-double-float (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 double float
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (p-allocate p-heap +double-float-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-make-double-float (from
                            &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Create a persistent double float."
  (declare (type double-float from))
  (with-transaction (p-heap)
    (sh-make-double-float (persistent-object-objid p-heap) from)))

;;; ---------------------------------------------------------------------------
(defun p-double-float-p (p-objid
                         &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{floatp}."

  (= (p-type-tag-of p-objid p-heap) +double-float-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-double-float)
     (the-double-float p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-double-float}}
      {a double float}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{the-double-float}}
\\Purposelabel
 Store the transient double float in \\funarg{the-double-float}\\ to the
 persistent double float referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-double-float};
 \\fcite{sh-write-double-float}."

  (with-transaction (p-heap)
    (sh-write-double-float (persistent-object-objid p-heap)
			   (persistent-object-objid p-objid)
			   the-double-float))
  the-double-float)

;;; ---------------------------------------------------------------------------
(defun p-double-float
     (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 double float
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-double-float)};
 \\fcite{sh-read-double-float}."

  (let ((t-object (is-registered-objid p-objid)))
    (unless t-object
      (with-transaction (p-heap)
	(setf t-object
	  (sh-read-double-float (persistent-object-objid p-heap)
				(persistent-object-objid p-objid))))
      (when *cache-numbers*
	(register-to-cache p-objid t-object)))
    t-object))

;;; ---------------------------------------------------------------------------
;;; Single float
;;; ---------------------------------------------------------------------------

(defun p-allocate-single-float (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 single float
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (p-allocate p-heap +single-float-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-make-single-float (from
                            &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Create a persistent single float."
  (declare (type single-float from))
  (with-transaction (p-heap)
    (sh-make-single-float (persistent-object-objid p-heap)
			  (coerce from 'double-float))))

;;; ---------------------------------------------------------------------------
(defun p-single-float-p (p-objid
                         &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{floatp}."

  (= (p-type-tag-of p-objid p-heap) +single-float-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-single-float)
     (the-single-float p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-single-float}}
      {a single float}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{the-single-float}}
\\Purposelabel
 Store the transient single float in \\funarg{the-single-float}\\ to the
 persistent single float referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-single-float};
 \\fcite{sh-write-single-float}."

  (with-transaction (p-heap)
    (sh-write-single-float (persistent-object-objid p-heap)
			   (persistent-object-objid p-objid)
			   (coerce the-single-float 'double-float)))
  the-single-float)

;;; ---------------------------------------------------------------------------
(defun p-single-float
     (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 single float
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-single-float)};
 \\fcite{sh-read-single-float}."

  (let ((t-object (is-registered-objid p-objid)))
    (unless t-object
      (with-transaction (p-heap)
	(setf t-object
	  (sh-read-single-float (persistent-object-objid p-heap)
				(persistent-object-objid p-objid))))
      (when *cache-numbers*
	(register-to-cache p-objid t-object)))
    t-object))

;;; ---------------------------------------------------------------------------
;;; Storing of floats
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object float) depth p-heap)
  (declare (ignore depth))
  (let ((type-of-t-object (type-of t-object)))
    (if (and +has-short-float-p+ (eq type-of-t-object 'short-float))
	(values (short-float-to-fixnum t-object) +short-float-type-tag+)
      (let ((p-objid (is-registered-object t-object)))
	(ecase type-of-t-object
	  (single-float
	   (if p-objid
	       (setf (p-single-float p-objid p-heap) t-object)
	     (setf p-objid (p-make-single-float t-object p-heap))))
	  ((double-float long-float)
	   (if p-objid
	       (setf (p-double-float p-objid p-heap) t-object)
	     (setf p-objid (p-make-double-float t-object p-heap)))))
        p-objid))))

;;; ---------------------------------------------------------------------------
;;; Loading of floats
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag
                                 (eql +short-float-type-tag+))
				depth p-heap)
  (declare (ignore depth p-heap))
  (fixnum-to-short-float p-objid))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag
                                 (eql +double-float-type-tag+))
				depth p-heap)
  (declare (ignore depth))
  (p-double-float p-objid p-heap))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag
                                 (eql +single-float-type-tag+))
				depth p-heap)
  (declare (ignore depth))
  (p-single-float p-objid p-heap))

;;; ---------------------------------------------------------------------------
;;; Bignum
;;; ---------------------------------------------------------------------------

(defun p-allocate-bignum
     (number-of-bits &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{number-of-bits}}
      {a fixnum}
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 bignum with \\funarg{number-of-bits}\\ bits
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (p-allocate p-heap +bignum-type-tag+ 0 +unsigned-byte-1-tag+ number-of-bits))

;;; ---------------------------------------------------------------------------
(defun p-make-bignum (from
		      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Create a persistent double float."
  (declare (type bignum from))
  (with-transaction (p-heap)
    (sh-make-bignum (persistent-object-objid p-heap) from)))

;;; ---------------------------------------------------------------------------
(defun p-bignum-p (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{integerp}."

  (= (p-type-tag-of p-objid p-heap) +bignum-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf p-bignum)
     (the-bignum p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-bignum}}
      {a bignum}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{the-bignum}}
\\Purposelabel
 Store the transient bignum in \\funarg{the-bignum}\\ to the
 persistent bignum referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-bignum};
 \\fcite{sh-write-bignum}."

  (with-transaction (p-heap)
    (sh-write-bignum (persistent-object-objid p-heap)
		     (persistent-object-objid p-objid)
		     the-bignum))
  the-bignum)

;;; ---------------------------------------------------------------------------
(defun p-bignum (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 bignum
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-bignum)};
 \\fcite{sh-read-bignum}."

  (let ((t-object (is-registered-objid p-objid)))
    (unless t-object
      (with-transaction (p-heap)
	(setf t-object
	  (sh-read-bignum (persistent-object-objid p-heap)
			  (persistent-object-objid p-objid))))
      (when *cache-numbers*
	(register-to-cache p-objid t-object)))
    t-object))

;;; ---------------------------------------------------------------------------
;;; Storing of integers
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object integer) depth p-heap)
  (declare (ignore depth))
  (if (p-fixnump t-object)
      (values t-object +fixnum-type-tag+)
    (let ((p-objid (is-registered-object t-object)))
      (if p-objid
	  (setf (p-bignum p-objid p-heap) t-object)
	(setf p-objid (p-make-bignum t-object p-heap)))
      p-objid)))

;;; ---------------------------------------------------------------------------
;;; Loading of integers
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +fixnum-type-tag+))
				depth p-heap)
  #+:lisp-doc "Returns \\funarg{p-objid}\\ itself."
  (declare (ignore depth p-heap))
  p-objid)

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +bignum-type-tag+))
				depth p-heap)
  (declare (ignore depth))
  (p-bignum p-objid p-heap))

;;; ---------------------------------------------------------------------------
;;; Ratio
;;; ---------------------------------------------------------------------------

(defun p-allocate-ratio (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 ratio number
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (p-allocate p-heap +ratio-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-ratiop (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{rationalp}."

  (= (p-type-tag-of p-objid p-heap) +ratio-type-tag+))

;;; --- ratio numerator -------------------------------------------------------

(defun p-numerator (p-objid
		    &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{numerator}."

  (p-objid-to-t-slot p-objid +ratio-location-numerator+ :immediate
		     p-heap nil +ratio-type-tag+))

;;; --- ratio denominator -----------------------------------------------------

(defun p-denominator (p-objid
		      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{denominator}."

  (p-objid-to-t-slot p-objid +ratio-location-denominator+ :immediate
		     p-heap nil +ratio-type-tag+))

;;; --- ratio -----------------------------------------------------------------

(defun store-ratio (t-ratio p-objid p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-ratio}}
      {a ratio number}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-ratio}}
\\Purposelabel
 Store the transient ratio number in \\funarg{t-ratio}\\ to the
 persistent ratio number referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-ratio}."

  (let ((depth (if *cache-numbers* :immediate :nocache))
	(force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-ratio))
      (unless p-objid
	(setf p-objid (p-allocate-ratio p-heap))
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-ratio depth +ratio-type-tag+
			       force-write)
	(t-slot-to-p-objid-in-transaction
	 (numerator t-ratio) depth p-heap p-objid
	 +ratio-location-numerator+ nil +ratio-type-tag+)
	(t-slot-to-p-objid-in-transaction
	 (denominator t-ratio) depth p-heap p-objid
	 +ratio-location-denominator+ nil +ratio-type-tag+))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-ratio)
    (t-ratio &optional p-objid (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-ratio}}
      {a ratio number}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-ratio}}
\\Purposelabel
 Store the transient ratio number in \\funarg{t-ratio}\\ to the
 persistent ratio number referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-ratio}."

  (values t-ratio (store-ratio t-ratio p-objid p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-ratio (p-objid
		&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 ratio number
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-ratio)}."

  (let ((object (is-registered-objid p-objid)))
    (unless object
      (with-transaction (p-heap)
	(with-read-lock (p-heap p-objid :immediate +ratio-type-tag+ t)
	  (setf object
	    (/ (p-numerator p-objid p-heap)
	       (p-denominator p-objid p-heap)))))
      (when *cache-numbers*
	(register-to-cache p-objid object)))
    object))

;;; ---------------------------------------------------------------------------
;;; Storing of ratio
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object ratio) depth p-heap)
  (declare (ignore depth))
  (store-ratio t-object nil p-heap))

;;; ---------------------------------------------------------------------------
;;; Loading of ratio
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +ratio-type-tag+))
				depth p-heap)
  (declare (ignore depth))
  (p-ratio p-objid p-heap))

;;; ---------------------------------------------------------------------------
;;; Complex
;;; ---------------------------------------------------------------------------

(defun p-allocate-complex (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 complex number
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (p-allocate p-heap +complex-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-complexp (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{complexp}."

  (= (p-type-tag-of p-objid p-heap) +complex-type-tag+))

;;; --- complex realpart ------------------------------------------------------

(defun p-realpart (p-objid
		   &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{realpart}."

  (p-objid-to-t-slot p-objid +complex-location-real-part+ :immediate
		     p-heap nil +complex-type-tag+))

;;; --- complex imagpart ------------------------------------------------------

(defun p-imagpart (p-objid
		   &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{imagpart}."

  (p-objid-to-t-slot p-objid +complex-location-imag-part+ :immediate
		     p-heap nil +complex-type-tag+))

;;; --- complex ---------------------------------------------------------------

(defun store-complex (t-complex p-objid p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-complex}}
      {a complex number}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-complex}}
\\Purposelabel
 Store the transient complex number in \\funarg{t-complex}\\ to the
 persistent complex number referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-complex}."

  (let ((depth (if *cache-numbers* :immediate :nocache))
	(force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-complex))
      (unless p-objid
	(setf p-objid (p-allocate-complex p-heap))
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-complex depth
			       +complex-type-tag+ force-write)
	(t-slot-to-p-objid-in-transaction
	 (realpart t-complex) depth p-heap p-objid
	 +complex-location-real-part+ nil +complex-type-tag+)
	(t-slot-to-p-objid-in-transaction
	 (imagpart t-complex) depth p-heap p-objid
	 +complex-location-imag-part+ nil +complex-type-tag+))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-complex)
    (t-complex &optional p-objid (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-complex}}
      {a complex number}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-complex}}
\\Purposelabel
 Store the transient complex number in \\funarg{t-complex}\\ to the
 persistent complex number referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-complex}."

  (values t-complex (store-complex t-complex p-objid p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-complex (p-objid
		  &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 complex number
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-complex)}."

  (let ((object (is-registered-objid p-objid)))
    (unless object
      (with-transaction (p-heap)
	(with-read-lock (p-heap p-objid :immediate +complex-type-tag+ t)
	  (setf object (complex (p-realpart p-objid p-heap)
				(p-imagpart p-objid p-heap)))))
      (when *cache-numbers*
        (register-to-cache p-objid object)))
    object))

;;; ---------------------------------------------------------------------------
;;; Storing of complex
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object complex) depth p-heap)
  (declare (ignore depth))
  (store-complex t-object nil p-heap))

;;; ---------------------------------------------------------------------------
;;; Loading of complex
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +complex-type-tag+))
				depth p-heap)
  (declare (ignore depth))
  (p-complex p-objid p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
