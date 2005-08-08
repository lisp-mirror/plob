;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-bit-vector.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	10.2.94
;;;; Description	PLOB functions for bit-vectors
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
;;; Bit-vector
;;; ---------------------------------------------------------------------------
(defun p-allocate-bit-vector (number-of-bits
                              &optional
			      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{number-of-bits}}
      {a fixnum}
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 bit vector with at least \\funarg{number-of-bits}\\ bits
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (declare (type fixnum number-of-bits))
  (let ((p-objid (p-allocate p-heap +bit-vector-type-tag+ 0
			     +unsigned-byte-1-tag+ number-of-bits)))
  (with-transaction (p-heap)
    (with-write-lock (p-heap p-objid nil :nocache +bit-vector-type-tag+ t)
      (setf (p-fixnum p-heap p-objid +bit-vector-location-length+)
	number-of-bits)))
  p-objid))

;;; ---------------------------------------------------------------------------
(defun p-bit-vector-p (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{bit-vector-p}."

  (declare (type ;;; 2005-04-06 hkirschk: Corrected for LispWorks 4.4
                 #-:small-fixnum fixnum
                 #+:small-fixnum integer
		 p-objid)
  (= (p-type-tag-of p-objid p-heap) +bit-vector-type-tag+)))

;;; --- bit-vector length -----------------------------------------------------
(defun p-bit-vector-length (p-objid
			    &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 See \\fcite{array-total-size}."

  (declare (type ;;; 2005-04-06 hkirschk: Corrected for LispWorks 4.4
                 #-:small-fixnum fixnum
                 #+:small-fixnum integer
                 p-objid))
  (with-transaction (p-heap)
    (with-read-lock (p-heap p-objid nil +bit-vector-type-tag+ t)
      (p-fixnum p-heap p-objid
		+bit-vector-location-length+ +bit-vector-type-tag+))))

;;; ---------------------------------------------------------------------------
(defun p-bit-vector-max-length (p-objid
                                &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{p-objid}}

\\Purposelabel

 Get the maximum possible length of the persistent bit vector
 referenced by \\funarg{p-objid}; this returns the total
 number of bits allocated for the bit vector including the
 space used for alignment.

\\Seealsolabel

 \\Fcite{p-bit-vector-length}."

  (declare (type ;;; 2005-04-06 hkirschk: Corrected for LispWorks 4.4
                 #-:small-fixnum fixnum
                 #+:small-fixnum integer
                 p-objid))
  (* (p-value-size p-objid p-heap) +sizeof-postore-word+))

;;; ---------------------------------------------------------------------------
(defun store-bit-vector (t-bit-vector p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-bit-vector}}
      {a bit vector}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-bit-vector}}
\\Purposelabel
 Store the transient bit vector in \\funarg{t-bit-vector}\\ to the
 persistent bit vector referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-bit-vector}."

  (declare (type bit-vector t-bit-vector))
  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-bit-vector))
      (unless p-objid
	(setf p-objid (p-allocate-bit-vector (length t-bit-vector) p-heap))
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-bit-vector depth
			       +bit-vector-type-tag+ force-write)
	(let* ((length-t-bit-vector (length t-bit-vector))
	       (number-of-written (setf (p-values p-heap p-objid
						  +unsigned-byte-1-tag+
						  length-t-bit-vector)
				    t-bit-vector)))
	  (setf (p-fixnum p-heap p-objid +bit-vector-location-length+)
	    length-t-bit-vector)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-bit-vector) (t-bit-vector
			    &optional p-objid
				      (depth *default-depth*)
				      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-bit-vector}}
      {a bit vector}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-bit-vector}}
\\Purposelabel
 Store the transient bit vector in \\funarg{t-bit-vector}\\ to the
 persistent bit vector referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-bit-vector}."

  (declare (type bit-vector t-bit-vector))
  (values t-bit-vector (store-bit-vector t-bit-vector p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-bit-vector-into-internal (p-heap p-objid into-bit-vector)
  #+:lisp-doc "
\\Argumentslabel

 See \\fcite{p-bit-vector-into}.

\\Purposelabel

 Internal used function for \\fcite{p-bit-vector-into}."

  (declare (type ;;; 2005-04-06 hkirschk: Corrected for LispWorks 4.4
                 #-:small-fixnum fixnum
                 #+:small-fixnum integer
                 p-objid)
   (type bit-vector into-bit-vector))
  ;; Read in the bit-vector:
  (p-values-into p-heap p-objid +unsigned-byte-1-tag+ into-bit-vector
		 (ceiling (p-bit-vector-length p-objid p-heap)
			  (* +sizeof-postore-word+ +bits-per-byte+))
		 +bit-vector-type-tag+)
  into-bit-vector)

;;; ---------------------------------------------------------------------------
(defun p-bit-vector-into (into-bit-vector
                          p-objid
                          &optional (depth *default-depth*)
                          (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{into-bit-vector}}
      {a bit vector}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Read the persistent bit vector referenced by
 \\funarg{p-objid}\\ destructively into \\funarg{into-bit-vector}.
\\Seealsolabel
 \\Fcite{p-bit-vector}."

  (declare (type ;;; 2005-04-06 hkirschk: Corrected for LispWorks 4.4
                 #-:small-fixnum fixnum
                 #+:small-fixnum integer
                 p-objid)
           (type bit-vector into-bit-vector)
           (ignore depth))
  (let ((t-bit-vector (is-registered-objid p-objid)))
    (if t-bit-vector
	;; Just copy in the cached vector:
	(replace into-bit-vector t-bit-vector)
      ;; Read in the bit-vector:
      (p-bit-vector-into-internal p-heap p-objid into-bit-vector)))
  into-bit-vector)

;;; ---------------------------------------------------------------------------
(defun p-bit-vector (p-objid
		     &optional (depth *default-depth*)
		     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 bit vector
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-bit-vector)}."

  (declare (type ;;; 2005-04-06 hkirschk: Corrected for LispWorks 4.4
                 #-:small-fixnum fixnum
                 #+:small-fixnum integer
                 p-objid))
  (let ((t-bit-vector (is-registered-objid p-objid)))
    (with-transaction (p-heap)
      (with-read-lock (p-heap p-objid depth +bit-vector-type-tag+
			      (null t-bit-vector))
	(unless t-bit-vector
	  (setf t-bit-vector (make-sequence
			      'bit-vector
			      (p-bit-vector-length p-objid p-heap)))
	  (register-to-cache p-objid t-bit-vector))
	(p-bit-vector-into-internal p-heap p-objid t-bit-vector)))
    t-bit-vector))

;;; ---------------------------------------------------------------------------
;;; Storing of bit-vectors
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object bit-vector) depth to-p-heap)
  (store-bit-vector t-object nil depth to-p-heap))

;;; ---------------------------------------------------------------------------
;;; Loading of bit-vectors
;;; ---------------------------------------------------------------------------

(defmethod load-object-into (p-objid
                             (into-t-bit-vector bit-vector)
                             depth p-heap)
  (p-bit-vector-into into-t-bit-vector p-objid depth p-heap))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +bit-vector-type-tag+))
				depth p-heap)
  (p-bit-vector p-objid depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: raw-text-unix
;;;; End:
