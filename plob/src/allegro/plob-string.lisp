;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-string.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	10.2.94
;;;; Description	PLOB functions for strings
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
;;; String
;;; ---------------------------------------------------------------------------

(defun p-allocate-string (number-of-characters
                          &optional
			  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{number-of-characters}}
      {a fixnum}
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 string with at least \\funarg{number-of-characters}\\ characters
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (declare (type fixnum number-of-characters))
  (p-allocate p-heap +string-type-tag+ 0
	      +character-type-tag+
	      (+ number-of-characters 1)))

;;; ---------------------------------------------------------------------------
(defun p-stringp (p-objid
                   &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{stringp}."

  (= (p-type-tag-of p-objid p-heap) +string-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-string-length (p-objid
                         &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{array-total-size}."

  (p-fixnum p-heap p-objid +string-location-length+ +string-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-string-max-length (p-objid
			    &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the maximum possible length of the persistent string
 referenced by \\funarg{p-objid}; this returns the total
 number of bytes allocated for the string including the
 space used for alignment.
\\Seealsolabel
 \\Fcite{p-string-length}."

  (* (p-value-size p-objid p-heap) +sizeof-postore-word+))

;;; ---------------------------------------------------------------------------
(defun store-string (t-string p-objid p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-string}}
      {a string}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-string}}
\\Purposelabel
 Store the transient string in \\funarg{t-string}\\ to the
 persistent string referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-string}."

  (declare (type string t-string))
  (let ((depth (if *cache-strings* :immediate :nocache))
	(force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-string))
      (unless p-objid
	(setf p-objid (p-allocate-string (length t-string) p-heap))
	(setf force-write t)))
    (with-transaction
     (p-heap)
     (let ((to-store nil))
       (with-write-lock
	(p-heap p-objid t-string depth +string-type-tag+ force-write)
	(let* ((length-t-string (length t-string)))
	  (setf (p-chars p-heap p-objid length-t-string) t-string)
	  (setf (p-fixnum p-heap p-objid +string-location-length+)
		length-t-string)))
       (when (and *verbose* (>= *verbose* 6) to-store)
         (format t "; Stored string \"~A\"~%" t-string)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-string)
    (t-string &optional p-objid (depth *default-depth*)
			(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-string}}
      {a string}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-string}}
\\Purposelabel
 Store the transient string in \\funarg{t-string}\\ to the
 persistent string referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-string}."

  (declare (type string t-string) (ignore depth))
  (values t-string (store-string t-string p-objid p-heap)))
  
;;; ---------------------------------------------------------------------------
(defun p-string (p-objid
		 &optional (depth *default-depth*)
		 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 string
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-string)}."

  (let ((t-string (is-registered-objid p-objid)))
    (with-transaction
     (p-heap)
     (let ((from-store nil))
       (with-read-lock
        (p-heap p-objid depth +string-type-tag+ (null t-string))
        (setf from-store t)
        (let* ((length-t-string (p-string-length p-objid p-heap)))
	  (unless t-string
	    (setf t-string (make-string length-t-string
					:initial-element #\Space))
	    (when *cache-strings*
	      (register-to-cache p-objid t-string)))
	  (p-chars-into p-heap p-objid t-string length-t-string)))
       (when (and *verbose* (>= *verbose* 6) from-store)
         (format t "; Loaded string \"~A\"~%" t-string))))
    t-string))

;;; ---------------------------------------------------------------------------
;;; Storing of strings
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object string) depth to-p-heap)
  (declare (ignore depth))
  (store-string t-object nil to-p-heap))

;;; ---------------------------------------------------------------------------
;;; Loading of strings
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +string-type-tag+))
				depth p-heap)
  (p-string p-objid depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
