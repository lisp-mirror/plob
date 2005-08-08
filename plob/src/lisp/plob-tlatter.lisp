;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-tlatter.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	2.3.94
;;;; Description	PLOB functions for TLatters
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
;;; TLatters
;;; ---------------------------------------------------------------------------

#+(and :lispworks3 :lisp-doc (not :document-api))
(:defdoc
 "tlatter ..."
 "Tlatter Macros and Accessors"
 "
\\Purposelabel
 A tlatter is similar to a cons but has 3~slots:\\ a {\\bf key},
 an associated {\\bf datum} and a reference to a {\\bf next} tlatter.
 Tlatters are used e.g.\\ in \\lw\\ for implementing hash tables.
 One hash table entry is a tlatter instance containing the hash key,
 the associated data and a reference to the next tlatter.

 \\plob\\ offers the class \\class{tlatter}\\ to make possible an
 efficient handling of \\lw\\ hash tables. In \\allegrocl, no
 instances of class \\class{tlatter}\\ are used at all; they are
 doing coalesced chaining as described in \\cite{bib:Knuth-73c}.

\\Remarkslabel
 The term `tlatter' is not invented by myself but I found it in the
 \\lw\\ internals.
 %% Perhaps someone could give me a hint what the
 %% term actually means; send your answer to
 %% {\\tt kirschke@kogs26.informatik.uni-hamburg.de}.

%%\\Seealsolabel
%% \\Fcite{+null-io+}.
")

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defun p-allocate-tlatter (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 tlatter
 allocated in the \\sh.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{p-allocate}."

  (p-allocate p-heap +tlatter-type-tag+))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defun p-tlatterp (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of type
 tlatter,
 \\lispnil\\ otherwise.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{tlatter-p}."

  (= (p-type-tag-of p-objid p-heap) +tlatter-type-tag+))

;;; --- TLatter key -----------------------------------------------------------

#+:lispworks3
(defun (setf p-tlatter-key) (t-key
                             p-objid
                             &optional (depth *default-depth*)
                             (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-key}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the {\\bf key} slot of the persistent tlatter referenced by
 \\funarg{p-objid} to \\funarg{t-key}.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{p-tlatter-key};
 \\fcite{tlatter-set-key}."

  (t-slot-to-p-objid t-key depth p-heap p-objid +tlatter-location-key+
		     nil +tlatter-type-tag+))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defun p-tlatter-key (p-objid
                      &optional (depth *default-depth*)
	              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the {\\bf key} slot of the persistent tlatter referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{(setf p-tlatter-key)};
 \\fcite{tlatter-get-key}."

  (p-objid-to-t-slot p-objid +tlatter-location-key+ depth p-heap nil
		     +tlatter-type-tag+))

;;; --- TLatter data ----------------------------------------------------------

#+:lispworks3
(defun (setf p-tlatter-data) (t-data
                              p-objid
                              &optional (depth *default-depth*)
                              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-data}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the {\\bf data} slot of the persistent tlatter referenced by
 \\funarg{p-objid} to \\funarg{t-data}.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{p-tlatter-data};
 \\fcite{tlatter-set-data}."

  (t-slot-to-p-objid t-data depth p-heap p-objid
		     +tlatter-location-data+ nil +tlatter-type-tag+))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defun p-tlatter-data (p-objid
                       &optional (depth *default-depth*)
	               (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the {\\bf data} slot of the persistent tlatter referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{(setf p-tlatter-data)};
 \\fcite{tlatter-get-data}."

  (p-objid-to-t-slot p-objid +tlatter-location-data+ depth p-heap nil
		     +tlatter-type-tag+))

;;; --- TLatter next ----------------------------------------------------------

#+:lispworks3
(defun (setf p-tlatter-next) (t-next
                              p-objid
                              &optional (depth *default-depth*)
                              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-next}}
      {a tlatter}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the {\\bf next} slot of the persistent tlatter referenced by
 \\funarg{p-objid} to \\funarg{t-next}.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{p-tlatter-next};
 \\fcite{tlatter-set-next}."

  (t-slot-to-p-objid t-next depth p-heap p-objid
		     +tlatter-location-next+ nil +tlatter-type-tag+))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defun p-tlatter-next (p-objid
                       &optional (depth *default-depth*)
	               (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the {\\bf next} slot of the persistent tlatter referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{(setf p-tlatter-next)};
 \\fcite{tlatter-get-next}."

  (p-objid-to-t-slot p-objid +tlatter-location-next+ depth p-heap nil
		     +tlatter-type-tag+))

;;; --- TLatter ---------------------------------------------------------------

#+:lispworks3
(defun store-tlatter (t-tlatter p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-tlatter}}
      {a tlatter}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-tlatter}}
\\Purposelabel
 Store the transient tlatter in \\funarg{t-tlatter}\\ to the
 persistent tlatter referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{p-tlatter}."

  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-tlatter))
      (unless p-objid
	(setf p-objid (p-allocate-tlatter p-heap))
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-tlatter :nocache
			       +tlatter-type-tag+ force-write)
	(setf (p-tlatter-key p-objid depth p-heap)
	  (tlatter-get-key t-tlatter))
	(setf (p-tlatter-data p-objid depth p-heap)
	  (tlatter-get-data t-tlatter))
	(setf (p-tlatter-next p-objid depth p-heap)
	  (tlatter-get-next t-tlatter)))))
  p-objid)

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defun (setf p-tlatter)
    (t-tlatter &optional p-objid (depth *default-depth*)
		         (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-tlatter}}
      {a tlatter}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-tlatter}}
\\Purposelabel
 Store the transient tlatter in \\funarg{t-tlatter}\\ to the
 persistent tlatter referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{p-tlatter}."

  (values t-tlatter (store-tlatter t-tlatter p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defun p-tlatter (p-objid
                  &optional (depth *default-depth*)
                  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 tlatter
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 Section \\fcite{tlatter ...};
 \\fcite{(setf p-tlatter)}."

  (with-transaction (p-heap)
    (read-lock p-heap p-objid depth +tlatter-type-tag+)
    (make-tlatter 
     (p-tlatter-key p-objid depth p-heap)
     (p-tlatter-data p-objid depth p-heap)
     (p-tlatter-next p-objid depth p-heap))))

;;; ---------------------------------------------------------------------------
;;; Storing of TLatters
;;; ---------------------------------------------------------------------------

;; see plob-builtin

;;; ---------------------------------------------------------------------------
;;; Loading of TLatters
;;; ---------------------------------------------------------------------------

#+:lispworks3
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag
				 (eql +tlatter-type-tag+))
				depth
				p-heap)
  (p-tlatter p-objid depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
