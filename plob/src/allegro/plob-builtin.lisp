;;;; -*-Lisp-*-----------------------------------------------------------------
;;;; Module	plob-builtin.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	18.11.93
;;;; Description	PLOB support for instances of immediate and
;;;;		built-in types.
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
(defconstant +plob-unbound-marker+
    (make-persistent-object +unbound-type-tag+ +unbound-type-tag+)
  #+:lisp-doc "Object used as persistent unbound marker.")

;;; ---------------------------------------------------------------------------
(defconstant +plob-unstorable-object-marker+
    (make-persistent-object +unstorable-object-marker+
			    +unstorable-object-marker+)
  #+:lisp-doc "Object used as persistent unstorable object marker.")

;;; ---------------------------------------------------------------------------
(defconstant +plob-min-marker+
    (make-persistent-object +min-tag+ +min-tag+)
  #+:lisp-doc "Object used for representing a minimum object.")

;;; ---------------------------------------------------------------------------
(defconstant +plob-max-marker+
    (make-persistent-object +max-tag+ +max-tag+)
  #+:lisp-doc "Object used for representing a maximum object.")

;;; ---------------------------------------------------------------------------
;;; Storing of immediate objects
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object character) depth to-p-heap)
  #+:lisp-doc "Returns the \\lisp{char-code}\\ of \\funarg{t-object}."
  (declare (ignore depth to-p-heap))
  (values (char-code t-object) +character-type-tag+))

;;; ---------------------------------------------------------------------------
(defmethod t-object-to-p-objid ((t-object integer)
                                (depth (eql :objid))
				to-p-heap)
  #+:lisp-doc "Returns \\funarg{t-object}\\ type-marked as \\objid."
  (declare (ignore to-p-heap))
  (values t-object +short-objid-tag+))

;;; ---------------------------------------------------------------------------
(defmethod t-object-to-p-objid ((t-object persistent-object)
                                (depth (eql :objid))
				to-p-heap)
  (declare (ignore to-p-heap))
  (persistent-object-internal-objid t-object))

;;; ---------------------------------------------------------------------------
(defmethod t-object-to-p-objid ((t-object persistent-immediate-object)
                                (depth (eql :objid))
				to-p-heap)
  (declare (ignore to-p-heap))
  (values (persistent-immediate-object-objid t-object)
	  (persistent-immediate-object-type-tag t-object)))

;;; ---------------------------------------------------------------------------
(defmethod t-object-to-p-objid ((t-object persistent-object)
                                (depth (eql :object))
				to-p-heap)
  #+:lisp-doc "Returns \\funarg{t-object}."
  (declare (ignore to-p-heap))
  t-object)

;;; ---------------------------------------------------------------------------
(defmethod t-object-to-p-objid (t-object depth to-p-heap)
  ;; A TLatter is so much low-level that it is not represented by a
  ;; built-in class; so storing is done here:
  #+(and :lisp-doc :lispworks3)
  "If \\funarg{t-object}\\ is of class \\class{tlatter}\\ (see section
 \\fcite{tlatter ...}), \\funarg{t-object}\\ is stored here;
 otherwise, \\fcite{t-object-to-p-objid-using-class}\\ is called."
  #+:lispworks3
  (if (tlatter-p t-object)
      (store-tlatter t-object nil depth to-p-heap)
    (t-object-to-p-objid-using-class t-object (class-of t-object)
                                     depth to-p-heap))
  #-:lispworks3
  (t-object-to-p-objid-using-class t-object (class-of t-object)
				   depth to-p-heap))

;;; ---------------------------------------------------------------------------
(defun get-depth-of-next-level (depth)
  #+:lisp-doc "Get the depth parameter for the next recursion level."
  (case depth
    ((:objid :object :cached :flat :deep)
     depth)
    (t
     :flat)))
  
;;; ---------------------------------------------------------------------------
(defmethod t-object-to-p-objid :around (t-object depth to-p-heap)
  #+:lisp-doc "Handle \\funarg{depth}."
  (let ((next-depth (get-depth-of-next-level depth))
	(p-objid nil) (p-type-tag nil))
    (case next-depth
     (:cached
      ;; Continue store for objects not found in cache:
      (setf p-objid (is-registered-object t-object)))
     (:flat
      ;; Store sublevels with a depth of :cached:
      (setf next-depth :cached)))
    (unless p-objid
      (let ((*default-persistent-heap* to-p-heap) ;; 1998/11/20 HK: Added.
            (*default-depth* depth)
	    (*transient-slot-value* nil))
	(multiple-value-setq (p-objid p-type-tag)
	  (call-next-method t-object next-depth to-p-heap))))
    (if p-objid
	(unless p-type-tag
	  (setf p-type-tag +short-objid-tag+))
      (progn
        (setf p-objid +unstorable-object-marker+)
        (setf p-type-tag +unstorable-object-marker+)))
    (values p-objid p-type-tag)))

;;; ---------------------------------------------------------------------------
;;; Loading of immediate objects
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object :around (p-objid p-type-tag depth p-heap)
  #+:lisp-doc "Handle \\funarg{depth}."
  (let ((next-depth (get-depth-of-next-level depth))
	(t-object nil) (t-object-p nil))
    (case next-depth
     (:cached
      ;; Continue load for objects not found in cache:
      (unless (p-immediatep p-type-tag)
	(multiple-value-setq (t-object t-object-p)
	  (is-registered-objid p-objid))))
     (:flat
      ;; Store sublevels with a depth of :cached:
      (setf next-depth :cached)))
    (unless t-object-p
      (let ((*default-persistent-heap* p-heap) ;; 1998/11/20 HK: Added.
            (*default-depth* depth)
	    (*transient-slot-value* nil))
	(setf t-object
	  (call-next-method p-objid p-type-tag next-depth p-heap))))
    t-object))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid p-objid-type-tag depth p-heap)
  #+:lisp-doc "Signals an error."
  (declare (ignore depth p-heap))
  (error "Trying to load objid ~A with unknown type tag ~A~%"
	 p-objid p-objid-type-tag))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +built-in-type-tag+))
				depth p-heap)
  (declare (ignore depth p-heap))
  (values (make-persistent-object p-objid p-objid-type-tag)))
  
;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
			        (p-objid-type-tag (eql +short-objid-tag+))
			        depth p-heap)
  #+:lisp-doc "Get the `real' type of the persistent object referenced by
 \\funarg{p-objid}\\ and go on with this \\typetag."
  (p-objid-to-t-object p-objid (p-type-tag-of p-objid p-heap) depth p-heap))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +character-type-tag+))
				depth p-heap)
  #+:lisp-doc "Returns the \\lisp{character}\\ of \\funarg{p-objid}."
  (declare (ignore depth p-heap))
  (code-char p-objid))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +unbound-type-tag+))
				depth p-heap)
  #+:lisp-doc "Returns the \\fcite{+plob-unbound-marker+}."
  (declare (ignore p-objid depth p-heap))
  +plob-unbound-marker+)

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag
                                 (eql +unstorable-object-marker+))
				depth p-heap)
  #+:lisp-doc "Returns the \\fcite{+plob-unstorable-object-marker+}."
  (declare (ignore p-objid depth p-heap))
  +plob-unstorable-object-marker+)

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +heap-type-tag+))
				depth p-heap)
  (declare (ignore depth p-heap))
  (make-persistent-heap p-objid))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +machine-type-tag+))
				depth p-heap)
  (declare (ignore depth p-heap))
  (make-persistent-object p-objid p-objid-type-tag))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object ((p-objid integer)
				p-objid-type-tag
				(depth (eql :objid))
                                p-heap)
  (declare (ignore p-heap))
  (values p-objid p-objid-type-tag))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object ((p-objid integer)
				p-objid-type-tag
				(depth (eql :object))
                                p-heap)
  (declare (ignore p-heap))
  (make-persistent-object p-objid p-objid-type-tag))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object ((p-objid persistent-object)
				p-objid-type-tag
				(depth (eql :objid))
                                p-heap)
  (declare (ignore p-objid-type-tag p-heap))
  (persistent-object-objid p-objid))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object ((p-objid persistent-object)
				p-objid-type-tag
				(depth (eql :object))
                                p-heap)
  (declare (ignore p-objid-type-tag p-heap))
  p-objid)

;;; ---------------------------------------------------------------------------
#|
;; 1998/01/22 HK: Removed method:
(defmethod p-objid-to-t-object ((p-objid string)
				p-objid-type-tag
				depth p-heap)
  #+:lisp-doc "Think of removing this method."
  (declare (ignore p-objid-type-tag))
  ;; A string is mapped to its symbol:
  (p-find-symbol p-objid :depth depth :package *package* :p-heap p-heap))
|#

;;; ---------------------------------------------------------------------------
#|
;; 1998/01/22 HK: Removed method:
(defmethod p-objid-to-t-object ((p-objid symbol)
				p-objid-type-tag
				depth p-heap)
  #+:lisp-doc "The \\funarg{p-objid}\\ is mapped to the value of the persistent symbol
 named \\funarg{p-objid}."
  (declare (ignore p-objid-type-tag))
  (let ((p-found (p-find-symbol p-objid
                                :depth :objid
                                :package (symbol-package p-objid)
                                :p-heap p-heap)))
    (if p-found
        (p-symbol-value p-found depth)
      (values))))
|#

;;; ---------------------------------------------------------------------------
(defmethod t-object-to-p-objid-using-class
     (t-object t-class depth p-heap)
  #+:lisp-doc "Return a \lisp{+unstorable-object-marker+}"
  (declare (ignore t-object t-class depth p-heap))
  (values +unstorable-object-marker+ +unstorable-object-marker+))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
