;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-struct-hash-table.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@informatik.uni-hamburg.de
;;;; Date	1996/11/04
;;;; Description	PLOB allocate and accessor functions for
;;;;		LISP hash tables
;;;; The bibliography references used are:
;;;; [St90]	Guy L. Steele Jr.:
;;;;		Common LISP
;;;;		The Language
;;;;		Second Edition
;;;;		Digital Press, Bedford, Massachusetts, 1990
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
;;; Persistent hash-table
;;; ---------------------------------------------------------------------------

(defstruct (persistent-hash-table
            (:include persistent-object))
  #+:lisp-doc "
\\Purposelabel
  A structure holding a persistent hash table."

  (test nil
	#+:lisp-doc :documentation #+:lisp-doc "
 The test mode of the hash table,
 see \\fcite{hash-table-test}.")

  (count nil #+:lisp-doc :documentation #+:lisp-doc "
 The number of elements in the hash table,
 see \\fcite{hash-table-count}.")

  (size nil
	#+:lisp-doc :documentation #+:lisp-doc "
 The size of the hash table, see \\fcite{hash-table-size}.")

  (rehash-size nil
	       #+:lisp-doc :documentation #+:lisp-doc "
 The rehash size of the hash table, see \\fcite{hash-table-rehash-size}.")

  (rehash-threshold nil
		    #+:lisp-doc :documentation #+:lisp-doc "
 The rehash threshols of the hash table,
 see \\fcite{hash-table-rehash-threshold}.")
  
  (key-table nil
	     #+:lisp-doc :documentation #+:lisp-doc "
 The key table of the hash table.")
  
  (value-table nil
	       #+:lisp-doc :documentation #+:lisp-doc "
 The value table of the hash table."))

(setf (class-extent (find-class 'persistent-hash-table)) :cached)
(setf (slot-extent 'key-table
                   (find-class 'persistent-hash-table)) :cached-demand-load)
(setf (slot-extent 'value-table
                   (find-class 'persistent-hash-table)) :cached-demand-load)

;;; ---------------------------------------------------------------------------
(defun store-hash-table (t-hash-table p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-hash-table}}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-hash-table}}
\\Purposelabel
 Store the transient hash table \\funarg{t-hash-table}\\ to
 \\funarg{p-objid}.
\\Seealsolabel
 \\stcite{435--441}."

  (with-transaction (p-heap)
    (let* ((struct-descr
	    (ensure-structure-description 'persistent-hash-table))
	   (t-hash-table-count (hash-table-count t-hash-table))
	   (t-hash-table-size (hash-table-size t-hash-table))
	   (p-key-table-vector nil)
	   (p-value-table-vector nil)
	   (p-hash-tab
	    (cond
	     ((gethash t-hash-table *hash-table->persistent-hash-table*))
	     (p-objid
	      (gethash (p-objid-to-t-object p-objid +structure-type-tag+
					    depth p-heap)
		       *hash-table->persistent-hash-table*))))
	   (to-store nil) (force-write nil))
      (if p-hash-tab
	  (setf p-objid (persistent-object-objid p-hash-tab))
	(progn
	  (setf p-objid (p-allocate-structure struct-descr))
	  (setf p-key-table-vector
	    (make-persistent-object
	     (p-allocate-vector t-hash-table-size p-heap)))
	  (setf p-value-table-vector
	    (make-persistent-object
	     (p-allocate-vector t-hash-table-size p-heap)))
	  (setf p-hash-tab
	    (make-persistent-hash-table
	     :test (hash-table-test t-hash-table)
	     :count t-hash-table-count
	     :size t-hash-table-size
	     :rehash-size (hash-table-rehash-size t-hash-table)
	     :rehash-threshold (hash-table-rehash-threshold t-hash-table)
	     :key-table p-key-table-vector
	     :value-table p-value-table-vector))
	  (setf (persistent-object-objid p-hash-tab) p-objid)
	  (setf (gethash t-hash-table *hash-table->persistent-hash-table*)
	    p-hash-tab)
	  (setf force-write t)))
      (with-write-lock (p-heap p-objid t-hash-table depth
			       +structure-type-tag+ force-write)
	(setf to-store t)
	(if (> t-hash-table-size (persistent-hash-table-size p-hash-tab))
	    (progn
	      ;; Enlarge the key-table-vector and value-table-vector:
	      (p-destroy (persistent-hash-table-key-table p-hash-tab :objid)
			 p-heap)
	      (p-destroy (persistent-hash-table-value-table p-hash-tab :objid)
			 p-heap)
	      (setf p-key-table-vector
		(make-persistent-object
		 (p-allocate-vector t-hash-table-size p-heap)))
	      (setf p-value-table-vector
		(make-persistent-object
		 (p-allocate-vector t-hash-table-size p-heap)))
	      (setf (persistent-hash-table-count p-hash-tab)
		t-hash-table-count)
	      (setf (persistent-hash-table-size p-hash-tab)
		t-hash-table-size)
	      (setf (persistent-hash-table-key-table p-hash-tab)
		p-key-table-vector)
	      (setf (persistent-hash-table-value-table p-hash-tab)
		p-value-table-vector))
	  (progn
	    ;; Load the key-table-vector and value-table-vector:
	    (setf (persistent-hash-table-count p-hash-tab)
	      t-hash-table-count)
	    (setf p-key-table-vector
	      (persistent-hash-table-key-table p-hash-tab :objid))
	    (setf p-value-table-vector
	      (persistent-hash-table-value-table p-hash-tab :objid))))
	;; Copy the hash table keys and values
	;; into the p-{key|value}-table-vectors:
	(write-lock p-heap p-key-table-vector nil :nocache +vector-type-tag+)
	(write-lock p-heap p-value-table-vector nil :nocache +vector-type-tag+)
	(with-objid-buffer
	    (objid-key-buffer p-key-table-vector t-hash-table-count p-heap)
	  (with-objid-buffer
	      (objid-value-buffer p-value-table-vector t-hash-table-count p-heap)
	    (loop for i from 0
		for key being each hash-key in t-hash-table
		using (hash-value value)
		do
		  (multiple-value-bind (key-objid key-type-tag)
		      (t-object-to-p-objid key depth p-heap)
		    (write-objid-buffer objid-key-buffer i
					key-objid key-type-tag))
		  (multiple-value-bind (value-objid value-type-tag)
		      (t-object-to-p-objid value depth p-heap)
		    (write-objid-buffer objid-value-buffer i
					value-objid value-type-tag)))))
	(store-structure-in-transaction p-hash-tab p-objid
					struct-descr depth p-heap))
      (when (and *verbose* (>= *verbose* 6) to-store)
	(format t "; Stored hash table ~A~%" t-hash-table))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-hash-table)
    (t-hash-table &optional p-objid (depth *default-depth*)
			    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-hash-table}}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-hash-table}}
\\Purposelabel
 Store the transient hash table \\funarg{t-hash-table}\\ to
 \\funarg{p-objid}.
\\Seealsolabel
 \\stcite{435--441}."

  (values t-hash-table (store-hash-table t-hash-table p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-hash-table
     (p-objid &optional (depth *default-depth*)
              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Create the transient representation of the persistent hash table
 referenced by \\funarg{p-objid}.
 returned in its transient representation.
\\Seealsolabel
 \\stcite{435--441}."

  (unregister-by-objid p-objid)
  (let* ((p-hash-tab
	  (load-instance-for-structure
	   (ensure-structure-description 'persistent-hash-table)
	   p-objid :cached p-heap))
	 (t-hash-table-count
	  (persistent-hash-table-count p-hash-tab))
	 (p-key-table-vector
	  (persistent-hash-table-key-table p-hash-tab :objid))
	 (p-value-table-vector
	  (persistent-hash-table-value-table p-hash-tab :objid))
	 (t-hash-table
	  (make-hash-table
	   :test (persistent-hash-table-test p-hash-tab)
	   :size (persistent-hash-table-size p-hash-tab)
	   :rehash-size (persistent-hash-table-rehash-size p-hash-tab)
	   :rehash-threshold
	   (persistent-hash-table-rehash-threshold p-hash-tab))))
    (unregister-by-objid p-objid)
    (register-to-cache p-objid t-hash-table)
    (setf (gethash t-hash-table *hash-table->persistent-hash-table*)
	  p-hash-tab)
    ;; Copy the hash table keys value and values
    ;; from the p-{key|value}-table-vectors:
    (read-lock p-heap p-key-table-vector :nocache +vector-type-tag+)
    (read-lock p-heap p-value-table-vector :nocache +vector-type-tag+)
    (with-objid-buffer
	(objid-key-buffer p-key-table-vector t-hash-table-count p-heap)
      (with-objid-buffer
	  (objid-value-buffer p-value-table-vector
			      t-hash-table-count p-heap)
	(loop for i from 0 below t-hash-table-count
	      do
	      (let ((key
		     (multiple-value-bind (key-objid key-type-tag)
			 (read-objid-buffer objid-key-buffer i)
		       (p-objid-to-t-object key-objid key-type-tag
					    depth p-heap)))
		    (value
		     (multiple-value-bind (value-objid value-type-tag)
			 (read-objid-buffer objid-value-buffer i)
		       (p-objid-to-t-object value-objid value-type-tag
					    depth p-heap))))
		(setf (gethash key t-hash-table) value)))))
    (when (and *verbose* (>= *verbose* 6))
      (format t "; Loaded hash table ~A~%" t-hash-table))
    t-hash-table))

;;; ---------------------------------------------------------------------------
;;; Storing of hash tables
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid-using-class
    ((t-object hash-table)
     #+:lispworks (t-class structure-class)
     #-:lispworks t-class
     depth p-heap)
  (declare (ignore t-class))
  (store-hash-table t-object nil depth p-heap))

;;; ---------------------------------------------------------------------------
;;; Loading of hash tables
;;; ---------------------------------------------------------------------------

(defmethod load-instance-for-class ((t-class-name (eql 'persistent-hash-table))
                                    (p-class-descr structure-description)
                                    p-objid depth p-heap
				    &optional t-into-object)
  (declare (ignore t-into-object))
  (p-hash-table p-objid depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
