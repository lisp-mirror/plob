;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-array.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	10.2.94
;;;; Description	PLOB functions for arrays
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
;;; Array
;;; ---------------------------------------------------------------------------

(defun p-allocate-array (t-array-rank
                         &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-array-rank}}
      {a fixnum}
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 array with rank \\funarg{t-array-rank}\\ allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate};
 \\fcite{array-rank}."

  (declare (type fixnum t-array-rank))
  (p-allocate p-heap +array-type-tag+ t-array-rank))

;;; ---------------------------------------------------------------------------
(defun p-arrayp (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{arrayp}."

  (declare (type fixnum p-objid))
  (= (p-type-tag-of p-objid p-heap) +array-type-tag+))

;;; ---------------------------------------------------------------------------
(defvar *ignore-displaced-to-failed* nil
  #+:lisp-doc "Flag if the error should be ignored which is signalled when
a call to \\fcite{array-displaced-to}\\ fails.")

;;; ---------------------------------------------------------------------------
(defvar *data-vector-array-table* (make-hash-table)
  #+:lisp-doc "A variable mapping the data vector of an array to
 the array (which uses the data vector for holding its elements).")

;;; ---------------------------------------------------------------------------
(defun array-displaced-to-with-interaction (t-array)
  #+:lisp-doc "
\\Purposelabel
 Straight call to \\fcite{array-displaced-to}\\ with some
 additional user-interaction if it cannot be decided if
 \\funarg{t-array}\\ is displaced to another array."

  (declare (type array t-array))

  (flet ((handle-displaced-to-error
          (error-prompt error-array)
	  (when (and (not *ignore-displaced-to-failed*)
		     *verbose* (>= *verbose* 1))
            (loop
	      (restart-case
		  (error error-prompt error-array)
		(continue
		    ()
		    :report
		      "Assume that the array is not :displaced-to another array;
	continue and ignore this error next time an array is encountered."
		  (setf *ignore-displaced-to-failed* t)
		  (return))
		(re-signal
		    ()
		    :report
		      "Assume that the array is not :displaced-to another array;
	continue and re-signal this error next time an array is encountered."
		  (return))
		(enter-displaced-to-array
		    (expr)
		    :report
		      "Enter an expression which evaluates to the :displaced-to array;
	continue and re-signal this error next time an array is encountered."
		    :interactive
		      (lambda ()
			(list (prompt-for 'array)))
		  (multiple-value-bind (displaced-to-p displaced-to-p-valid)
		      (array-displaced-to-p t-array expr)
		    (if (not displaced-to-p-valid)
			(setf displaced-to-p t))
		    (if displaced-to-p
			(return expr)))
		  (format *query-io* "
Sub-Error: Array ~A~is not displaced to ~A; try again.~%"
			  t-array expr)))))))

    (multiple-value-bind (displaced-to displaced-to-array)
      (array-displaced-to t-array)
      (let* ((data (array-data-vector t-array))
             (d-to
              (cond
               ((and (not displaced-to) (not displaced-to-array))
	        (handle-displaced-to-error "Cannot determine if array ~A
	is displaced to another array." t-array))
               (displaced-to
                (if (symbolp displaced-to-array)
                    (setf displaced-to-array
                          (gethash data *data-vector-array-table*)))
                (if displaced-to-array
                    displaced-to-array
	            (handle-displaced-to-error (concatenate 'string
                                                            "Array ~A
	is displaced to another array but this array cannot be found."
                                                            (if data "
	HINT: Storing the referenced array before the displaced to array
	will circumvent this error."))
                                               t-array)))
               (t
                nil))))
        (if (and data d-to)
            (setf (gethash data *data-vector-array-table*) t-array))
        d-to))))

;;; --- array data vector -----------------------------------------------------

(defun p-array-data-vector (p-objid
			    &optional (depth *default-depth*)
			    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the data vector of the persistent array referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{array-data-vector}."

  (p-objid-to-t-slot p-objid +array-location-data-vector+ depth p-heap
		     nil +array-type-tag+))

;;; --- array fill pointer ----------------------------------------------------

(defun (setf p-array-fill-pointer)
    (t-array-fill-pointer p-objid
     &optional (depth *default-depth*)
	       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-array-fill-pointer}}
      {a fixnum}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{(setf fill-pointer)}."

  (t-slot-to-p-objid t-array-fill-pointer depth p-heap p-objid
		     +array-location-fill-pointer+ nil +array-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-array-fill-pointer (p-objid
			     &optional (depth *default-depth*)
				       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{fill-pointer}."

  (p-objid-to-t-slot p-objid +array-location-fill-pointer+ depth
		     p-heap nil +array-type-tag+))

;;; --- array displaced offset ------------------------------------------------

(defun p-array-displaced-offset (p-objid
			         &optional (depth *default-depth*)
					   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the displaced offset of the persistent array referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{array-displaced-offset}."

  (p-objid-to-t-slot p-objid +array-location-displaced-offset+ depth
		     p-heap nil +array-type-tag+))

;;; --- array adjustable ------------------------------------------------------

(defun p-array-adjustable (p-objid
			   &optional (depth *default-depth*)
				     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{adjustable-array-p}."

  (p-objid-to-t-slot p-objid +array-location-adjustable+ depth p-heap
		     nil +array-type-tag+))

;;; --- array rank ------------------------------------------------------------

(defun p-array-rank (p-objid
		     &optional (depth *default-depth*)
			       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{array-rank}."

  (with-transaction (p-heap)
    (with-read-lock (p-heap p-objid depth +array-type-tag+ t)
      (p-fixnum p-heap p-objid +array-location-rank+ +array-type-tag+))))

;;; --- array dimensions ------------------------------------------------------

(defun p-array-dimensions (p-objid
			   &optional (depth *default-depth*)
			   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{array-dimensions}."

  (let ((rank (p-array-rank p-objid depth p-heap))
        (dimensions nil))
    (with-transaction (p-heap)
      (with-read-lock (p-heap p-objid depth +array-type-tag+ t)
	(loop for i from (1- rank) downto 0
	    do
	      (push (p-fixnum p-heap p-objid
			      (+ i +array-location-first-dimension+)
                            +array-type-tag+)
		    dimensions))))
    dimensions))

;;; --- array element type ----------------------------------------------------

(defun p-array-element-type (p-objid
			     &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{array-element-type}."

  (let* ((p-objid-data-vector (p-array-data-vector p-objid :objid p-heap))
         (type-tag (p-type-tag-of p-objid-data-vector p-heap)))
    (cond
     ((= type-tag +vector-type-tag+)
      t)
     ((= type-tag +bit-vector-type-tag+)
      '(unsigned-byte 1))
     ((= type-tag +ivector-type-tag+)
      (p-ivector-type p-objid-data-vector p-heap))
     (t
      (error "Unknown array data vector type tag ~D found."
             type-tag)))))

;;; --- array -----------------------------------------------------------------

(defun store-array (t-array p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-array}}
      {an array}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-array}}
\\Purposelabel
 Store the transient array in \\funarg{t-array}\\ to the
 persistent array referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-array}."

  (flet ((set-p-array-dimensions
	     (p-objid depth p-heap t-array-dimensions)
	   "Make T-ARRAY-DIMENSIONS the
array dimensions of the plob array referenced by P-OBJID in P-HEAP."
	   (let ((rank (p-array-rank p-objid depth p-heap)))
	     (with-transaction (p-heap)
	       (write-lock p-heap p-objid nil :nocache +array-type-tag+)
	       (loop for i from 0 to (1- rank)
	           do
		     (setf (p-fixnum p-heap p-objid
				     (+ i +array-location-first-dimension+)
				     +array-type-tag+)
		       (elt t-array-dimensions i)))))
	   t-array-dimensions))

    (declare (type array t-array))
    (let ((force-write nil))
      (unless p-objid
	(setf p-objid (is-registered-object t-array))
	(unless p-objid
	  (setf p-objid (p-allocate-array (array-rank t-array) p-heap))
	  (setf force-write t)))
      (with-transaction (p-heap)
	(with-write-lock (p-heap p-objid t-array depth +array-type-tag+
				 force-write)
	  (let ((displaced-to (array-displaced-to-with-interaction t-array)))
	    (if displaced-to
		;; then we have a displaced to array:
		(progn
		  (t-slot-to-p-objid (array-displaced-offset t-array)
				     depth p-heap p-objid
				     +array-location-displaced-offset+ nil
				     +array-type-tag+)
		  (t-slot-to-p-objid displaced-to depth p-heap p-objid
				     +array-location-data-vector+ nil))
	      ;; else we have no displaced to array:
	      (let ((data-vector (array-data-vector t-array)))
		(t-slot-to-p-objid nil :cached p-heap p-objid
				   +array-location-displaced-offset+ nil
				   +array-type-tag+)
		(if data-vector
		    ;; then store the data vector direct:
		    (t-slot-to-p-objid data-vector depth p-heap p-objid
				       +array-location-data-vector+ nil
				       +array-type-tag+)
		  ;; else allocate a plob vector with size
		  ;; (array-total-size t-array) ...
		  (let* ((total-size (array-total-size t-array))
			 (data-vector-objid (p-allocate-vector total-size)))
		    ;; ... and write directly to it:
		    (dotimes (i total-size)
		      (setf (p-svref data-vector-objid i depth p-heap)
			(row-major-aref t-array i)))
                    (t-slot-to-p-objid data-vector-objid :objid p-heap p-objid
                                       +array-location-data-vector+ nil
                                       +array-type-tag+)
                    ))))
	    (setf (p-array-fill-pointer p-objid depth p-heap)
	      (when (array-has-fill-pointer-p t-array)
		(fill-pointer t-array)))
	    (t-slot-to-p-objid (adjustable-array-p t-array) :cached
			       p-heap p-objid +array-location-adjustable+ nil
			       +array-type-tag+)
	    (set-p-array-dimensions p-objid depth p-heap
				    (array-dimensions t-array))))))
    p-objid))

;;; ---------------------------------------------------------------------------
(defun (setf p-array) (t-array &optional p-objid
					 (depth *default-depth*)
					 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-array}}
      {an array}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-array}}
\\Purposelabel
 Store the transient array in \\funarg{t-array}\\ to the
 persistent array referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-array}."

  (values t-array (store-array t-array p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-array (p-objid
		&optional (depth *default-depth*)
		(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 array
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-array)}."

  (let ((object (is-registered-objid p-objid)))
    (with-transaction (p-heap)
      (with-read-lock (p-heap p-objid depth +array-type-tag+ (null object))
	(let* ((p-displaced-offset
		(p-array-displaced-offset p-objid depth p-heap))
	       (p-data-vector
		(if p-displaced-offset
		    ;; The array to be loaded is displaced to
		    ;; another array; the another array is
		    ;; contained in the data vector:
		    (p-array-data-vector p-objid depth p-heap)
		  ;; The array is not displaced to another array;
		  ;; Load only the objid of the data vector:
		  (p-index
		   p-heap p-objid +array-location-data-vector+
		   +array-type-tag+)))
	       (p-fill-pointer (p-array-fill-pointer p-objid depth p-heap))
	       (p-adjustable (p-array-adjustable p-objid depth p-heap))
	       (p-dimensions (p-array-dimensions p-objid depth p-heap)))
	  (if p-displaced-offset
	      (unless object
		(setf object 
		  (make-array p-dimensions
			      :displaced-to p-data-vector
			      :displaced-index-offset p-displaced-offset
			      :element-type
			      (p-array-element-type p-objid p-heap)
			      :fill-pointer p-fill-pointer
			      :adjustable p-adjustable))
		(register-to-cache p-objid object))
	    (progn
	      (unless object
		(setf object 
		  (make-array p-dimensions
			      :adjustable p-adjustable
			      :element-type
			      (p-array-element-type p-objid p-heap)
			      :fill-pointer p-fill-pointer))
		(register-to-cache p-objid object))
	      (let ((data-vector (array-data-vector object)))
		(if data-vector
		    (load-object-into p-data-vector data-vector depth p-heap)
		  (dotimes (i (array-total-size object))
		    (setf (row-major-aref object i)
		      (p-svref p-data-vector i depth p-heap))))))))))
    object))

;;; ---------------------------------------------------------------------------
;;; Storing of arrays
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object array) depth to-p-heap)
  (store-array t-object nil depth to-p-heap))

;;; ---------------------------------------------------------------------------
;;; Loading of arrays
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +array-type-tag+))
				depth p-heap)
  (p-array p-objid depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
