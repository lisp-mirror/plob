;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-vector.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	10.2.94
;;;; Description	PLOB functions for vectors
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
;;; Vector
;;; ---------------------------------------------------------------------------

(defun p-allocate-vector (number-of-elements
                          &optional
			  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{number-of-elements}}
      {a fixnum}
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 vector with \\funarg{number-of-elements}\\ elements
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

   (declare (type fixnum number-of-elements))
   (p-allocate p-heap +vector-type-tag+ number-of-elements))

;;; ---------------------------------------------------------------------------
(defun p-vectorp (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{vectorp}."

  (= (p-type-tag-of p-objid p-heap) +vector-type-tag+))

;;; --- vector svref ----------------------------------------------------------

(defun (setf p-svref) (t-element p-objid at-location
		       &optional (depth *default-depth*)
				 (p-heap *default-persistent-heap*))
				  
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
\\Purposelabel
 See \\fcite{(setf svref)}."

  (t-slot-to-p-objid t-element depth p-heap p-objid
		     (+ +vector-size+ at-location)
		     #'(lambda (new-value t-vector)
			 (setf (svref t-vector at-location) new-value))
		     +vector-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-svref (p-objid at-location
		&optional (depth *default-depth*)
			  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
\\Purposelabel
 See \\fcite{svref}."

  (p-objid-to-t-slot p-objid  (+ +vector-size+ at-location)
		     depth p-heap
		     #'(lambda (new-value t-vector)
			 (setf (svref t-vector at-location) new-value))
		     +vector-type-tag+))

;;; --- vector length ---------------------------------------------------------

(defun p-vector-length (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{array-total-size}."

  (- (p-objid-size p-objid p-heap) +vector-size+))

;;; --- vector ----------------------------------------------------------------

(defun store-vector (t-vector p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-vector}}
      {a vector}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-vector}}
\\Purposelabel
 Store the transient vector in \\funarg{t-vector}\\ to the
 persistent vector referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-vector}."

  (declare (type simple-vector t-vector))
  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-vector))
      (unless p-objid
	(setf p-objid (p-allocate-vector (length t-vector) p-heap))
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-vector depth
			       +vector-type-tag+ force-write)
      (let ((vector-length (p-vector-length p-objid p-heap)))
	(with-objid-buffer (objid-buffer p-objid vector-length p-heap)
	  (dotimes (i vector-length)
	    (multiple-value-bind (sub-objid sub-type-tag)
		(t-object-to-p-objid (aref t-vector i)
				     depth p-heap)
	      (write-objid-buffer objid-buffer i sub-objid sub-type-tag))))))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-vector)
    (t-vector &optional p-objid (depth *default-depth*)
			(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-vector}}
      {a vector}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-vector}}
\\Purposelabel
 Store the transient vector in \\funarg{t-vector}\\ to the
 persistent vector referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-vector}."

  (declare (type simple-vector t-vector))
  (values t-vector (store-vector t-vector p-objid depth p-heap)))
  
;;; ---------------------------------------------------------------------------
(defun p-vector-into-internal (p-heap p-objid into-vector depth)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-vector-into}.
\\Purposelabel
 Internal used function for \\fcite{p-vector-into}."

  (declare (type simple-vector into-vector))
  (assert (not (t-ivectorp into-vector)))
  ;; Read in the vector:
  (let ((vector-length (p-vector-length p-objid p-heap)))
    (with-objid-buffer (objid-buffer p-objid vector-length p-heap)
      (dotimes (i vector-length)
        (multiple-value-bind (sub-objid sub-type-tag)
	    (read-objid-buffer objid-buffer i)
          (setf (aref into-vector i)
                (p-objid-to-t-object sub-objid sub-type-tag
                                     depth p-heap))))))
  into-vector)

;;; ---------------------------------------------------------------------------
(defun p-vector-into (into-vector
		      p-objid
		      &optional (depth *default-depth*)
		      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{into-vector}}
      {a vector}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Read the persistent vector referenced by
 \\funarg{p-objid}\\ destructively into \\funarg{into-vector}.
\\Seealsolabel
 \\Fcite{p-vector}."

  (declare (type simple-vector into-vector))
  (let ((t-vector (is-registered-objid p-objid)))
    (unless (eq into-vector t-vector)
      (if t-vector
          ;; Just copy in the cached vector:
	(replace into-vector t-vector)
        ;; Read in the vector:
        (p-vector-into-internal p-heap p-objid into-vector depth))))
  into-vector)

;;; ---------------------------------------------------------------------------
(defun p-vector (p-objid
		 &optional (depth *default-depth*)
			   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 vector
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-vector)}."

  (let ((t-vector (is-registered-objid p-objid)))
    (with-transaction
     (p-heap)
     (with-read-lock
      (p-heap p-objid depth +vector-type-tag+ (null t-vector))
      (unless t-vector
        (setf t-vector (make-sequence 'simple-vector
                                      (p-vector-length p-objid p-heap)))
        (register-to-cache p-objid t-vector))
      (p-vector-into-internal p-heap p-objid t-vector depth)))
    t-vector))

;;; ---------------------------------------------------------------------------
;;; IVector
;;; ---------------------------------------------------------------------------

(defun p-allocate-ivector (immediate-type
                           number-of-elements
                           &optional
			   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isatypetag{\\funarg{immediate-type}}
 \\isa{\\funarg{number-of-elements}}
      {a fixnum}
\\Purposelabel
 Returns the \\objid\\ of a new persistent immediate vector
 allocated in the \\sh\\ with
 \\funarg{number-of-elements}\\ elements specialized to the type
 referenced by \\funarg{immediate-type}.
\\Remarkslabel
 An ivector is a vector whose element type is specialized to an
 immediate type, e.g.\\ a vector which can only hold single floats
 is an ivector (of single floats). Transient ivector's are created
 by the \\cl\\ system for arrays with an element type being an
 immediate type, e.g.\\ an ivector of single floats is created
 by the statement \\lisp{(make-array \\ldots\\ :element-type
 'single-float)}.
\\Seealsolabel
 \\Fcite{p-allocate}."

   (declare (type fixnum number-of-elements))
   (catch-errors (c-sh-make-ivector (persistent-object-objid p-heap)
                                    (p-upgraded-array-element-tag
				     immediate-type)
                                    number-of-elements)))

;;; ---------------------------------------------------------------------------
(defun p-ivectorp (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Returns \\nonnil\\ iff \\funarg{p-objid}\\ references a
 persistent object of type
 immediate vector,
 \\lispnil\\ otherwise.
\\Seealsolabel
 \\Fcite{t-ivectorp}."

  (= (p-type-tag-of p-objid p-heap) +ivector-type-tag+))

;;; --- ivector type ----------------------------------------------------------

(defun p-ivector-type (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 Returns a type specifier.
\\Purposelabel
 Get the immediate type to which the elements of the persistent
 immediate vector referenced by \\funarg{p-objid}\\ are specialized to.
\\Seealsolabel
 \\Fcite{p-allocate-ivector};
 \\fcite{p-upgraded-array-element-tag}."

  (with-transaction (p-heap)
    (let* ((type-info
	    (get-type-info (p-fixnum p-heap p-objid
				     +ivector-location-type+
				     +ivector-type-tag+))))
      (type-info-name type-info))))

;;; --- ivector length --------------------------------------------------------

(defun p-ivector-length (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{array-total-size}."

  (with-transaction (p-heap)
    (p-fixnum p-heap p-objid +ivector-location-length+ +ivector-type-tag+)))

;;; --- ivector ---------------------------------------------------------------

(defun store-ivector (t-ivector p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-ivector}}
      {an immediate vector}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-vector}}
\\Purposelabel
 Store the transient immmediate vector in \\funarg{t-ivector}\\ to the
 persistent immediate vector referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-ivector}."

  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-ivector))
      (unless p-objid
	(setf p-objid (p-allocate-ivector (array-element-type t-ivector)
					  (length t-ivector)
					  p-heap))
	(setf force-write t)))
    (with-transaction (p-heap)
      (with-write-lock (p-heap p-objid t-ivector depth
			       +ivector-type-tag+ force-write)
	(let* ((type-info
		(get-type-info (p-fixnum p-heap p-objid
					 +ivector-location-type+
					 +ivector-type-tag+)))
	       (length-t-ivector (length t-ivector))
	       (number-of-written
		(setf (p-values p-heap p-objid (type-info-tag type-info)
				length-t-ivector)
		      t-ivector)))
	  (when (and (/= length-t-ivector number-of-written)
		     *verbose* (>= *verbose* 1))
	    (cerror "Ignore mismatch."
		    "Write mismatch for ivector: ~
		     expected to write ~D elements, wrote ~D elements."
		    length-t-ivector number-of-written))))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-ivector)
    (t-ivector &optional p-objid (depth *default-depth*)
                         (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-ivector}}
      {an immediate vector}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-vector}}
\\Purposelabel
 Store the transient immmediate vector in \\funarg{t-ivector}\\ to the
 persistent immediate vector referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-ivector}."

  (values t-ivector (store-ivector t-ivector p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-ivector-into-internal (p-heap p-objid into-ivector depth)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-ivector-into}.
\\Purposelabel
 Internal used function for \\fcite{p-ivector-into}."

  (declare (type vector into-ivector)
           (ignore depth))
  (assert (t-ivectorp into-ivector))
  ;; Read in the ivector:
  (let* ((type-info
	  (get-type-info (p-fixnum p-heap p-objid
				   +ivector-location-type+
				   +ivector-type-tag+)))
	 (length-into-ivector (length into-ivector))
         (number-of-read (p-values-into p-heap p-objid
					(type-info-tag type-info) into-ivector
                                        length-into-ivector)))
    (when (and (/= length-into-ivector number-of-read)
	       *verbose* (>= *verbose* 1))
      (cerror "Ignore mismatch."
	      "Read mismatch for ivector: ~
	       expected to read ~D elements, read ~D elements."
	      length-into-ivector number-of-read)))
  into-ivector)

;;; ---------------------------------------------------------------------------
(defun p-ivector-into (into-ivector
                       p-objid
                       &optional (depth *default-depth*)
                       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{into-ivector}}
      {an immediate vector}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Read the persistent immediate vector referenced by
 \\funarg{p-objid}\\ destructively into \\funarg{into-ivector}.
\\Seealsolabel
 \\Fcite{p-ivector}."

  (declare (type simple-vector into-ivector))
  (let ((t-ivector (is-registered-objid p-objid)))
    (unless (eq into-ivector t-ivector)
      (if t-ivector
          ;; Just copy in the cached ivector:
          (replace into-ivector t-ivector)
        ;; Read in the ivector:
        (p-ivector-into-internal p-heap p-objid into-ivector depth))))
  into-ivector)

;;; ---------------------------------------------------------------------------
(defun p-ivector (p-objid
                  &optional (depth *default-depth*)
                  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 immediate vector
 referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-ivector)}."

  (let ((t-ivector (is-registered-objid p-objid)))
    (with-transaction (p-heap)
      (with-read-lock (p-heap p-objid depth +ivector-type-tag+
			      (null t-ivector))
        (unless t-ivector
	  (setf t-ivector (make-sequence `(vector ,(p-ivector-type
                                                    p-objid p-heap))
				         (p-ivector-length p-objid p-heap)))
          (register-to-cache p-objid t-ivector))
        (p-ivector-into-internal p-heap p-objid t-ivector depth)))
    t-ivector))

;;; ---------------------------------------------------------------------------
;;; Storing of vectors
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object vector) depth to-p-heap)
  (cond
   ((subtypep (type-of t-object) 'simple-vector)
    (store-vector t-object nil depth to-p-heap))
   ((t-ivectorp t-object)
    (store-ivector t-object nil depth to-p-heap))
   (t
    ;; It looks & tastes like a vector but is is a 1-dimensional
    ;; (maybe displaced) array ...
    (call-next-method))))

;;; ---------------------------------------------------------------------------
;;; Loading of vectors
;;; ---------------------------------------------------------------------------

(defmethod load-object-into (p-objid
                             (into-t-vector vector)
                             depth p-heap)
  (if (t-ivectorp into-t-vector)
      (p-ivector-into into-t-vector p-objid depth p-heap)
    (p-vector-into into-t-vector p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +vector-type-tag+))
				depth p-heap)
  (p-vector p-objid depth p-heap))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +ivector-type-tag+))
				depth p-heap)
  (p-ivector p-objid depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
