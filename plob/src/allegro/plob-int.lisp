;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-int.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	1998/04/07
;;;; Description	Handling of C [unsigned] int vectors
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
;;; Variables
;;; ---------------------------------------------------------------------------
(defvar *dynamic-allocated-int-vectors* nil
  #+:lisp-doc
  "List containing all dynamic allocated [unsigned] int vectors so far.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (pushnew :c-int-vector *features*)
  ;; (setf *features* (delete :c-int-vector *features*))
  )

;;; ---------------------------------------------------------------------------
(defconstant +c-int-type+
    #+:allegro 'fixnum
    #-:allegro '(signed-byte 32)
  #+:lisp-doc
  "LISP type declaration matching a C int.")

;;; ---------------------------------------------------------------------------
;;; Classes
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Interface: Generic functions
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Implementation: Macros and functions
;;; ---------------------------------------------------------------------------

#+:allegro
(ff:def-c-type (internal-c-int-vector :in-foreign-space) 2 :signed-long)

;;; ---------------------------------------------------------------------------
#|
#+:allegro
(defun room-as-string ()
  #+:lisp-doc
  "Return (room t) in a string."
  (with-output-to-string (stream)
    (let ((*standard-output* stream))
      (room t))))

#+:allegro
(defun mallocated-bytes ()
  #+:lisp-doc
  "Return total number of bytes occupied by malloc() calls."
  (let* ((token " total bytes:")
	 (length-token (length token))
	 (in (make-string-input-stream (room-as-string))))
    ;; Scan through room-string:
    (loop for line = (read-line in nil :done)
	then (read-line in nil :done)
	until (eq line :done)
	when (string-equal token (subseq line 0 length-token))
	do
	  (multiple-value-bind (free-bytes free-byte-characters)
	      (read-from-string line nil nil :start length-token)
	    (multiple-value-bind (used-bytes used-byte-characters)
		(read-from-string line nil nil
				  :start free-byte-characters)
	      (return used-bytes))))))
|#

;;; ---------------------------------------------------------------------------
#+:allegro
(defun make-c-int-vector (length)
  #+:lisp-doc
  "\\Purposelabel
 Allocate a int-vector.
\\Seealsolabel
 \\Fcite{make-int-vector}."
  #-:c-int-vector
  (make-array length
	      :element-type +c-int-type+
	      :initial-element +ignore-slot-tag+)
  #+:c-int-vector
  (let ((vector
	   #+(and :allegro (not (version>= 5)))
	   (excl::malloc (* 4 (1+ length)))
	   #+(and :allegro (version>= 5))
	   (excl:aclmalloc (* 4 (1+ length)))
	   ))
    (setf (c-int-vector-length vector) length)
    vector))

;;; ---------------------------------------------------------------------------
#+:allegro
(defun compute-c-int-vector-location (x location)
  (let ((l (c-int-vector-length x)))
    (cond
     ((minusp l)
      (error "Trying to access free'd vector at index ~A" location))
     ((or (< location 0) (>= location l))
      (error "Index ~A out of vector length ~A" location l)))
    (1+ location)))

;;; ---------------------------------------------------------------------------
#+:allegro
(defun c-int-vector (x location)
  #-:c-int-vector
  (aref x location)
  #+:c-int-vector
  (internal-c-int-vector x (compute-c-int-vector-location x location)))

;;; ---------------------------------------------------------------------------
#+:allegro
(defun (setf c-int-vector) (new-value x location)
  #-:c-int-vector
  (setf (aref x location) new-value)
  #+:c-int-vector
  (setf (internal-c-int-vector x (compute-c-int-vector-location x location))
    new-value))

;;; ---------------------------------------------------------------------------
#+:allegro
(defun c-int-vector-length (x)
  #-:c-int-vector
  (length x)
  #+:c-int-vector
  (internal-c-int-vector x 0))

;;; ---------------------------------------------------------------------------
#+:allegro
(defun (setf c-int-vector-length) (new-length x)
  #-:c-int-vector
  new-length
  #+:c-int-vector
  (setf (internal-c-int-vector x 0) new-length))

;;; ---------------------------------------------------------------------------
#+:allegro
(defun c-int-vector-fill (x new-value)
  #-:c-int-vector
  (fill x new-value)
  #+:c-int-vector
  (loop for i from 1 to (c-int-vector-length x)
      finally (return x)
      do (setf (internal-c-int-vector x i) new-value)))

;;; ---------------------------------------------------------------------------
(defun make-int-vector (length &optional (dynamic t))
  #+:lisp-doc "
\\Purposelabel
  Make a vector of \\funarg{length}\\ elements holding signed C integers.
\\Seealsolabel
  \\Fcite{free-int-vector}."
  (let ((int-vector (or #+:lispworks4 ;; or later
			(fli:allocate-foreign-object
			 :type `(:c-array :signed ,length)
			 ;; :initial-element +ignore-slot-tag+
			 )
			#+:allegro
			(make-c-int-vector length)
			(make-array length
				    :element-type +c-int-type+
				    :initial-element +ignore-slot-tag+))))
    #+(or :lispworks4 (and :allegro :c-int-vector))
    (int-vector-fill int-vector +ignore-slot-tag+)
    (when dynamic
      (push int-vector *dynamic-allocated-int-vectors*))
    int-vector))

;;; ---------------------------------------------------------------------------
(defun free-int-vector (vector)
  #+:lisp-doc
  "Free memory occupied by \\funarg{vector}."
  (or
   #+:lispworks4 ;; or later
   (progn
     (fli:free-foreign-object vector)
     t)
   #+(and :allegro :c-int-vector)
   (progn
     #+(and :allegro (not (version>= 5)))
     (excl::free vector)
     #+(and :allegro (version>= 5))
     (excl::aclfree vector)
     t)
   t))

;;; ---------------------------------------------------------------------------
(defmacro with-dynamic-int-vectors (bindings &body body)
  #+:lisp-doc
  "Handle dynamic extent on int vectors.
\\Seealsolabel
 \\Fcite{make-int-vector}; \\fcite{free-int-vector}."

  (let ((result (gensym "RESULT-")))
    `(let ((*dynamic-allocated-int-vectors* nil))
       (declare (special *dynamic-allocated-int-vectors*))
       (let ((,result nil)
	     ,@bindings)
         ,@(when bindings
	     #-:lispworks4 ;; and hopefully not later
             `((declare (dynamic-extent ,@(map 'list #'car bindings)))))
         (unwind-protect
             (setf ,result (multiple-value-list (progn ,@body)))
           (map nil #'free-int-vector
		*dynamic-allocated-int-vectors*))
       (values-list ,result)))))

;;; ---------------------------------------------------------------------------
(defun int-vector (vector location)
  #+:lisp-doc
  "Return the element at \\funarg{location}\\ in \\funarg{vector}."
  (declare (inline int-vector))
  (or #+:lispworks4 ;; or later
      (fli:foreign-aref vector location)
      #+:allegro
      (c-int-vector vector location)
      (aref vector location)))

;;; ---------------------------------------------------------------------------
(defun (setf int-vector) (new-value vector location)
  #+:lisp-doc
  "Set the element at \\funarg{location}\\ in \\funarg{vector}\\ 
 to \\funarg{new-value}."
  (or #+:lispworks4 ;; or later
      (setf (fli:foreign-aref vector location) new-value)
      #+:allegro
      (setf (c-int-vector vector location) new-value)
      (setf (aref vector location) new-value)))

;;; ---------------------------------------------------------------------------
(defun int-vector-length (vector)
  #+:lisp-doc
  "Return the number of elements in \\funarg{vector}."
  (or #+:lispworks4 ;; or later
      (car (fli:foreign-array-dimensions vector))
      #+:allegro
      (c-int-vector-length vector)
      (length vector)))

;;; ---------------------------------------------------------------------------
(defun int-vector-fill (vector item)
  #+:lisp-doc
  "Set all elements in \\funarg{vector}\\ to \\funarg{item}."
  (or #+:lispworks4 ;; or later
      (dotimes (i (int-vector-length vector) vector)
	(setf (int-vector vector i) item))
      #+:allegro
      (c-int-vector-fill vector item)
      (fill vector item)))

;;; ---------------------------------------------------------------------------
(defun int-vector-to-c-pointer (vector)
  #+:lisp-doc
  "Return \\funarg{vector}\\ converted into a C pointer
 addressing the vector."
  (or #+(and :allegro :c-int-vector)
      (+ vector 4)			; Add 4 bytes for length element
      vector))

;;; ---------------------------------------------------------------------------
(defun int-vector-to-vector (vector)
  #+:lisp-doc
  "Return \\funarg{vector}\\ converted into a vector."
  (if (vectorp vector)
      (copy-seq vector)
    (let* ((l (int-vector-length vector))
           (v (make-array l :element-type +c-int-type+)))
      (dotimes (i l v)
        (setf (aref v i) (int-vector vector i))))))

;;; ---------------------------------------------------------------------------
(defun vector-to-int-vector (vector &optional (dynamic t))
  #+:lisp-doc
  "Return \\funarg{vector}\\ converted into a int-vector."
    (let* ((l (length vector))
           (v (make-int-vector l dynamic)))
      (dotimes (i l v)
        (setf (int-vector v i) (aref vector i)))))

;;; ---------------------------------------------------------------------------
;;; Implementation: Methods
;;; ---------------------------------------------------------------------------

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
