;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-function.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	18.2.94
;;;; Description	PLOB functions for functions
;;;;
;;;; Copyright	PLOB! Copyright 1994--2006 Heiko Kirschke.
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
;;;; $Id$
;;;;
;;;; --------------------------------------------------------------------------

(in-package :plob)

;;; ---------------------------------------------------------------------------
;;; String
;;; ---------------------------------------------------------------------------

(defun p-allocate-function (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 function
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (p-allocate p-heap +function-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-functionp (p-objid
                    &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{functionp}."

  (= (p-type-tag-of p-objid p-heap) +function-type-tag+))

;;; --- function name ---------------------------------------------------------

(defun (setf p-function-name) (t-name
                               p-objid
                               &optional (depth *default-depth*)
                               (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-name}}
      {a symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the name of the persistent function referenced by
 \\funarg{p-objid} to \\funarg{t-name}.
\\Seealsolabel
 \\Fcite{p-function-name};
 \\fcite{get-function-name}."

  (t-slot-to-p-objid t-name depth p-heap p-objid +function-location-name+
		     nil +function-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-function-name (p-objid
                        &optional (depth *default-depth*)
	                (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the name of the persistent function referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-function-name)};
 \\fcite{get-function-name}."

  (p-objid-to-t-slot p-objid +function-location-name+ depth p-heap nil
		     +function-type-tag+))

;;; --- function language -----------------------------------------------------

(defun (setf p-function-language) (t-language
                                   p-objid
                                   &optional (depth *default-depth*)
                                   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-language}}
      {a keyword symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the language of the persistent function referenced by
 \\funarg{p-objid} to \\funarg{t-language}.
\\Seealsolabel
 \\Fcite{p-function-language};
 \\fcite{+lisp-code-type+}."

  (t-slot-to-p-objid t-language depth p-heap p-objid
		     +function-location-language+ nil +function-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-function-language (p-objid
                            &optional (depth *default-depth*)
	                    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the language of the persistent function referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-function-language)}."

  (p-objid-to-t-slot p-objid +function-location-language+ depth p-heap
		     nil +function-type-tag+))

;;; --- function code ---------------------------------------------------------

(defun (setf p-function-code) (t-code
                               p-objid
                               &optional (depth *default-depth*)
                               (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-code}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Set the code of the persistent function referenced by
 \\funarg{p-objid} to \\funarg{t-code}.
\\Seealsolabel
 \\Fcite{p-function-code}."

  (t-slot-to-p-objid t-code depth p-heap p-objid +function-location-code+
		     nil +function-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-function-code (p-objid
                        &optional (depth *default-depth*)
	                (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Get the code of the persistent function referenced by
 \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-function-code)}."

  (p-objid-to-t-slot p-objid +function-location-code+ depth p-heap nil
		     +function-type-tag+))

;;; ---------------------------------------------------------------------------
(defvar *ignore-missing-function-name* nil
  #+:lisp-doc "
 Flag if unnamed functions should be stored as unloadable objects.")

;;; ---------------------------------------------------------------------------
(defun store-function (t-function p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-function}}
      {a function}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-function}}
\\Purposelabel
 Store the transient function in \\funarg{t-function}\\ to the
 persistent function referenced by \\funarg{p-objid}.
\\Remarkslabel
 \\plob\\ cannot store code because of relocation and binder
 problems which would occure when the code is subject to be
 loaded again; instead, a function is stored by its name.
 At function load time \\plob\\ tries to find an \\lisp{equal}\\ named
 function in the \\cl\\ image.
\\Seealsolabel
 \\Fcite{p-function};
 \\fcite{get-function-name}."

  (declare (type function t-function))
  (let ((force-write nil))
    (unless p-objid
      (setf p-objid (is-registered-object t-function))
      (unless p-objid
	(setf p-objid (p-allocate-function p-heap))
	(setf force-write t)))
    (with-transaction (p-heap)
      (let ((name (get-function-name t-function)))
	(with-write-lock
	    (p-heap p-objid t-function
		    (if (and name (symbolp name)) :nocache depth)
		    +function-type-tag+ force-write)
	  (setf (p-function-language p-objid :cached p-heap) +lisp-code-type+)
	  (cond
	   ((and name (symbolp name))
	    ;; For a function named by a symbol, the function's identity
	    ;; is represented by its name, not by its objid:
	    (let ((p-objid-name (p-intern name :p-heap p-heap)))
	      (multiple-value-bind (p-objid-function p-type-tag-function)
		  (p-symbol-function p-objid-name :objid p-heap)
		(if (and p-objid-function
			 (eq p-type-tag-function +function-type-tag+))
		    (setf p-objid p-objid-function)
		  (setf (p-symbol-function p-objid-name :objid p-heap)
		    p-objid)))
	      (setf (p-function-name p-objid :objid p-heap) p-objid-name))
	    (register-to-cache p-objid t-function))
	   ((and name (or (consp name) (stringp name)))
	    (setf (p-function-name p-objid :cached p-heap) name))
	   (*ignore-missing-function-name*
	    (setf (p-marker p-heap p-objid +function-location-name+)
	      +unbound-type-tag+))
	   (t
	    (if (and *verbose* (>= *verbose* 1))
		(restart-case
		    (error "Cannot get name of function ~A.
Hint for LispWorkers: Compiling interpreted functions might suppress this error."
			   t-function)
		  (continue
		      ()
		      :report
			"Mark this function and all others unnamed following as unloadable."
		    (setf *ignore-missing-function-name* t)
		    (setf (p-marker p-heap p-objid +function-location-name+)
		      +unbound-type-tag+))
		  (unloadable
		      ()
		      :report
			"Mark this function as unloadable; re-signal this error."
		    (setf (p-marker p-heap p-objid +function-location-name+)
		      +unbound-type-tag+))
		  (enter-function-name
		      (entered-name)
		      :report
			"Enter a symbolic name for the function."
		      :interactive
			(lambda ()
			  (list (prompt-for 'symbol)))
		    (setf (p-function-name p-objid :cached p-heap)
		      entered-name)))
	      (setf (p-marker p-heap p-objid +function-location-name+)
		+unbound-type-tag+))))))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-function)
    (t-function &optional p-objid (depth *default-depth*)
			  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-function}}
      {a function}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-function}}
\\Purposelabel
 Store the transient function in \\funarg{t-function}\\ to the
 persistent function referenced by \\funarg{p-objid}.
\\Remarkslabel
 \\plob\\ cannot store code because of relocation and binder
 problems which would occure when the code is subject to be
 loaded again; instead, a function is stored by its name.
 At function load time \\plob\\ tries to find an \\lisp{equal}\\ named
 function in the \\cl\\ image.
\\Seealsolabel
 \\Fcite{p-function};
 \\fcite{get-function-name}."

  (declare (type function t-function))
  (values t-function (store-function t-function p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun make-unloadable-function-stub (p-objid)
  #+:lisp-doc "
\\Purposelabel
 Returns a $\\lambda$-expression which signals an error like
 \\begin{quote}\\tt
 Unloadable \\plob\\ function \\funarg{p-objid}\\ called \\ldots
 \\end{quote}
 on being called. This is used as code for a function whose name
 was not found at load time in the current \\cl\\ image.
\\Seealsolabel
 \\Fcite{p-function}."

  #'(lambda (&rest args)
      (when (and *verbose* (>= *verbose* 1))
	(cerror "Return no values."
		"Unloadable PLOB function ~A called with arguments ~A."
		p-objid args))
      (values)))

;;; ---------------------------------------------------------------------------
(defun p-function (p-objid
		   &optional (depth *default-depth*)
		   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 function
 referenced by \\funarg{p-objid}.
\\Remarkslabel
 \\plob\\ cannot load code because of relocation and binder
 problems; instead, a function is stored by its name.
 At function load time \\plob\\ tries to find an \\lisp{equal}\\ named
 function in the \\cl\\ image.
\\Seealsolabel
 \\Fcite{(setf p-function)};
 \\fcite{get-function-name}."

  (let ((t-function (is-registered-objid p-objid)))
    (unless t-function
      (with-transaction (p-heap)
	(with-read-lock (p-heap p-objid depth +function-type-tag+ t)
	  (let ((name (unless (p-marker p-heap
					p-objid
					+function-location-name+
					+function-type-tag+)
			(p-function-name p-objid :cached p-heap)))
		#+:store-functions
		(language (p-function-language p-objid :cached p-heap)))
	    (when name
	      #+:store-functions
	      (unless (and (eq language +lisp-code-type+)
			   *verbose* (>= *verbose* 1))
		(cerror "Try to load it anyway."
			"Function named ~A is no ~A but a ~A function."
			name +lisp-code-type+ language))
	      (when (fboundp name)
		(setf t-function (symbol-function name))))
	    (cond
	     (t-function
	      nil)
	     (*ignore-missing-function-name*
	      (setf t-function (make-unloadable-function-stub p-objid)))
	     (t
	      (if (and *verbose* (>= *verbose* 1))
		  (restart-case
		      (error "Function ~A is not loadable."
			     (make-persistent-object
			      (persistent-object-objid p-objid)))
		    (continue
			()
			:report
			  "Return a function stub for this and all following unloadable functions which signals an error on being called."
		      (setf *ignore-missing-function-name* t)
		      (setf t-function
			(make-unloadable-function-stub p-objid)))
		    (unloadable
			()
			:report
			  "Return a function stub only for this unloadable functions which signals an error on being called."
		      (setf t-function
			(make-unloadable-function-stub p-objid))))
		(setf t-function
		  (make-unloadable-function-stub p-objid)))))))))
      (when t-function
	(register-to-cache p-objid t-function))
    t-function))

;;; ---------------------------------------------------------------------------
;;; Storing of functions
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object function) depth to-p-heap)
  (store-function t-object nil depth to-p-heap))

;;; ---------------------------------------------------------------------------
;;; Loading of functions
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +function-type-tag+))
				depth p-heap)
  (p-function p-objid depth p-heap))


;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
