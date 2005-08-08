;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-symbol.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	10.2.94
;;;; Description	PLOB functions for symbols
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
;;; Symbol
;;; ---------------------------------------------------------------------------

#+(and :lisp-doc :document-api)
(:defdoc
 "symbol ..."
 "Persistent Symbols"
 "
\\Purposelabel
 \\plob\\ offers persistent symbols as known in its transient form in
 \\cl; each symbol belongs to a package, has a property list, a function
 and a value.

 To make the handling of symbols easier, the macro character
 \\#! (hash sign followed by an exclamation mark) is used. Behind this
 letter, the name of a persistent symbol is expected; this can be
 e.g.\\ a transient symbol. Its expansion is
 \\begin{quote}\\tt
  (p-symbol-value \\textsl{\\lt{}persistent-symbol-name\\gt}\\/)
 \\end{quote}
 This form can be used to read the value of a persistent symbol;
 the \\lisp{setf}-form can be used to set the value of a persistent
 symbol.

 The objects referenced by a persistent symbol are of course persistent
 too.
\\Exampleslabel
 Store \\cl\\ data to the persistent symbol *p*:
 \\begin{quote}\\tt
(setf \\#!*p* \"This string is persistent.\")
 \\end{quote}
 Evaluating the above statement may produce an error message like
 \\begin{quote}\\tt
  Error: Persistent package COMMON-LISP-USER does not exist.
 \\end{quote}
 This is not really an error condition but the error is raised as a
 `security valve' to escape from creating unwanted persistent
 packages (otherwise, since packages are persistent, too many
 unused packages might litter the \\sh).

 Evaluating the following statement yields the value of the
 persistent symbol *p*:
 \\begin{quote}\\tt
\\#!*p*
        ==> \"This string is persistent.\"
 \\end{quote}

 Please note that there is a difference between a {\\sl transient}
 and a {\\sl persistent symbol}\\/: Both can have an own
 value, plist and function binding; there is no `tight coupling' between
 the transient and the persistent symbol.
\\Seealsolabel
 \\Fcite{persistent-symbol-reader};
 \\fcite{p-symbol}.")

;;; ---------------------------------------------------------------------------
(defun p-allocate-symbol (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new persistent
 symbol
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{p-allocate}."

  (p-allocate p-heap +symbol-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-symbolp (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{symbolp}."

  (= (p-type-tag-of p-objid p-heap) +symbol-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-boundp (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{boundp}."

  (with-transaction (p-heap)
    (block nil
      (when (or (symbolp p-objid) (stringp p-objid))
	(let ((p-objid-symbol (p-find-symbol p-objid
					     :depth :objid
					     :p-heap p-heap)))
	  (if p-objid-symbol
	      (setf p-objid p-objid-symbol)
	    (return nil))))
      (not (p-marker p-heap p-objid
		     +symbol-location-value+ +symbol-type-tag+)))))

;;; ---------------------------------------------------------------------------
(defun p-fboundp (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{fboundp}."

  (with-transaction (p-heap)
    (block nil
      (when (or (symbolp p-objid) (stringp p-objid))
	(let ((p-objid-symbol (p-find-symbol p-objid
					     :depth :objid
					     :p-heap p-heap)))
	  (if p-objid-symbol
	      (setf p-objid p-objid-symbol)
	    (return nil))))
      (not (p-marker p-heap p-objid
		     +symbol-location-function+ +symbol-type-tag+)))))

;;; --- symbol function  ------------------------------------------------------

(defun (setf p-symbol-function) (t-symbol-function
                                 p-objid
                                 &optional (depth *default-depth*)
                                 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-symbol-function}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{(setf symbol-function)}."

  (t-slot-to-p-objid t-symbol-function depth p-heap p-objid
		     +symbol-location-function+ nil +symbol-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-symbol-function (p-objid
                          &optional (depth *default-depth*)
                          (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{symbol-function}."

  (p-objid-to-t-slot p-objid +symbol-location-function+ depth p-heap
		     nil +symbol-type-tag+))

;;; --- symbol package --------------------------------------------------------

(defun p-symbol-package (p-objid
			 &optional (depth *default-depth*)
                         (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{symbol-package}."

  (p-objid-to-t-slot p-objid +symbol-location-package+ depth p-heap
		     nil +symbol-type-tag+))

;;; --- symbol plist ----------------------------------------------------------

(defun (setf p-symbol-plist) (t-symbol-plist
			      p-objid
			      &optional (depth *default-depth*)
                              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-symbol-plist}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{(setf symbol-plist)}."

  (t-slot-to-p-objid t-symbol-plist depth p-heap p-objid
		     +symbol-location-plist+ nil +symbol-type-tag+))

;;; ---------------------------------------------------------------------------
(defun p-symbol-plist (p-objid
		       &optional (depth *default-depth*)
                       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{symbol-plist}."

  (p-objid-to-t-slot p-objid +symbol-location-plist+ depth p-heap nil
		     +symbol-type-tag+))

;;; --- symbol name -----------------------------------------------------------

(defun p-symbol-name (p-objid
		      &optional (depth *default-depth*)
                      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{symbol-name}."

  (let ((name (p-objid-to-t-slot p-objid +symbol-location-name+
				 depth p-heap nil +symbol-type-tag+)))
    (if (stringp name)
	(external-to-internal-name name)
      name)))

;;; --- symbol value ----------------------------------------------------------

(defun (setf p-symbol-value) (t-symbol-value
			      p-objid
			      &optional (depth *default-depth*)
                              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-symbol-value}}
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{(setf symbol-value)}."

  (when (or (symbolp p-objid) (stringp p-objid))
    (setf p-objid (p-intern p-objid)))
  (t-slot-to-p-objid t-symbol-value depth p-heap p-objid
		     +symbol-location-value+ nil +symbol-type-tag+) )

;;; ---------------------------------------------------------------------------
(defun pretty-print-symbol (t-symbol)
  #+:lisp-doc "Pretty print a symbol."
  (let ((t-package (symbol-package t-symbol)))
    (if (eq t-package (find-package :keyword))
	(format nil ":~A" t-symbol)
      (let ((t-package-name (package-name t-package)))
	(multiple-value-bind (found external)
	    (intern (symbol-name t-symbol) (symbol-package t-symbol))
	  (declare (ignore found))
	  (if (eq external :external)
	      (format nil "~A:~A" t-package-name t-symbol)
	    (format nil "~A::~A" t-package-name t-symbol)))))))

;;; ---------------------------------------------------------------------------
(defun p-extern (name-symbol)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{name-symbol}}
      {a string or a symbol}
\\Valueslabel
 Returns the \\objid\\ of the persistent symbol named
 \\funarg{name-symbol}.
\\Purposelabel
 Checks if \\funarg{name-symbol}\\ names a persistent
 symbol; if not, a \\lisp{cerror}\\ is raised and the
 user is asked if a persistent symbol named
 \\funarg{name-symbol}\\ should be created.
\\Seealsolabel
 \\Fcite{p-find-symbol};
 \\fcite{p-intern};
 \\fcite{p-unintern}."

  (let ((p-objid (p-find-symbol name-symbol :depth :objid)))
    (unless p-objid
      (if (and *verbose* (>= *verbose* 1))
	  (restart-case
	      (error "Cannot locate a persistent symbol named ~S." name-symbol)
	    (continue
		(value)
		:report
		  "Allocate a persistent symbol and supply a value for it."
		:interactive
		  (lambda ()
		    (list (prompt-for t "Value for persistent symbol named ~S:"
				      name-symbol)))
	      (setf p-objid (p-intern name-symbol))
	      (setf (p-symbol-value p-objid *default-depth*
				    *default-persistent-heap*)
		value))
	    (allocate-symbol
		()
		:report
		  "Only allocate a persistent symbol."
	      (setf p-objid (p-intern name-symbol))))
	(setf p-objid (p-intern name-symbol))))
    p-objid))

;;; ---------------------------------------------------------------------------
(defun p-symbol-value (p-objid
		       &optional (depth *default-depth*)
                       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{symbol-value}."

  (when (or (symbolp p-objid) (stringp p-objid))
    (setf p-objid (p-extern p-objid)))
  (p-objid-to-t-slot p-objid +symbol-location-value+ depth p-heap nil
		     +symbol-type-tag+))

;;; --- symbol ----------------------------------------------------------------

(defun store-symbol (t-symbol p-objid depth p-heap)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-symbol}}
      {a symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-symbol}}
\\Purposelabel
 Store the transient symbol in \\funarg{t-symbol}\\ to the
 persistent symbol referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-symbol}."

  (with-transaction (p-heap)
    (let ((old-symbol-objid (p-find-symbol (symbol-name t-symbol)
					   :package (symbol-package t-symbol)
					   :depth :objid
					   :p-heap p-heap))
	  (to-store nil) (force-write nil))
      (unless p-objid
	(setf p-objid old-symbol-objid)
	(unless p-objid
	  (setf p-objid (is-registered-object t-symbol))
	  (unless p-objid
	    (setf p-objid (p-allocate-symbol p-heap))
	    (setf force-write t))))
      (with-write-lock (p-heap p-objid t-symbol depth
			       +symbol-type-tag+ force-write)
	(if old-symbol-objid
	    (when (/= p-objid old-symbol-objid)
	      (error
	       "Persistent symbol named ~A already registered with objid ~A."
	       t-symbol old-symbol-objid))
	  ;; The symbol was not registered up to now;
	  ;; Create and set symbol's name:
	  (let* ((p-objid-name 
		  (t-object-to-p-objid (internal-to-external-name
					(symbol-name t-symbol))
				       :cached p-heap))
		 (package (symbol-package t-symbol))
		 (p-objid-package (when package
				    (t-object-to-p-objid package
							 :objid
							 p-heap))))
	    #-:lispworks4 ;; and hopefully not later
	    (declare (dynamic-extent p-objid-name package p-objid-package))
	    (setf to-store t)
	    (t-slot-to-p-objid-in-transaction p-objid-name :objid
					      p-heap p-objid
					      +symbol-location-name+
					      nil +symbol-type-tag+)
	    ;; Set symbol's package:
	    (when package
	      (t-slot-to-p-objid-in-transaction p-objid-package :objid
						p-heap p-objid
						+symbol-location-package+
						nil +symbol-type-tag+)
	      (setf (getbtree-by-objid
		     p-objid-name
		     (p-package-internals p-objid-package :objid p-heap)
		     :objid p-heap)
		p-objid))))
	(if (constantp t-symbol)
	    ;; Set symbol's value:
	    (progn
	      (setf to-store t)
	      (setf (p-symbol-value p-objid :objid p-heap) p-objid))
	  (when (eq depth :deep)
	    ;; Set symbol's value:
	    (if (boundp t-symbol)
		(setf (p-symbol-value p-objid depth p-heap)
		  (symbol-value t-symbol))
	      (setf (p-marker p-heap p-objid +symbol-location-value+)
		+unbound-type-tag+))
	    ;; Set symbol's function:
	    (if (fboundp t-symbol)
		(setf (p-symbol-function p-objid depth p-heap)
		  (symbol-function t-symbol))
	      (setf (p-marker p-heap p-objid +symbol-location-function+)
		+unbound-type-tag+))
	    ;; Set symbol's plist:
	    (setf (p-symbol-plist p-objid depth p-heap)
	      (symbol-plist t-symbol)))))
      (when (and *verbose* (>= *verbose* 6) to-store)
	(format t "; Stored symbol ~A~%" (pretty-print-symbol t-symbol)))))
  p-objid)

;;; ---------------------------------------------------------------------------
(defun (setf p-symbol)
    (t-symbol &optional p-objid (depth *default-depth*)
			(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-symbol}}
      {a symbol}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 \\retarg{\\funarg{t-symbol}}
\\Purposelabel
 Store the transient symbol in \\funarg{t-symbol}\\ to the
 persistent symbol referenced by \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-symbol}."

  (values t-symbol (store-symbol t-symbol p-objid depth p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-symbol (p-objid
		 &optional (depth *default-depth*)
		 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 Return a transient representation for the persistent
 symbol
 referenced by \\funarg{p-objid}.
\\Remarkslabel
 If there is already a symbol found in the transient \\cl\\ image with
 the same name and package as the persistent symbol referenced by
 \\funarg{p-objid}, the persistent symbol is {\\sl not} loaded from
 the \\sh. Such a load would `clobber' the existing transient symbol
 which could be a little too dangerous.
\\Seealsolabel
 \\Fcite{(setf p-symbol)}."

  (if *in-bootstrap-p*
      (bootstrap-p-symbol p-objid p-heap)
    (multiple-value-bind (object found)
        (is-registered-objid p-objid)
      (let ((from-store nil))
        (with-transaction
         (p-heap)
         (with-read-lock
          (p-heap p-objid depth +symbol-type-tag+ (not found))
          (setf from-store t)
          (let* ((new-symbol-package 
                  (unless (eql (p-marker p-heap p-objid
                                         +symbol-location-package+)
                               +unbound-type-tag+)
                    (p-symbol-package p-objid *default-depth* p-heap)))
                 (new-symbol-package-name
                  (when new-symbol-package
                    (p-package-name new-symbol-package :cached p-heap)))
                 (new-symbol-name (p-symbol-name p-objid :cached p-heap))
                 (existing-symbol nil))
            (unless (or (null new-symbol-package-name)
                        (find-package new-symbol-package-name))
              ;; The new-symbol-package-name is non-NIL and the
              ;; transient package wasn't found; so create the
              ;; package and supply a warning that a new package
              ;; was created:
              (make-package new-symbol-package-name)
              (when (and *verbose* (>= *verbose* 1))
                (warn "PLOB created package ~A at loading symbol ~A"
                      new-symbol-package-name new-symbol-name)))
            (if (and new-symbol-package new-symbol-package-name)
                ;; The symbol has a package it belongs to:
                (multiple-value-setq (object existing-symbol)
                    (intern new-symbol-name new-symbol-package-name))
              ;; The symbol has no package, so create an uninterned symbol
              ;; with make-symbol:
              (progn
                (setf object (make-symbol new-symbol-name))
                (when (and *verbose* (>= *verbose* 1))
                  (warn "Loaded uninterned symbol ~A" object))))
            (unless found
              (register-to-cache p-objid object))
            (unless existing-symbol
              ;; The symbol was created by one the above calls.
              (unless (constantp object)
                ;; Set symbol's value:
                (if (eql (p-marker p-heap p-objid +symbol-location-value+)
                         +unbound-type-tag+)
                    (makunbound object)
                  (setf (symbol-value object)
                        (p-symbol-value p-objid depth p-heap))))
              (when (eq depth :deep)
                ;; Set symbol's plist:
                (unless (eql (p-marker p-heap p-objid
                                       +symbol-location-plist+)
                             +unbound-type-tag+)
                  (setf (symbol-plist object)
                        (p-symbol-plist p-objid depth p-heap)))
                ;; Set symbol's function:
                (if (eql (p-marker p-heap p-objid +symbol-location-function+)
                         +unbound-type-tag+)
                    (fmakunbound object)
                  (setf (fdefinition object)
                        (p-symbol-function p-objid depth p-heap))))))))
        (when (and *verbose* (>= *verbose* 6) from-store)
	  (format t "; Loaded symbol ~A~%" (pretty-print-symbol object)))
        object))))

;;; ---------------------------------------------------------------------------
(defun p-fmakunbound (t-symbol
		      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-symbol}}
      {a (transient) symbol}
\\Purposelabel
 Make the function cell of the persistent symbol named like
 \\funarg{t-symbol}\\ unbound.
\\Seealsolabel
 \\Fcite{fmakunbound}."

  (with-transaction (p-heap)
    (let ((p-symbol (p-find-symbol t-symbol
				   :package (symbol-package t-symbol)
				   :depth :object
				   :p-heap p-heap)))
      (when p-symbol
	(setf (p-marker p-heap p-symbol +symbol-location-function+)
	  +unbound-type-tag+))
      p-symbol)))

;;; ---------------------------------------------------------------------------
(defun p-makunbound (t-symbol
		     &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{t-symbol}}
      {a (transient) symbol}
\\Purposelabel
 Make the value cell of the persistent symbol named like
 \\funarg{t-symbol}\\ unbound.
\\Seealsolabel
 \\Fcite{makunbound}."

  (with-transaction (p-heap)
    (let ((p-symbol (p-find-symbol t-symbol
				   :package (symbol-package t-symbol)
				   :depth :object
				   :p-heap p-heap)))
     (when p-symbol
       (setf (p-marker p-heap p-symbol +symbol-location-value+)
	 +unbound-type-tag+))
     p-symbol)))

;;; ---------------------------------------------------------------------------
(defconstant +p-setq-error-prompt+ "Cannot p-setq ~S -- not a symbol."
  #+:lisp-doc "Prompt which is shown in the error message concerning invalid symbols.")

;;; ---------------------------------------------------------------------------
(defmacro p-setq (symbol value
		         &optional (depth *default-depth*)
		         (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 See \\fcite{setq}."

  (let ((saved-symbol (gensym "SAVED-SYMBOL-"))
        (saved-p-heap (gensym "SAVED-P-HEAP-")))
    `(let* ((,saved-symbol (quote ,symbol))
            (,saved-p-heap ,p-heap))
       (unless (symbolp ,saved-symbol)
         (error +p-setq-error-prompt+ ,saved-symbol))
       (setf (p-symbol-value ,saved-symbol ,depth ,saved-p-heap)
             ,value))))

;;; ---------------------------------------------------------------------------
;;; Storing of symbols
;;; ---------------------------------------------------------------------------

(defmethod t-object-to-p-objid ((t-object symbol) depth to-p-heap)
  (store-symbol t-object nil depth to-p-heap))
  
;;; ---------------------------------------------------------------------------
;;; Loading of symbols
;;; ---------------------------------------------------------------------------

(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +symbol-type-tag+))
				depth p-heap)
  (p-symbol p-objid depth p-heap))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
