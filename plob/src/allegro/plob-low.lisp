;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-low.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	15.11.93
;;;; Description	More LISP suitable interface to PLOB stable heap.
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
;;; Constants
;;; ---------------------------------------------------------------------------
(defconstant +plob-bin-directory+
    (logical-pathname "PLOB:BIN;")
  #+:lisp-doc "
\\Purposelabel
 Directory containing some auxiliary binary files.")

;;; ---------------------------------------------------------------------------
(defconstant +string-buffer-size+ 256
  #+:lisp-doc "
\\Purposelabel
 Size of the string buffers used for returning strings from
 the \\plob\\ low-level functions.")

;;; ---------------------------------------------------------------------------
(defconstant +type-name-buffer-size+ 128
  #+:lisp-doc "Max.\\ number of characters in a type name.")

;;; ---------------------------------------------------------------------------
(defun sh-map-class-info (enum-function)
  #+:lisp-doc "
\\Argumentslabel
 The \\funarg{enum-function}\\ argument is
 a function taking four arguments:
 \\begin{description}
 \\item[\\funarg{type-tag}]
   This is the \\typetag\\ of the \\plob\\ C level
   built-in class.
 \\item[\\funarg{type-name}]
   This is a string naming the type identified by
   \\funarg{type-tag}.
 \\item[\\funarg{objid-size}]
   This is the number of reference words in units of bits for
   instances of the type identified by \\funarg{type-tag}.
   This value is always a multiple of~32.
 \\item[\\funarg{value-size}]
   This is the number of value bits for instances of
   the type identified by \\funarg{type-tag}.
 \\end{description}
\\Purposelabel
 The \\funarg{enum-function}\\ is called for each \\plob\\ C level
 built-in class."

  (loop as  type-tag of-type fixnum = 0
      and type-name-buffer = (make-string +type-name-buffer-size+
					  :initial-element #\Space)
      and type-name = nil
      and objid-size of-type fixnum = 0
      and value-size of-type fixnum = 0
      and type-flags of-type fixnum = 0
      for iterated fixnum from 0 by 1
      finally (return iterated)
      for done = (multiple-value-setq
		     (done type-tag type-name objid-size value-size type-flags)
		   (catch-errors 
		    (c-sh-map-class-info-first
		     type-tag
		     type-name-buffer +type-name-buffer-size+
		     objid-size value-size type-flags)))
      then
	(multiple-value-setq
	    (done type-tag type-name objid-size value-size type-flags)
	  (catch-errors 
	   (c-sh-map-class-info-next
	    type-tag type-name-buffer +type-name-buffer-size+
	    objid-size value-size type-flags)))
      while (= done +c-true+)
      do
	(unless (funcall enum-function type-tag type-name
			 objid-size value-size)
	  (return (1+ iterated)))))

;;; ---------------------------------------------------------------------------
(defun sh-can-modify (p-heap-objid p-objid)
  #+:lisp-doc "
\\Seealsolabel
 \\shcite{function}{SH\\us{}can\\us{}modify}{2}."

  (declare (type fixnum p-heap-objid p-objid))
  (/= (catch-errors (c-sh-can-modify p-heap-objid p-objid)) +c-false+))

;;; ---------------------------------------------------------------------------
(defstruct stableheap-configuration
  #+:lisp-doc "
\\Purposelabel
 This is the \\cl\\ equivalent of the C
 \\shcite{struct}{stableheap\\us{}configuration}{1}."

  (configuration-flags 0
		       :type fixnum
		       #+:lisp-doc :documentation #+:lisp-doc "
 Bitmask with configuration flags; its value is a bitwise-or of the constants
 \\lisp{KEY\\us{}TO\\us{}ADDRESS}\\ \\ldots\\ \\lisp{INCREMENTAL\\us{}GC}\\ %
 defined in \\cite[\\citepage{1}]{bib:Brown-92}.")

  (minimum-key 0
	       :type fixnum
	       #+:lisp-doc :documentation #+:lisp-doc "
 Minimum \\objid\\ allocated by the \\postore.")

  (maximum-key 0
	       :type fixnum
	       #+:lisp-doc :documentation #+:lisp-doc "
 Maximum \\objid\\ allocated by the \\postore. \\note\\ The \\objid[s]\\ allocated
 by \\postore\\ start at {\\bf maximum-key} going down to {\\bf minimum-key}.")

  (key-alignment 0
		 :type fixnum
		 #+:lisp-doc :documentation #+:lisp-doc "
 The alignment of the allocated \\objid[s]."))

;;; ---------------------------------------------------------------------------
(defun sh-configuration (p-heap-objid)
  #+:lisp-doc "
\\Purposelabel
 Returns an instance of \\fcite{stableheap-configuration}\\ containing
 \\sh\\ configuration informations.
\\Seealsolabel
 \\Fcite{stableheap-configuration};
 \\fcite{sh-statistics};
 \\shcite{function}{SH\\us{}configuration}{3}."

  (declare (type fixnum p-heap-objid))
  (multiple-value-bind (done configuration-flags
			     minimum-key maximum-key key-alignment)
      (catch-errors (c-sh-configuration p-heap-objid
					0 0 0 0))
    (when (/= done +c-false+)
      (make-stableheap-configuration
       :configuration-flags configuration-flags
       :minimum-key minimum-key
       :maximum-key maximum-key
       :key-alignment key-alignment))))

;;; ---------------------------------------------------------------------------
(defun sh-create-object (p-heap-objid type-tag
				      &optional (number-of-extra-objids 0)
				      (extra-values-type-tag +null-type-tag+)
				      (number-of-extra-values 0))
  #+:lisp-doc "C interface function for \\fcite{p-allocate}."

  (declare (type fixnum p-heap-objid type-tag
                 number-of-extra-objids number-of-extra-values))
  (let ((new-objid (catch-errors (c-sh-create-object p-heap-objid type-tag
				                     number-of-extra-objids
						     extra-values-type-tag
                                                     number-of-extra-values))))
    (declare (type fixnum new-objid))
    (if (= new-objid +null-objid+)
	(error 'postore-error
               :error-message "Allocating a persistent object failed.")
      new-objid)))

;;; ---------------------------------------------------------------------------
(defun sh-create-structure (p-heap-objid p-objid-structure-description)
  #+:lisp-doc "C interface function for \\fcite{p-allocate-structure}."

  (declare (type fixnum p-heap-objid p-objid-structure-description))
  (let ((new-objid
         (catch-errors (c-sh-create-structure
			p-heap-objid p-objid-structure-description))))
    (declare (type fixnum new-objid))
    (if (= new-objid +null-objid+)
	(error 'postore-error
               :error-message "Allocating a persistent structure failed.")
      new-objid)))

;;; ---------------------------------------------------------------------------
(defun sh-create-instance (p-heap-objid p-objid-class-description)
  #+:lisp-doc "C interface function for \\fcite{p-allocate-instance}."

  (declare (type fixnum p-heap-objid p-objid-class-description))
  (let ((new-objid
         (catch-errors (c-sh-create-instance
			p-heap-objid p-objid-class-description))))
    (declare (type fixnum new-objid))
    (if (= new-objid +null-objid+)
	(error 'postore-error
               :error-message "Allocating a persistent CLOS instance failed.")
      new-objid)))

;;; ---------------------------------------------------------------------------
(defun sh-write-instance-data (p-heap-objid p-objid-instance p-objid-data)
  #+:lisp-doc "Write \\funarg{p-objid-data}\\ to the instance data slot of
 \\funarg{p-objid-instance}."
  (declare (type fixnum p-heap-objid p-objid-instance p-objid-data))
  (c-sh-write-instance-data p-heap-objid p-objid-instance p-objid-data))

;;; ---------------------------------------------------------------------------
(defun sh-write-instance-wrapper
    (p-heap-objid p-objid-instance p-objid-wrapper)
  #+:lisp-doc "Write \\funarg{p-objid-wrapper}\\ to the class wrapper slot of
 \\funarg{p-objid-instance}."
  (declare (type fixnum p-heap-objid p-objid-instance p-objid-wrapper))
  (c-sh-write-instance-wrapper p-heap-objid p-objid-instance p-objid-wrapper))

;;; ---------------------------------------------------------------------------
(defun sh-destroy-object (p-heap-objid p-objid)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-destroy}.
\\Purposelabel
 See \\fcite{p-destroy}.
\\Seealsolabel
 \\shcite{\fn}{SH\\us{}destroy\\us{}object}{3}."

  (declare (type fixnum p-heap-objid p-objid))
  (catch-errors (c-sh-destroy-object p-heap-objid p-objid)))

;;; ---------------------------------------------------------------------------
;;; 1996/09/26 HK: Restart clock not supported for RPC version
#|
(defun sh-get-restart-clock ()
  #+:lisp-doc "
\\Seealsolabel

 \\shcite{function}{SH\\us{}get\\us{}restart\\us{}clock}{4}."

  (catch-errors (c-sh-get-restart-clock)))
|#

;;; ---------------------------------------------------------------------------
(defun sh-close (p-heap-objid &optional (with-garbage-collection t))
  #+:lisp-doc "
\\Purposelabel
 Low-level close of the \\sh.
\\Remarkslabel
 Do not call this function interactively; call \\fcite{close-heap}\\ for
 correct closing of the \\sh.
\\Seealsolabel
 \\Fcite{close-heap};
 \\shcite{function}{SH\\us{}close}{2}."

  (declare (type fixnum p-heap-objid))
  (catch-errors
   (c-sh-close p-heap-objid
	       (if with-garbage-collection +c-true+ +c-false+)))
  (values))

;;; ---------------------------------------------------------------------------
(defun sh-exit (&optional force (url *database-url*))
  #+:lisp-doc "
\\Purposelabel
 Exit the \\plob\\ daemon server process."

  (/= (catch-errors (c-sh-exit (write-url (effective-url url))
			       (if force +c-true+ +c-false+)))
      +c-false+))

;;; ---------------------------------------------------------------------------
(defun sh-objid-size (p-heap-objid p-objid)
  #+:lisp-doc "See \\fcite{p-objid-size}."
  (declare (type fixnum p-heap-objid p-objid))
  (catch-errors (c-sh-objid-size p-heap-objid p-objid)))

;;; ---------------------------------------------------------------------------
(defun make-stable-heap-description ()
  #+:lisp-doc "
Return a string describing the Stable Heap to be created."
  (let ((name (process-name)))
    (if name name "")))

;;; ---------------------------------------------------------------------------
(defparameter *heap-spare-size* (* 32 1024 1024)
  #+:lisp-doc "
 Max.\\ size of the LISP heap.")

;;; ---------------------------------------------------------------------------
(defparameter *max-lisp-addr*
    (or
     #+(and :allegro :alpha)
     (+ (max #x30000000 #x54000000) *heap-spare-size*)
     #+(and :allegro :hpux)
     (+ (max #x20000000 #x64000000) *heap-spare-size*)
     #+(and :allegro :mips)
     (+ (max #x30000000 #x64000000) *heap-spare-size*)
     #+(and :allegro :aix)
     (+ (max #x30000000 #x64000000) *heap-spare-size*)
     #+(and :allegro :solaris2)
     (+ (max #x8000000 #x5000000) *heap-spare-size*)
     #+(and :allegro :linux86 (version>= 6))
     (+ (max #x71000000 #xa0000000) *heap-spare-size*)
     #+(and :allegro :linux86)
     (+ (max #x20000000 #x64000000) *heap-spare-size*)
     #+(and :lispworks :solaris2)
     (logand #xff000000 (+ (sys::object-address nil) #x20000000)))
  #+:lisp-doc "
 Max.\\ address which is in use by LISP. For using a local client, this is
 the address where the POSTORE library will map the database file in.")
     
;;; ---------------------------------------------------------------------------
(defun sh-open (&optional (url *database-url*)
                          (description (make-stable-heap-description)))
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{url}}
       {an URL naming the database to open}
\\Purposelabel
 Low-level open of the database.
\\Remarkslabel
 Do not call this function interactively; \\plob\\ cares for correct
 opening the \\sh\\ the first time this action will be necessary.
 If you want to open a \\plob\\ session explicitely, call the
 \\fcite{open-session}.

\\Seealsolabel
 \\Fcite{open-session};
 \\shcite{function}{SH\\us{}open}{4}."

  (let* ((eff-url (effective-url url))
	 (transport (url-transport eff-url)))
    (sh-load (if (equal transport "local")
		 +lib-local-client-plob+
	       +lib-rpc-client-plob+))
    (let ((p-heap-objid
	   (catch-errors
	    (c-sh-short-open (write-url eff-url) description
			     (if *max-lisp-addr*
				 (floor *max-lisp-addr* 1024)
			       0)
			     ))))
      (if (= p-heap-objid +null-objid+)
	  (setf p-heap-objid nil)
	(register-all-type-tags))
      p-heap-objid)))

;;; ---------------------------------------------------------------------------
(defun canonicalize-expecting-class (expecting-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{expecting-class}}
\\Purposelabel
 Returns a canonicalized \\funarg{expecting-class}."

  (declare (inline canonicalize-expecting-class))
  (if expecting-class
      expecting-class
    +null-objid+))

;;; ---------------------------------------------------------------------------
(defun canonicalize-expecting-type-tag (expecting-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isatypetag{\\funarg{expecting-type-tag}}
\\Purposelabel
 Returns a canonicalized \\funarg{expecting-type-tag}."

  (declare (inline canonicalize-expecting-type-tag))
  (if expecting-type-tag
      expecting-type-tag
    +null-type-tag+))

;;; ---------------------------------------------------------------------------
(defun sh-flush-object (p-heap-objid p-objid remove-from-cache-p)
  #+:lisp-doc "
 \\Purposelabel
  Flush the object referenced by \\funarg{p-objid}.
 \\Seealsolabel
  \\Fcite{flush-object}."
  (when *lib-plob-loaded*
    (c-sh-flush-object (if p-heap-objid p-heap-objid +null-objid+)
		       (if p-objid p-objid +null-objid+)
		       (if remove-from-cache-p +c-true+ +c-false+))))

;;; ---------------------------------------------------------------------------
(defconstant +pprint-buffer-size+ 512
  #+:lisp-doc "
\\Purposelabel
 Size of the string buffer used for printing a persistent object into
 a string by the \\plob\\ low-level print function.
\\Seealsolabel
 \\Fcite{sh-pprint-objid};
 \\fcite{sh-pprint-symbol}.")

;;; ---------------------------------------------------------------------------
(defun sh-pprint-objid (p-heap-objid p-objid type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}}
 \\isanobjid{\\funarg{p-objid}}
 \\isatypetag{\\funarg{type-tag}}
\\Valueslabel
 A string with a printed representation for \\funarg{p-objid}.
\\Purposelabel
 Print a persistent object into a string.
\\Seealsolabel
 \\Fcite{sh-pprint-symbol};
 \\fcite{+pprint-buffer-size+}."

  (declare (type integer p-objid type-tag))
  (let* ((buffer (make-string +pprint-buffer-size+
			      :initial-element #\Space)))
    (catch-errors (c-sh-pprint-objid p-heap-objid p-objid type-tag
				     buffer +pprint-buffer-size+))
    buffer))

;;; ---------------------------------------------------------------------------
(defun sh-pprint-symbol (p-heap-objid p-objid)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 A string with a printed representation for \\funarg{p-objid}.
\\Purposelabel
 Print a persistent symbol into a string.
\\Seealsolabel
 \\Fcite{sh-pprint-objid};
 \\fcite{+pprint-buffer-size+}."

  (declare (type fixnum p-heap-objid p-objid))
  (let* ((buffer (make-string +pprint-buffer-size+
			      :initial-element #\Space)))
    (catch-errors (c-sh-pprint-symbol p-heap-objid p-objid
				      buffer +pprint-buffer-size+))
    buffer))

;;; ---------------------------------------------------------------------------
(defun sh-make-bignum (p-heap-objid from)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-make-bignum}.
\\Valueslabel
 See \\fcite{p-make-bignum}.
\\Purposelabel
 C interface function for \\fcite{p-make-bignum}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-make-bignum}."
  (declare (type fixnum p-heap-objid) (type bignum from))
  (let ((format-objid (p-find-symbol +lisp-symbolic-system-name+ 
				     :depth :objid))
	(size-in-bits (find-bignum-size-in-bits from)))
    (unless format-objid
      (setf format-objid (p-intern +lisp-symbolic-system-name+)))
    (with-handle-lock-conflict
	#'(lambda (p-heap-objid p-objid)
	    (declare (ignore p-objid))
	    (catch-errors
	     (c-sh-make-bignum p-heap-objid format-objid size-in-bits from
			       (if +bignum-deref+ +c-true+ +c-false+)
			       +poi-tag+ +bignum-header-size+)))
      p-heap-objid from)))

;;; ---------------------------------------------------------------------------
(defun sh-make-double-float (p-heap-objid from)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-make-double-float}.
\\Valueslabel
 See \\fcite{p-make-double-float}.
\\Purposelabel
 C interface function for \\fcite{p-make-double-float}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-make-double-float};
 \\fcite{sh-write-double-float}."
  (declare (type fixnum p-heap-objid) (type double-float from))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (declare (ignore p-objid))
       (catch-errors (c-sh-make-double-float p-heap-objid from)))
   p-heap-objid from))

;;; ---------------------------------------------------------------------------
(defun sh-make-single-float (p-heap-objid from)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-make-single-float}.
\\Valueslabel
 See \\fcite{p-make-single-float}.
\\Purposelabel
 C interface function for \\fcite{p-make-single-float}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-make-single-float};
 \\fcite{sh-write-single-float}."
  (declare (type fixnum p-heap-objid) (type single-float from))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (declare (ignore p-objid))
       (catch-errors (c-sh-make-single-float p-heap-objid from)))
   p-heap-objid from))

;;; ---------------------------------------------------------------------------
(defun sh-read-bignum (p-heap-objid p-objid)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-bignum}.
\\Valueslabel
 See \\fcite{p-bignum}.
\\Purposelabel
 C interface function for \\fcite{p-bignum}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid}\\ references a persistent object
 of type \\class{bignum}.

 It is checked if a read-lock is set
 on `vector'-level on \\funarg{p-objid}; if no lock is
 set, a `vector'-level read lock is set on \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-bignum};
 \\fcite{sh-write-bignum}."

  (declare (type fixnum p-heap-objid p-objid))
  (let* ((format-objid (p-find-symbol +lisp-symbolic-system-name+
				      :depth :objid))
	 (size-in-bits (sh-read-fixnum p-heap-objid p-objid
				       +bignum-location-size+ +null-objid+
				       +bignum-type-tag+))
         (the-bignum (allocate-bignum size-in-bits)))
    (declare (type fixnum size-in-bits)
	     #-:lispworks4 ;; and hopefully not later
	     (dynamic-extent size-in-bits))
    (unless format-objid
      (setf format-objid (p-intern +lisp-symbolic-system-name+)))
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid)
	 (catch-errors
	  (c-sh-read-bignum p-heap-objid format-objid
			    p-objid size-in-bits the-bignum
			    (if +bignum-deref+ +c-true+ +c-false+)
			    +poi-tag+ +bignum-header-size+)))
     p-heap-objid p-objid)
    (if (eq (minusp size-in-bits) (minusp the-bignum))
	the-bignum
      (- the-bignum))))

;;; ---------------------------------------------------------------------------
(defun sh-read-double-float (p-heap-objid p-objid)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-double-float}.
\\Valueslabel
 See \\fcite{p-double-float}.
\\Purposelabel
 C interface function for \\fcite{p-double-float}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid}\\ references a persistent
 object of type \\class{double-float}.

 It is checked if a read-lock is set
 on `vector'-level on \\funarg{p-objid}; if no lock is
 set, a `vector'-level read lock is set on \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-double-float};
 \\fcite{sh-write-double-float}."

  (declare (type fixnum p-heap-objid p-objid))
  (let ((old-lock-mode 0)
        (the-double-float 1.0d0))
    (declare (type fixnum old-lock-mode)
	     #-:lispworks4 ;; and hopefully not later
	     (dynamic-extent old-lock-mode))
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid)
	 (multiple-value-setq
	  (old-lock-mode the-double-float)
	   (catch-errors
            (c-sh-read-double-float p-heap-objid p-objid 1.0d0)))
	 old-lock-mode)
     p-heap-objid p-objid)
    the-double-float))

;;; ---------------------------------------------------------------------------
(defun sh-read-fixnum (p-heap-objid p-objid at-index
				    expecting-class expecting-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-fixnum}.
\\Valueslabel
 See \\fcite{p-fixnum}.
\\Purposelabel
 C interface function for \\fcite{p-fixnum}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-fixnum};
 \\fcite{sh-write-fixnum}."

  (declare (type fixnum p-heap-objid p-objid at-index))
  (let ((old-lock-mode 0)
        (the-fixnum 0))
    #-:lispworks
    (declare (type fixnum old-lock-mode) 
	     #-:lispworks4 ;; and hopefully not later
             (dynamic-extent old-lock-mode))
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid)
	 (multiple-value-setq
	  (old-lock-mode the-fixnum)
	   (catch-errors
            (c-sh-read-fixnum p-heap-objid p-objid
			      (canonicalize-expecting-class
			       expecting-class)
			      (canonicalize-expecting-type-tag
			       expecting-type-tag)
			      at-index 0)))
	 old-lock-mode)
     p-heap-objid p-objid)
    the-fixnum))

;;; ---------------------------------------------------------------------------
(defun sh-read-index (p-heap-objid p-objid at-index
				   expecting-class expecting-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-index}.
\\Valueslabel
 See \\fcite{p-index}.
\\Purposelabel
 C interface function for \\fcite{p-index}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-index};
 \\fcite{sh-write-index}."

  (declare (type fixnum p-heap-objid p-objid at-index))
  (let ((old-lock-mode 0)
        (immediate-value 0)
        (immediate-type-tag 0))
    #-:lispworks 
    (declare (type fixnum old-lock-mode)
	     #-:lispworks4 ;; and hopefully not later
             (dynamic-extent old-lock-mode))
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid)
	 (multiple-value-setq
	     (old-lock-mode immediate-value immediate-type-tag)
             (catch-errors
               (c-sh-read-index p-heap-objid p-objid
				(canonicalize-expecting-class expecting-class)
			        (canonicalize-expecting-type-tag
			         expecting-type-tag)
			        at-index 0 0)))
	 old-lock-mode)
     p-heap-objid p-objid)
    (values immediate-value immediate-type-tag)))

;;; ---------------------------------------------------------------------------
(defun sh-read-indices-into
     (p-heap-objid p-objid start-index number-of-objids
                   objids type-tags
                   expecting-class expecting-type-tag)
  #+:lisp-doc "
\\Purposelabel
 Read the \\objid{}s of \\funarg{number-of-objids}\\ slots of the
 persistent object \\funarg{p-objid}\\ starting at slot index
 \\funarg{start-index}\\ into \\funarg{objids}\\ and the
 corresponding \\typetag{}s\\ into \\funarg{type-tags}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}."

  (declare (type fixnum p-heap-objid p-objid start-index number-of-objids))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
	(c-sh-read-indices p-heap-objid p-objid
			   (canonicalize-expecting-class
			    expecting-class)
			   (canonicalize-expecting-type-tag
			    expecting-type-tag)
			   start-index number-of-objids
                           (int-vector-to-c-pointer objids)
			   (int-vector-to-c-pointer type-tags))))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-read-objid (p-heap-objid p-objid at-index
				   expecting-class expecting-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-objid}.
\\Valueslabel
 See \\fcite{p-objid}.
\\Purposelabel
 C interface function for \\fcite{p-objid}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-objid};
 \\fcite{sh-write-objid}."

  (declare (type fixnum p-heap-objid p-objid at-index))
  (let ((old-lock-mode 0)
        (the-objid +null-objid+))
    (declare (type fixnum old-lock-mode)
	     #-:lispworks4 ;; and hopefully not later
	     (dynamic-extent old-lock-mode))
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid)
	 (multiple-value-setq
	  (old-lock-mode the-objid)
	   (catch-errors
            (c-sh-read-objid p-heap-objid p-objid
			     (canonicalize-expecting-class expecting-class)
			     (canonicalize-expecting-type-tag
			      expecting-type-tag)
			     at-index 0)))
	 old-lock-mode)
     p-heap-objid p-objid)
    the-objid))

;;; ---------------------------------------------------------------------------
(defun sh-read-root (p-heap-objid)
  #+:lisp-doc "
\\Valueslabel
 The \\objid\\ of the \\plob\\ root object is returned.
\\Purposelabel
 Low-level read of the \\plob\\ root object.
\\Seealsolabel
 \\Fcite{sh-write-root}."

  (declare (type fixnum p-heap-objid))
  (let ((root-objid (catch-errors (c-sh-read-root p-heap-objid))))
    (declare (type fixnum root-objid))
    (if (/= root-objid +null-objid+)
        root-objid)))

;;; ---------------------------------------------------------------------------
(defun sh-read-single-float (p-heap-objid p-objid)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-single-float}.
\\Valueslabel
 See \\fcite{p-single-float}.
\\Purposelabel
 C interface function for \\fcite{p-single-float}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid}\\ references a persistent
 object of type \\class{single-float}.

 It is checked if a read-lock is set
 on `vector'-level on \\funarg{p-objid}; if no lock is
 set, a `vector'-level read lock is set on \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{p-single-float};
 \\fcite{sh-write-single-float}."

  (declare (type fixnum p-heap-objid p-objid))
  (let ((old-lock-mode 0)
        (the-single-float 1.0e0))
    (declare (type fixnum old-lock-mode)
	     #-:lispworks4 ;; and hopefully not later
	     (dynamic-extent old-lock-mode))
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid)
	 (multiple-value-setq
	  (old-lock-mode the-single-float)
	   (catch-errors
            (c-sh-read-single-float p-heap-objid p-objid 1.0e0)))
	 old-lock-mode)
     p-heap-objid p-objid)
    the-single-float))

;;; ---------------------------------------------------------------------------
(defun sh-read-chars (p-heap-objid p-objid to-string number-of-characters)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{to-string}}
      {a string}
 Rest see \\fcite{p-chars-into}.
\\Valueslabel
 See \\fcite{p-chars-into}.
\\Purposelabel
 C interface function for \\fcite{p-chars-into}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-chars-into};
 \\fcite{sh-write-chars};
 \\shcite{function}{SH\\us{}read\\us{}words}{5}."

  (declare (type fixnum p-heap-objid p-objid number-of-characters))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
         (c-sh-read-chars p-heap-objid p-objid +null-objid+ +string-type-tag+
                          number-of-characters to-string)))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-read-values (p-heap-objid p-objid element-type-tag 
				    to-t-simple-vector number-of-elements
                                    expecting-class expecting-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{to-t-simple-vector}}
      {a simple vector}
 Rest see \\fcite{p-values-into}.
\\Valueslabel
 See \\fcite{p-values-into}.
\\Purposelabel
 C interface function for \\fcite{p-values-into}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 This function uses the \\lw\\ specific feature that the C code with
 a foreign-function argument of type simple-vector receives
 a pointer to the simple vector's data area.
\\Seealsolabel
 \\Fcite{p-values-into};
 \\fcite{sh-write-values};
 \\shcite{function}{SH\\us{}read\\us{}words}{5}."

  (declare (type fixnum p-heap-objid p-objid number-of-elements))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
        (c-sh-read-values p-heap-objid p-objid
			  (canonicalize-expecting-class expecting-class)
			  (canonicalize-expecting-type-tag expecting-type-tag)
			  0 element-type-tag number-of-elements
			  to-t-simple-vector
			  +poi-tag+ +simple-vector-header-size+)))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
;;; 1996/09/26 HK: Restart clock not supported for RPC version
#|
(defun sh-set-restart-clock (time)
  #+:lisp-doc "
\\Seealsolabel

 \\shcite{function}{SH\\us{}set\\us{}restart\\us{}clock}{5}."

  (declare (type fixnum time))
  (catch-errors (c-sh-set-restart-clock time)))
|#

;;; ---------------------------------------------------------------------------
(defun sh-reset (&optional force (url *database-url*))

  #+:lisp-doc "
\\Purposelabel
 Reset the \\plob\\ daemon server process."

  (/= (catch-errors (c-sh-reset (write-url (effective-url url))
				(if force +c-true+ +c-false+)))
      +c-false+))

;;; ---------------------------------------------------------------------------
(defun sh-restart (&optional force (url *database-url*))
  #+:lisp-doc "
\\Purposelabel
 Restart the \\plob\\ daemon server process."

  (/= (catch-errors (c-sh-restart (write-url (effective-url url))
				  (if force +c-true+ +c-false+)))
      +c-false+))

;;; ---------------------------------------------------------------------------
(defun sh-stabilise (p-heap-objid)
  #+:lisp-doc "
\\Purposelabel
 Low-level flush of the \\sh.
\\Seealsolabel
 \\Fcite{plob-flush};
 \\shcite{function}{SH\\us{}stabilise}{6}."

  (declare (type fixnum p-heap-objid))
  (catch-errors (c-sh-stabilise p-heap-objid)))

;;; ---------------------------------------------------------------------------
(defstruct stableheap-statistics
  #+:lisp-doc "
\\Purposelabel

 This is the \\cl\\ equivalent of the C
 \\shcite{struct}{stableheap\\us{}statistics}{1}."

  (maximum-space 0
		 :type fixnum
		 #+:lisp-doc :documentation #+:lisp-doc "
 The maximum size of persistent memory in bytes.")

  (allocated-space 0
		   :type fixnum
		   #+:lisp-doc :documentation #+:lisp-doc "
 The already allocated persistent memory in bytes.")

  (unallocated-space 0
		     :type fixnum
		     #+:lisp-doc :documentation #+:lisp-doc "
 ???")

  (unused-allocated-space 0
			  :type fixnum
			  #+:lisp-doc :documentation #+:lisp-doc "
 ???")

  (allocated-management-space 0
			      :type fixnum
			      #+:lisp-doc :documentation #+:lisp-doc "
 ???")

  (number-of-objects 0
		     :type fixnum
		     #+:lisp-doc :documentation #+:lisp-doc "
 Number of allocated persistent objects; this is around 2837 after the
 \\sh\\ is formatted by calling the \\fcite{format-plob-root};
 performing a garbage collection right after the formatting reduces
 the number of objects to 1740."))
  
;;; ---------------------------------------------------------------------------
(defun sh-sessions (p-heap-objid)
  #+:lisp-doc "
\\Purposelabel
 Returns an instance containing all active sessions. Currently, this is
 a BTree."
  (when (sh-open-p)
    (let ((sessions (catch-errors (c-sh-sessions p-heap-objid))))
      (when (/= sessions +null-objid+)
        sessions))))

;;; ---------------------------------------------------------------------------
;;; Look up for machine names
;;; ---------------------------------------------------------------------------
(defun sh-get-host-addr (name)
  #+:lisp-doc "Convert \\funarg{name}\\ into a numeric Internet address."
  (with-dynamic-int-vectors
      ((address-buffer (make-int-vector 4)))
    (when (/= (c-sh-get-host-addr
	       name (int-vector-to-c-pointer address-buffer))
	      +c-false+)
      (int-vector-to-vector address-buffer))))

;;; ---------------------------------------------------------------------------
(defun sh-create-machine (p-heap-objid machine-addr loginp)
  #+:lisp-doc "
\\Purposelabel
 Returns an instance of class \\class{machine}."
  (with-dynamic-int-vectors
      ((address-buffer (vector-to-int-vector machine-addr)))
    (let ((machine
	   (catch-errors
	    (c-sh-create-machine p-heap-objid
				 (int-vector-to-c-pointer address-buffer)
				 loginp))))
      (when (/= machine +null-objid+)
        machine))))

;;; ---------------------------------------------------------------------------
(defun sh-machine-loginp (p-heap-objid machine loginp)
  #+:lisp-doc "
\\Purposelabel
 Set resp.\\ get login flag of \\funarg{machine}"
  (catch-errors
   (c-sh-machine-loginp p-heap-objid machine loginp)))

;;; ---------------------------------------------------------------------------
(defun sh-machine-addr (p-heap-objid machine)
  #+:lisp-doc "
\\Purposelabel
 Get the Internet address of \\funarg{machine}"
  (with-dynamic-int-vectors
      ((address-buffer (make-int-vector 4)))
    (when (/= (catch-errors
	       (c-sh-machine-addr p-heap-objid
				  machine
				  (int-vector-to-c-pointer address-buffer)))
	      +c-false+)
      (int-vector-to-vector address-buffer))))

;;; ---------------------------------------------------------------------------
(defun sh-search-machine (p-heap-objid machine-addr)
  #+:lisp-doc "
\\Purposelabel
 Search \\funarg{machine}\\ in the internal list of known machines."
  (let ((machine
	 (catch-errors
	  (with-dynamic-int-vectors
	      ((address-buffer (vector-to-int-vector machine-addr)))
	    (c-sh-search-machine p-heap-objid
				 (int-vector-to-c-pointer address-buffer))))))
    (when (/= machine +null-objid+)
      machine)))

;;; ---------------------------------------------------------------------------
(defun sh-delete-machine (p-heap-objid machine-addr)
  #+:lisp-doc "
\\Purposelabel
 Delete \\funarg{machine}\\ from the internal list of known machines."
  (let ((machine
	 (catch-errors
	  (with-dynamic-int-vectors
	      ((address-buffer (vector-to-int-vector machine-addr)))
	    (c-sh-delete-machine p-heap-objid
				 (int-vector-to-c-pointer address-buffer))))))
    (when (/= machine +null-objid+)
      machine)))

;;; ---------------------------------------------------------------------------
(defun sh-insert-machine (p-heap-objid machine)
  #+:lisp-doc "
\\Purposelabel
 Insert \\funarg{machine}\\ into the internal list of known machines."
  (let ((machine
	 (catch-errors (c-sh-insert-machine p-heap-objid machine))))
      (when (/= machine +null-objid+)
        machine)))

;;; ---------------------------------------------------------------------------
(defun sh-machines (p-heap-objid)
  #+:lisp-doc "
\\Purposelabel
 Returns an instance containing all known machines.
 Currently, this is a BTree."
  (let ((machines (catch-errors (c-sh-machines p-heap-objid))))
    (when (/= machines +null-objid+)
      machines)))

;;; ---------------------------------------------------------------------------
(defun sh-statistics (p-heap-objid)
  #+:lisp-doc "
\\Purposelabel
 Returns an instance of \\fcite{stableheap-statistics}\\ containing some
 \\sh\\ statistics.
\\Seealsolabel
 \\Fcite{stableheap-statistics};
 \\fcite{sh-configuration};
 \\shcite{function}{SH\\us{}statistics}{6}."

  (declare (type fixnum p-heap-objid))
  (multiple-value-bind (done maximum-space allocated-space
			     unallocated-space unused-allocated-space
			     allocated-management-space number-of-objects)
        (catch-errors (c-sh-statistics p-heap-objid
                                       0 0 0 0 0 0))
    (when (/= done +c-false+)
      (make-stableheap-statistics
       :maximum-space maximum-space
       :allocated-space allocated-space
       :unallocated-space unallocated-space
       :unused-allocated-space unused-allocated-space
       :allocated-management-space allocated-management-space
       :number-of-objects number-of-objects))))

;;; ---------------------------------------------------------------------------
(defun sh-type-tag-of (p-heap-objid p-objid)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
\\Purposelabel
 See \\fcite{p-type-tag-of}."

  (declare (type fixnum p-heap-objid p-objid))
  (catch-errors (c-sh-type-tag p-heap-objid p-objid)))

;;; ---------------------------------------------------------------------------
(defun sh-type-tag-to-string (type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isatypetag{\\funarg{type-tag}}
\\Purposelabel
 Convert the \\funarg{type-tag}\\ into string form.
\\Exampleslabel
 Convert the \\typetag\\ \\lisp{+heap-type-tag+}\\ to a string:
 \\begin{lispcode}
(sh-type-tag-to-string +heap-type-tag+) ==> \"heap\"
 \\end{lispcode}
\\Seealsolabel
 \\Fcite{sh-type-tag-of}."

  (declare (type fixnum type-tag))
  (let ((buffer (make-string +type-name-buffer-size+
			     :initial-element #\Space)))
    (catch-errors
      (c-sh-type-string type-tag buffer +type-name-buffer-size+))
    buffer))

;;; ---------------------------------------------------------------------------
(defun sh-value-size (p-heap-objid p-objid)
  #+:lisp-doc "See \\Fcite{p-value-size}."
  (declare (type fixnum p-heap-objid p-objid))
  (catch-errors (c-sh-value-size p-heap-objid p-objid)))

;;; ---------------------------------------------------------------------------
(defun sh-write-bignum (p-heap-objid p-objid the-bignum)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite {(setf p-bignum)}.
\\Valueslabel
 \\retoldmode{\\funarg{p-heap-objid}}{\\funarg{p-objid}}
\\Purposelabel
 C interface function for \\fcite{(setf p-bignum)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid}\\ references a persistent object
 of type \\class{bignum}.

 It is checked if a write-lock is set
 on `vector'-level on \\funarg{p-objid}; if no lock is
 set, a `vector'-level write lock is set on \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-bignum)};
 \\fcite{sh-read-bignum}."

  (declare (type fixnum p-heap-objid p-objid))
  (let* ((format-objid (p-find-symbol +lisp-symbolic-system-name+
				      :depth :objid))
	 (size-in-bits (find-bignum-size-in-bits the-bignum)))
    (unless format-objid
      (setf format-objid (p-intern +lisp-symbolic-system-name+)))
    (with-handle-lock-conflict
	#'(lambda (p-heap-objid p-objid)
	    (catch-errors
	     (c-sh-write-bignum p-heap-objid format-objid p-objid
				size-in-bits the-bignum
				(if +bignum-deref+ +c-true+ +c-false+)
				+poi-tag+ +bignum-header-size+)))
      p-heap-objid p-objid)))

;;; ---------------------------------------------------------------------------
(defun sh-write-double-float (p-heap-objid p-objid
                                           the-double-float)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{(setf p-double-float)}.
\\Valueslabel
 \\retoldmode{\\funarg{p-heap-objid}}{\\funarg{p-objid}}
\\Purposelabel
 C interface function for \\fcite{(setf p-double-float)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid}\\ references a persistent object
 of type \\class{double-float}.

 It is checked if a write-lock is set
 on `vector'-level on \\funarg{p-objid}; if no lock is
 set, a `vector'-level write lock is set on \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-double-float)};
 \\fcite{sh-read-double-float}."

  (declare (type fixnum p-heap-objid p-objid))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
        (c-sh-write-double-float p-heap-objid p-objid the-double-float)))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-write-fixnum (p-heap-objid p-objid at-index
				     expecting-class expecting-type-tag
                                     the-fixnum)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{(setf p-fixnum)}.
\\Valueslabel
 \\retoldmode{\\funarg{p-heap-objid}}{\\funarg{p-objid}}
\\Purposelabel
 C interface function for \\fcite{(setf p-fixnum)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{(setf p-fixnum)};
 \\fcite{sh-read-fixnum}."

  (declare (type fixnum p-heap-objid p-objid at-index the-fixnum))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
        (c-sh-write-fixnum p-heap-objid p-objid
			   (canonicalize-expecting-class expecting-class)
			   (canonicalize-expecting-type-tag expecting-type-tag)
			   at-index the-fixnum)))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-write-index (p-heap-objid p-objid at-index
				    expecting-class expecting-type-tag
                                    immediate-value immediate-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{(setf p-index)}.
\\Valueslabel
 See \\fcite{(setf p-index)}.
\\Purposelabel
 C interface function for \\fcite{(setf p-index)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{(setf p-index)};
 \\fcite{sh-read-index}."

  (declare (type fixnum p-heap-objid p-objid at-index
                 immediate-value immediate-type-tag))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
        (c-sh-write-index p-heap-objid p-objid
			  (canonicalize-expecting-class expecting-class)
                          (canonicalize-expecting-type-tag expecting-type-tag)
			  at-index immediate-value immediate-type-tag)))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-write-indices
     (p-heap-objid p-objid start-index number-of-objids
                   objids type-tags
                   expecting-class expecting-type-tag)
  #+:lisp-doc "
\\Purposelabel
 Write the \\objid{}s of \\funarg{number-of-objids}\\ slots of the
 persistent object \\funarg{p-objid}\\ starting at slot index
 \\funarg{start-index}\\ from \\funarg{objids}\\ and the
 corresponding \\typetag{}s\\ from \\funarg{type-tags}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}."

  (declare (type fixnum p-heap-objid p-objid start-index number-of-objids))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
	(c-sh-write-indices p-heap-objid p-objid
			    (canonicalize-expecting-class expecting-class)
			    (canonicalize-expecting-type-tag
			     expecting-type-tag)
			    start-index number-of-objids
			    (int-vector-to-c-pointer objids)
			    (int-vector-to-c-pointer type-tags))))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-write-objid (p-heap-objid p-objid at-index
				    expecting-class expecting-type-tag
                                    the-p-objid)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{(setf p-objid)}.
\\Valueslabel
 See \\fcite{(setf p-objid)}.
\\Purposelabel
 C interface function for \\fcite{(setf p-objid)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{(setf p-objid)};
 \\fcite{sh-read-objid}."

  (declare (type fixnum p-heap-objid p-objid at-index the-p-objid))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
        (c-sh-write-objid p-heap-objid p-objid
			  (canonicalize-expecting-class expecting-class)
			  (canonicalize-expecting-type-tag expecting-type-tag)
			  at-index the-p-objid)))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-write-root (p-heap-objid p-objid)
  #+:lisp-doc "
\\Purposelabel
 Low-level set of the \\plob\\ root object to \\funarg{p-objid}.
\\Remarkslabel
 {\\sl A note of caution:}
 Calling this function with inappropiate arguments will make
 the root object which \\plob\\ expects inaccessible and makes
 therefore also the persistent objects contained in the persistent
 heap inaccessible.
\\Seealsolabel
 \\Fcite{sh-read-root}."

  (declare (type fixnum p-heap-objid))
  (let ((new-root-objid
	 (catch-errors (c-sh-write-root p-heap-objid
					(if p-objid p-objid +null-objid+)))))
    (if (= new-root-objid +null-objid+) nil new-root-objid)))

;;; ---------------------------------------------------------------------------
(defun sh-write-single-float (p-heap-objid p-objid
                                           the-single-float)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{(setf p-single-float)}.
\\Valueslabel
 \\retoldmode{\\funarg{p-heap-objid}}{\\funarg{p-objid}}
\\Purposelabel
 C interface function for \\fcite{(setf p-single-float)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid}\\ references a persistent object
 of type \\class{single-float}.

 It is checked if a write-lock is set
 on `vector'-level on \\funarg{p-objid}; if no lock is
 set, a `vector'-level write lock is set on \\funarg{p-objid}.
\\Seealsolabel
 \\Fcite{(setf p-single-float)};
 \\fcite{sh-read-single-float}."

  (declare (type fixnum p-heap-objid p-objid))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
        (c-sh-write-single-float p-heap-objid p-objid the-single-float)))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-write-chars (p-heap-objid p-objid from-string number-of-characters)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{from-string}}
      {a string}
 Rest see \\fcite{(setf p-chars)}.
\\Valueslabel
 See \\fcite{(setf p-chars)}.
\\Purposelabel
 C interface function for \\fcite{(setf p-chars)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{(setf p-chars)};
 \\fcite{sh-read-chars};
 \\shcite{function}{SH\\us{}write\\us{}words}{5}."

  (declare (type fixnum p-heap-objid p-objid number-of-characters))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
        (c-sh-write-chars p-heap-objid p-objid +null-objid+ +string-type-tag+
			  number-of-characters from-string)))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-write-values (p-heap-objid p-objid
				     expecting-class expecting-type-tag
				     element-type-tag
                                     from-t-simple-vector number-of-elements)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{from-t-simple-vector}}
      {a simple vector}
 Rest see \\fcite{(setf p-values)}.
\\Valueslabel
 See \\fcite{(setf p-values)}.
\\Purposelabel
 C interface function for \\fcite{(setf p-values)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 This function uses the \\lw\\ specific feature that the C code with
 a foreign-function argument of type simple-vector receives
 a pointer to the simple vector's data area.
\\Seealsolabel
 \\Fcite{(setf p-values)};
 \\fcite{sh-read-values};
 \\shcite{function}{SH\\us{}write\\us{}words}{5}."

  (declare (type fixnum p-heap-objid p-objid number-of-elements))
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid)
       (catch-errors
        (c-sh-write-values p-heap-objid p-objid
			   (canonicalize-expecting-class expecting-class)
			   (canonicalize-expecting-type-tag expecting-type-tag)
                           0 element-type-tag number-of-elements
			   from-t-simple-vector
			   +poi-tag+ +simple-vector-header-size+)))
   p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-objid-valid-p (p-objid)
  #+:lisp-doc "
\\Purposelabel
 Return \\nonnil\\ if \\funarg{p-objid}\\ is a valid \\objid,
 \\lispnil\\ otherwise."

  (and *lib-plob-loaded*
       (integerp p-objid)
       (/= p-objid +null-objid+)
       (/= (catch-errors (c-sh-objid-valid-p p-objid)) +c-false+)))

;;; ---------------------------------------------------------------------------
(defun sh-open-p ()
  #+:lisp-doc "
\\Purposelabel
 Check if the \\sh\\ is open at all.
\\Seealsolabel
 \\Fcite{assert-sh-open-p}."

  (declare (inline sh-open-p))
  (or *in-bootstrap-p*
      (sh-objid-valid-p *root-persistent-heap-objid*)))

;;; ---------------------------------------------------------------------------
(defun assert-sh-open-p ()
  #+:lisp-doc "
\\Purposelabel
 Assert that the \\sh\\ is open at all; if it is not yet open,
 it is opened.
\\Seealsolabel
 \\Fcite{sh-open-p}."

  (declare (inline assert-sh-open-p))
    (unless (sh-open-p)
      (open-heap)))

;;; ---------------------------------------------------------------------------
(defun assert-open-session-p (&optional 
			      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Assert that the \\sh\\ and the process-local sessions is open at all;
 if it is not yet open, it is opened.
\\Seealsolabel
 \\Fcite{assert-sh-open-p}."

  (declare (inline assert-open-session-p))
  (assert-sh-open-p)
  (unless (persistent-heap-pid p-heap)
    (open-session *database-url* p-heap))
  p-heap)

;;; ---------------------------------------------------------------------------
;;; Locking
;;; ---------------------------------------------------------------------------

(defun sh-set-lock (p-heap-objid p-objid expecting-type-tag lock-mode at-index)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}}
 Rest see \\fcite{p-set-lock}.
\\Valueslabel
 See \\fcite{p-set-lock}.
\\Purposelabel
 C interface function for \\fcite{p-set-lock}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-set-lock};
 \\fcite{sh-insert-lock}."

  (declare (type fixnum p-heap-objid p-objid lock-mode at-index))
  (catch-errors
   (c-sh-set-lock p-heap-objid p-heap-objid lock-mode p-objid
		  (canonicalize-expecting-type-tag expecting-type-tag)
		  at-index)))

;;; ---------------------------------------------------------------------------
(defun sh-insert-lock (p-heap-objid p-objid
				    expecting-type-tag lock-mode at-index)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}}
 Rest see \\fcite{p-insert-lock}.
\\Valueslabel
 See \\fcite{p-insert-lock}.
\\Purposelabel
 C interface function for \\fcite{p-insert-lock}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-insert-lock};
 \\fcite{sh-set-lock}."

  (declare (type fixnum p-heap-objid p-objid lock-mode at-index))
  (catch-errors
   (c-sh-insert-lock p-heap-objid p-heap-objid lock-mode p-objid
		     (canonicalize-expecting-type-tag expecting-type-tag)
		     at-index)))

;;; ---------------------------------------------------------------------------
(defun sh-read-only (p-heap-objid p-objid read-only-p)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{read-only-p}}
      {a fixnum with one of the constant values of
       {\\bf +read-only+}, {\\bf +read-write+} or {\\bf +read-only-p+}}
\\Valueslabel
 Returns the old read-only state of \\funarg{p-objid}, one of the
 constant values of {\\bf +read-only+} or {\\bf +read-write+}.
\\Purposelabel
 The actions done by {\\bf sh-read-only} depend on the value of
 \\funarg{read-only-p}:
 \\begin{description}
 \\item[{\\bf +read-only+}]
  Lock the persistent object referenced by \\funarg{p-objid}\\ as
  read-only; no further write locks will be accepted for
  \\funarg{p-objid}. No error is signalled if there was already
  a read-only lock set to \\funarg{p-objid}.
 \\item[{\\bf +read-write+}]
  Remove the read-only lock from the persistent object referenced
  by \\funarg{p-objid}. No error is signalled if there was no
  read-only lock set at all to \\funarg{p-objid}.
 \\item[{\\bf +read-only-p+}]
  Return the current read-only lock status of the persistent object
  referenced by \\funarg{p-objid}.
 \\end{description}
\\Seealsolabel
 \\Fcite{p-read-only};
 \\fcite{(setf p-read-only)}."

  (declare (type fixnum p-heap-objid p-objid read-only-p))
  (/= (catch-errors (c-sh-read-only p-heap-objid p-objid read-only-p))
      +c-false+))

;;; ---------------------------------------------------------------------------
(defun sh-unlock (p-heap-objid p-objid lock-mode at-index)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}}
 Rest see \\fcite{p-unlock}.
\\Valueslabel
 See \\fcite{p-unlock}.
\\Purposelabel
 C interface function for \\fcite{p-unlock}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-unlock};
 \\fcite{sh-set-lock};
 \\fcite{sh-insert-lock}."

  (declare (type fixnum p-heap-objid p-objid lock-mode at-index))
  (let ((old-lock-mode
         (catch-errors
          (c-sh-unlock p-heap-objid p-heap-objid lock-mode p-objid at-index))))
    (declare (type fixnum old-lock-mode))
    (when (/= old-lock-mode +unlock-failed+)
      old-lock-mode)))

;;; ---------------------------------------------------------------------------
(defun sh-unlock-all (p-heap-objid p-objid)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}}
 Rest see \\fcite{p-unlock-all}.
\\Valueslabel
 See \\fcite{p-unlock-all}.
\\Purposelabel
 C interface function for \\fcite{p-unlock-all}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.
\\Seealsolabel
 \\Fcite{p-unlock-all};
 \\fcite{sh-unlock};
 \\fcite{sh-unlock-all-all}."

  (declare (type fixnum p-heap-objid p-objid))
  (c-sh-unlock-all p-heap-objid p-heap-objid p-objid))

;;; ---------------------------------------------------------------------------
(defun sh-unlock-all-all (p-heap-objid p-objid)
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-unlock-all-all}.
\\Valueslabel
 See \\fcite{p-unlock-all-all}.
\\Purposelabel
 C interface function for \\fcite{p-unlock-all-all}.
\\Seealsolabel
 \\Fcite{p-unlock-all};
 \\fcite{sh-unlock};
 \\fcite{sh-unlock-all-all}."

  (declare (type fixnum p-heap-objid p-objid))
  (catch-errors (c-sh-unlock-all-all p-heap-objid p-objid)))

;;; ---------------------------------------------------------------------------
;;; Transactions
;;; ---------------------------------------------------------------------------

(defun sh-begin-transaction (p-heap-objid &optional ignore-error)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}}
 Rest see \\fcite{begin-transaction}.
\\Valueslabel
 See \\fcite{begin-transaction}.
\\Purposelabel
 C interface function for \\fcite{begin-transaction}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}.

 To start a transaction, do not call this function interactively;
 use the \\fcite{begin-transaction}.
\\Seealsolabel
 \\Fcite{begin-transaction}."

  (declare (type fixnum p-heap-objid))
  (let ((in-transaction
	 (catch-errors
          (c-sh-begin-transaction p-heap-objid
				  (if ignore-error
				      +c-true+
				    +c-false+)))))
    (declare (type fixnum in-transaction))
    (if (/= in-transaction +null-transaction-id+) in-transaction)))

;;; ---------------------------------------------------------------------------
(defun sh-cancel-transaction (p-heap-objid &optional ignore-error)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}}
 Rest see \\fcite{cancel-transaction}.
\\Valueslabel
 See \\fcite{cancel-transaction}.
\\Purposelabel
 C interface function for \\fcite{cancel-transaction}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 To cancel a transaction, do not call this function interactively;
 use the \\fcite{cancel-transaction}.
\\Seealsolabel
 \\Fcite{cancel-transaction}."

  (declare (type fixnum p-heap-objid))
  (let ((in-transaction
	 (catch-errors
          (c-sh-cancel-transaction p-heap-objid
				   (if ignore-error
				       +c-true+
				     +c-false+)))))
    (declare (type fixnum in-transaction))
    (when (/= in-transaction +null-transaction-id+) in-transaction)))

;;; ---------------------------------------------------------------------------
(defun sh-end-transaction (p-heap-objid &optional ignore-error)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}}
 Rest see \\fcite{end-transaction}.
\\Valueslabel
 See \\fcite{end-transaction}.
\\Purposelabel
 C interface function for \\fcite{end-transaction}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 To end a transaction, do not call this function interactively;
 use the \\fcite{end-transaction}.
\\Seealsolabel
 \\Fcite{end-transaction}."

  (declare (type fixnum p-heap-objid))
  (let ((in-transaction
	 (catch-errors
          (c-sh-end-transaction p-heap-objid
				(if ignore-error
				    +c-true+
				  +c-false+)))))
    (declare (type fixnum in-transaction))
    (if (/= in-transaction +null-transaction-id+) in-transaction)))

;;; ---------------------------------------------------------------------------
(defun sh-in-transaction-p (p-heap-objid
                            &optional (transaction-id +null-transaction-id+))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}}
 \\isa{\\funarg{transaction-id}}
      {a numeric transaction ID}
\\Valueslabel
 See \\fcite{in-transaction-p}.

 If \\funarg{transaction-id}\\ is optionally
 passed, the numeric transaction ID is only returned iff it's
 \\lisp{equal}\\ to \\funarg{transaction-id}; \\lispnil\\ otherwise.
\\Purposelabel
 C interface function for \\fcite{in-transaction-p}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}.

 To check for an active transaction, do not call this function
 interactively; use the \\fcite{in-transaction-p}.
\\Seealsolabel
 \\Fcite{in-transaction-p}."

  (declare (type fixnum p-heap-objid transaction-id))
  (let ((in-transaction
	 (catch-errors (c-sh-in-transaction-p p-heap-objid transaction-id))))
    (if (/= in-transaction +null-transaction-id+) in-transaction)))

;;; ---------------------------------------------------------------------------
;;; Flag word handling
;;; ---------------------------------------------------------------------------
(defun flag-word (&optional (new-flag-word 0 new-flag-word-p))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{new-flag-word}}
      {a fixnum}
\\Valueslabel
 Returns the current value of the flag word.
\\Purposelabel
 Set resp.\\ retrieve the flag word from the C level. The flag word is a
 32~bit \\lisp{int}\\ represented only in the C level;
 its meaning is currently undefined. I used it for
 [re]setting flag bits which signalled to the C level to switch on
 resp.\\ off debugging messages.

 The action done by this function depends upon if the {\\opt} argument
 \\funarg{new-flag-word}\\ was passed or not:
 \\begin{description}
 \\item[Argument \\funarg{new-flag-word}\\ was passed:]
  The flag word of the C level is set to \\funarg{new-flag-word}.
 \\item[Argument \\funarg{new-flag-word}\\ was not passed:]
  The current flag word of the C level is returned.
 \\end{description}"

  (if new-flag-word-p
      (progn
        (c-sh-flag-word +flag-set+ new-flag-word)
        new-flag-word)
    (c-sh-flag-word +flag-get+ new-flag-word)))

;;; ---------------------------------------------------------------------------
;;; BTrees
;;; ---------------------------------------------------------------------------
(defun sh-btree-clear (p-heap-objid p-objid-btree)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
\\Valueslabel
 Returns \\lispt\\ if the persistent BTree referenced by
 \\funarg{p-objid-btree}\\ was already empty,
 \\nonnil\\ otherwise.
\\Purposelabel
 C interface function for \\fcite{clrbtree (t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}\\ and if it's an empty BTree; otherwise
 an error is signalled.
\\Seealsolabel
 \\Fcite{clrbtree (t)}."

  (declare (type fixnum p-objid-btree))
  (let ((cleared
	 (with-handle-lock-conflict
	  #'(lambda (p-heap-objid p-objid-btree)
	      (catch-errors (c-sh-btree-clear p-heap-objid p-objid-btree)))
	  p-heap-objid p-objid-btree)))
    (declare (type fixnum cleared)
	     #-:lispworks4 ;; and hopefully not later
	     (dynamic-extent cleared))
    (cond ((= cleared +btree-deleted+) t)
          ((= cleared +btree-not-found+) nil)
          (t (when (and *verbose* (>= *verbose* 1))
	       (cerror
		"Ignore return value."
		"Received unknown return value ~A from (c-sh-btree-clear ...)."
		cleared))
             (values)))))

;;; ---------------------------------------------------------------------------
(defun sh-btree-delete-return-value (deleted)
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{deleted}}
       {one of the values of the constants
        {\\bf +btree-deleted+} or {\\bf +btree-not-found+}}
\\Valueslabel
 If \\funarg{deleted}\\ is {\\bf +btree-deleted+}, return \\lispt;
 if \\funarg{deleted}\\ is {\\bf +btree-not-found+}, return \\lispnil;
 otherwise, signal an error.
\\Purposelabel
 Canonicalize the low-level return value \\funarg{deleted}\\ returned
 from one of the {\\bf sh-btree-delete\\ldots} functions so that it
 matches the return value of \\fcite{remhash}.
\\Seealsolabel
 Return value of \\fcite{remhash}."

  (cond ((= deleted +btree-deleted+) t)
	((= deleted +btree-not-found+) nil)
	(t (when (and *verbose* (>= *verbose* 1))
	     (cerror
	      "Ignore return value."
	      "Received unknown return value ~A from (c-sh-btree-delete ...)."
	      deleted))
	   (values))))

;;; ---------------------------------------------------------------------------
(defun sh-btree-delete (p-heap-objid p-objid-btree
                                     immediate-key-value
                                     immediate-key-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-value}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{immediate-key-type-tag}}
\\Purposelabel
 C interface function for various methods of \\fcite{rembtree}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{rembtree}."

  (declare (type fixnum p-objid-btree
                 immediate-key-value immediate-key-type-tag))
  (sh-btree-delete-return-value
   (with-handle-lock-conflict
    #'(lambda (p-heap-objid p-objid-btree)
	(catch-errors
         (c-sh-btree-delete p-heap-objid p-objid-btree
			    immediate-key-value immediate-key-type-tag)))
    p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-delete-by-float (p-heap-objid p-objid-btree
					      immediate-key-single-float)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-single-float}}
      {a single float}
\\Purposelabel
 C interface function for \\fcite{rembtree (float t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{rembtree (float t)}."

  (declare (type fixnum p-objid-btree))
  (sh-btree-delete-return-value
   (with-handle-lock-conflict
    #'(lambda (p-heap-objid p-objid-btree)
	(catch-errors
         (c-sh-btree-delete-by-float p-heap-objid p-objid-btree
				     (coerce immediate-key-single-float
					     'double-float))))
    p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-delete-by-double (p-heap-objid p-objid-btree
					       immediate-key-double-float)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-double-float}}
      {a double float}
\\Purposelabel
 C interface function for \\fcite{rembtree (float t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{rembtree (float t)}."

  (declare (type fixnum p-objid-btree))
  (sh-btree-delete-return-value
   (with-handle-lock-conflict
    #'(lambda (p-heap-objid p-objid-btree)
	(catch-errors
         (c-sh-btree-delete-by-double p-heap-objid p-objid-btree
				      immediate-key-double-float)))
    p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-delete-by-string (p-heap-objid p-objid-btree
                                               immediate-key-vector)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-vector}}
      {a string}
\\Purposelabel
 C interface function for various methods of \\fcite{rembtree}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{rembtree (string t)};
 \\fcite{rembtree (symbol t)}."

  (declare (type fixnum p-objid-btree immediate-key-type-tag))
  (sh-btree-delete-return-value
   (with-handle-lock-conflict
    #'(lambda (p-heap-objid p-objid-btree)
	(catch-errors
         (c-sh-btree-delete-by-string p-heap-objid p-objid-btree
				      immediate-key-vector)))
    p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-insert-return-value (inserted)
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{inserted}}
       {one of the values of the constants
        {\\bf +btree-inserted+} or {\\bf +btree-not-found+}}
\\Valueslabel
 If \\funarg{inserted}\\ is {\\bf +btree-inserted+}, return \\lispnil;
 if \\funarg{inserted}\\ is {\\bf +btree-updated+}, return \\lispt;
 otherwise, signal an error.
\\Purposelabel
 Canonicalize the low-level return value \\funarg{inserted}\\ returned
 from one of the {\\bf sh-btree-insert\\ldots} functions so that it
 matches the return value of \\fcite{(setf gethash)}.
\\Seealsolabel
 Return value of \\fcite{(setf gethash)}."

  (cond ((= inserted +btree-inserted+) nil)
	((= inserted +btree-updated+) t)
	(t (when (and *verbose* (>= *verbose* 1))
	     (cerror
	      "Ignore return value."
	      "Received unknown return value ~A from (c-sh-btree-insert ...)."
	      inserted))
	   (values))))

;;; ---------------------------------------------------------------------------
(defun sh-btree-insert (p-heap-objid p-objid-btree
                                     immediate-key-value
                                     immediate-key-type-tag
                                     immediate-data-value
                                     immediate-data-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-value}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{immediate-key-type-tag}}
 \\isa{\\funarg{immediate-data-value}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{immediate-data-type-tag}}
\\Purposelabel
 C interface function for many methods of
 \\fcite{(setf getbtree-with-data)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{(setf getbtree-with-data)}."

  (declare (type fixnum p-objid-btree
		 immediate-key-value immediate-key-type-tag
		 immediate-data-value immediate-data-type-tag))
  (sh-btree-insert-return-value
   (with-handle-lock-conflict
    #'(lambda (p-heap-objid p-objid-btree)
	(catch-errors
         (c-sh-btree-insert p-heap-objid p-objid-btree
			    immediate-key-value immediate-key-type-tag
			    immediate-data-value immediate-data-type-tag)))
    p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-insert-by-float (p-heap-objid p-objid-btree
					      immediate-key-single-float
					      immediate-data-value
					      immediate-data-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-single-float}}
      {a single float}
 \\isa{\\funarg{immediate-data-value}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{immediate-data-type-tag}}
\\Purposelabel
 C interface function for
 \\fcite{(setf getbtree-with-data) (t float t t t t t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{(setf getbtree-with-data) (t float t t t t t)}."

  (declare (type fixnum p-objid-btree
                 immediate-data-value immediate-data-type-tag))
  (sh-btree-insert-return-value
   (with-handle-lock-conflict
    #'(lambda (p-heap-objid p-objid-btree)
	(catch-errors
         (c-sh-btree-insert-by-float p-heap-objid p-objid-btree
				     (coerce immediate-key-single-float
					     'double-float)
				     immediate-data-value
				     immediate-data-type-tag)))
    p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-insert-by-double (p-heap-objid p-objid-btree
					       immediate-key-double-float
					       immediate-data-value
					       immediate-data-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-double-float}}
      {a double float}
 \\isa{\\funarg{immediate-data-value}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{immediate-data-type-tag}}
\\Purposelabel
 C interface function for
 \\fcite{(setf getbtree-with-data) (t float t t t t t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{(setf getbtree-with-data) (t float t t t t t)}."

  (declare (type fixnum p-objid-btree
                 immediate-data-value immediate-data-type-tag))
  (sh-btree-insert-return-value
   (with-handle-lock-conflict
    #'(lambda (p-heap-objid p-objid-btree)
	(catch-errors
         (c-sh-btree-insert-by-double p-heap-objid p-objid-btree
				      immediate-key-double-float
				      immediate-data-value
				      immediate-data-type-tag)))
    p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-insert-by-string (p-heap-objid p-objid-btree
                                               immediate-key-vector
                                               immediate-data-value
                                               immediate-data-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-vector}}
      {a string}
 \\isa{\\funarg{immediate-data-value}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{immediate-data-type-tag}}
\\Purposelabel
 C interface function for various methods of
 \\fcite{(setf getbtree-with-data)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{(setf getbtree-with-data) (t string t t t t t)};
 \\fcite{(setf getbtree-with-data) (t symbol t t t t t)}."

  (declare (type fixnum p-objid-btree immediate-key-type-tag
		 immediate-data-value immediate-data-type-tag))
  (sh-btree-insert-return-value
   (with-handle-lock-conflict
    #'(lambda (p-heap-objid p-objid-btree)
	(catch-errors
         (c-sh-btree-insert-by-string p-heap-objid p-objid-btree
				      immediate-key-vector
				      immediate-data-value
				      immediate-data-type-tag)))
    p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------

;; 1996/10/01 HK: No callbacks for RPC version:
;;(defvar *btree-map-callback-counter* 1
;;  "
;;\\Purposelabel
;; A counter used for identifying BTree map callbacks.")


;;; ---------------------------------------------------------------------------
(defconstant +map-buffer-size+ 256
  #+:lisp-doc "
\\Purposelabel
 Size of the mapping buffers used for returning multiple values from
 the \\plob\\ low-level btree mapping functions.")

;;; ---------------------------------------------------------------------------
(defun sh-btree-map-search
  (p-heap-objid p-objid-mapper p-objid-btree
		&optional
		(immediate-key-start-value    +min-tag+)
		(immediate-key-start-type-tag +min-tag+)
		(compare-start-key            +less-equal+)
		(immediate-key-end-value      +max-tag+)
		(immediate-key-end-type-tag   +max-tag+)
		(compare-end-key              +less-equal+)
		descending)
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid-mapper)
       (catch-errors
	(c-sh-btree-map-search p-objid-mapper p-heap-objid p-objid-btree
			       immediate-key-start-value
			       immediate-key-start-type-tag
			       compare-start-key
			       immediate-key-end-value
			       immediate-key-end-type-tag
			       compare-end-key
			       (if descending +c-true+ +c-false+))))
       p-heap-objid p-objid-mapper))

;;; ---------------------------------------------------------------------------
(defun sh-btree-map-search-by-float
  (p-heap-objid p-objid-mapper p-objid-btree
		&optional
		(immediate-key-start-single-float most-negative-single-float)
		(immediate-key-start-type-tag +min-tag+)
		(compare-start-key +less-equal+)
		(immediate-key-end-single-float most-positive-single-float)
		(immediate-key-end-type-tag +max-tag+)
		(compare-end-key +less-equal+)
		descending)
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid-mapper)
       (catch-errors
	(c-sh-btree-map-search-by-float
	 0 p-objid-mapper p-heap-objid p-objid-btree
	 (coerce immediate-key-start-single-float 'double-float)
	 immediate-key-start-type-tag
	 compare-start-key
	 (coerce immediate-key-end-single-float 'double-float)
	 immediate-key-end-type-tag
	 compare-end-key
	 (if descending +c-true+ +c-false+))))
       p-heap-objid p-objid-mapper))

;;; ---------------------------------------------------------------------------
(defun sh-btree-map-search-by-double
  (p-heap-objid p-objid-mapper p-objid-btree
		&optional
		(immediate-key-start-double-float most-negative-double-float)
		(immediate-key-start-type-tag +min-tag+)
		(compare-start-key +less-equal+)
		(immediate-key-end-double-float most-positive-double-float)
		(immediate-key-end-type-tag +max-tag+)
		(compare-end-key +less-equal+)
		descending)
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid-mapper)
       (catch-errors
	(c-sh-btree-map-search-by-double
	 0 p-objid-mapper p-heap-objid p-objid-btree
	 immediate-key-start-double-float
	 immediate-key-start-type-tag
	 compare-start-key
	 immediate-key-end-double-float
	 immediate-key-end-type-tag
	 compare-end-key
	 (if descending +c-true+ +c-false+))))
       p-heap-objid p-objid-mapper))

;;; ---------------------------------------------------------------------------
(defun sh-btree-map-search-by-string
  (p-heap-objid p-objid-mapper p-objid-btree
		&optional
		(immediate-key-start-string " ")
		(immediate-key-start-type-tag +min-tag+)
		(compare-start-key +less-equal+)
		(immediate-key-end-string "~")
		(immediate-key-end-type-tag +max-tag+)
		(compare-end-key +less-equal+)
		descending)
  (with-handle-lock-conflict
   #'(lambda (p-heap-objid p-objid-mapper)
       (catch-errors
	(c-sh-btree-map-search-by-string
	 p-objid-mapper p-heap-objid p-objid-btree
	 immediate-key-start-string
	 immediate-key-start-type-tag
	 compare-start-key
	 immediate-key-end-string
	 immediate-key-end-type-tag
	 compare-end-key
	 (if descending +c-true+ +c-false+))))
       p-heap-objid p-objid-mapper))

;;; ---------------------------------------------------------------------------
(defconstant +btree-mapper-origin->symbol+
  `((,+seek-set+     . :set)
    (,+seek-current+ . :current)
    (,+seek-end+     . :end))
  #+:lisp-doc "
\\Purposelabel
 Internal constant.
 Mapping between symbols \\lisp{:set}, \\lisp{:current}\\,
 \\lisp{:end}\\ and their \\plob\\ C level numeric BTree mapper origin
 companions {\\bf +seek-set+}, {\\bf +seek-current+}, {\\bf +seek-end+}.
\\Seealsolabel
  \\Fcite{btree-mapper-seek}.")

;;; ---------------------------------------------------------------------------
(defun sh-btree-map-seek
  (p-heap-objid p-objid-mapper increment origin)
  (let ((numeric-origin (car (rassoc origin
				     +btree-mapper-origin->symbol+))))
    (unless numeric-origin
      (setf numeric-origin origin))
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid-mapper)
	 (catch-errors
	  (c-sh-btree-map-seek
	   p-heap-objid p-objid-mapper
	   increment numeric-origin 0 0 0 0)))
     p-heap-objid p-objid-mapper)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-map-seek-set
  (p-heap-objid p-objid-mapper increment origin
		immediate-data-value immediate-data-type-tag)
  (let ((numeric-origin (car (rassoc origin
				     +btree-mapper-origin->symbol+))))
    (unless numeric-origin
      (setf numeric-origin origin))
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid-mapper)
	 (catch-errors
	  (c-sh-btree-map-seek-set
	   p-heap-objid p-objid-mapper
	   increment numeric-origin
	   immediate-data-value immediate-data-type-tag)))
     p-heap-objid p-objid-mapper)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-map (p-heap-objid p-objid-btree map-function
		     &optional
		     (immediate-key-start-value    +min-tag+)
		     (immediate-key-start-type-tag +min-tag+)
		     (compare-start-key            +greater-equal+)
		     (immediate-key-end-value      +max-tag+)
		     (immediate-key-end-type-tag   +max-tag+)
		     (compare-end-key              +less-equal+)
		     descending)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-start-value}\\ resp.\\ %
       \\funarg{immediate-key-end-value}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{immediate-key-start-type-tag}\\ resp.\\ %
              \\funarg{immediate-key-end-type-tag}}
\\Purposelabel
 C interface function for
 \\fcite{mapbtree-internal (t t integer t integer t t)}\\ and
 \\fcite{mapbtree-internal (t t persistent-object t t t t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{mapbtree-internal (t t integer t integer t t)};
 \\fcite{mapbtree-internal (t t persistent-object t t t t)}."

  (declare (type fixnum p-heap-objid p-objid-btree
		 immediate-key-start-value immediate-key-start-type-tag
                 immediate-key-end-value immediate-key-end-type-tag))

  (with-dynamic-int-vectors
      ((p-objid-mapper 0)
       (key-value-buffer  (make-int-vector +map-buffer-size+))
       (key-type-buffer   (make-int-vector +map-buffer-size+))
       (data-value-buffer (make-int-vector +map-buffer-size+))
       (data-type-buffer  (make-int-vector +map-buffer-size+)))
    (loop as iterated of-type fixnum = 0
      finally (return iterated)
      for mapped = (multiple-value-setq
                       (mapped p-objid-mapper)
                       (with-handle-lock-conflict
                        #'(lambda (p-heap-objid p-objid-btree)
                            (catch-errors
                              (c-sh-btree-map-first
                               p-objid-mapper p-heap-objid p-objid-btree
                               immediate-key-start-value
                               immediate-key-start-type-tag
			       compare-start-key
                               immediate-key-end-value
                               immediate-key-end-type-tag
			       compare-end-key descending
                               +map-buffer-size+
                               (int-vector-to-c-pointer key-value-buffer)
			       (int-vector-to-c-pointer key-type-buffer)
                               (int-vector-to-c-pointer data-value-buffer)
			       (int-vector-to-c-pointer data-type-buffer))))
                        p-heap-objid p-objid-btree))
      then
      (with-handle-lock-conflict
       #'(lambda (p-heap-objid p-objid-btree)
           (declare (ignore p-objid-btree))
           (catch-errors
             (c-sh-btree-map-next p-objid-mapper p-heap-objid
                                  +map-buffer-size+
                                  (int-vector-to-c-pointer key-value-buffer)
				  (int-vector-to-c-pointer key-type-buffer)
                                  (int-vector-to-c-pointer data-value-buffer)
				  (int-vector-to-c-pointer data-type-buffer))))
       p-heap-objid p-objid-btree)
      thereis
      (loop for i of-type fixnum from 0 below mapped
        do
        (incf iterated)
        (unless (funcall map-function
                         (int-vector key-value-buffer i)
                         (int-vector key-type-buffer i)
                         (int-vector data-value-buffer i)
                         (int-vector data-type-buffer i))
          (when (= mapped +map-buffer-size+)
            (catch-errors (c-sh-btree-map-last p-heap-objid
                                               p-objid-mapper)))
          (return iterated)))
      while (= mapped +map-buffer-size+))))

;;; ---------------------------------------------------------------------------
(defun sh-btree-map-by-float (p-heap-objid p-objid-btree map-function
			      &optional
			      (immediate-key-start-single-float
			       most-negative-single-float)
			      (immediate-key-start-type-tag +min-tag+)
			      (compare-start-key +greater-equal+)
			      (immediate-key-end-single-float
			       most-positive-single-float)
			      (immediate-key-end-type-tag +max-tag+)
			      (compare-end-key +less-equal+)
			      descending)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-start-single-float}\\ resp.\\ %
       \\funarg{immediate-key-end-single-float}}
      {a single float}
 \\isatypetag{\\funarg{immediate-key-start-type-tag}\\ resp.\\ %
              \\funarg{immediate-key-end-type-tag}}
\\Purposelabel
 C interface function for
 \\fcite{mapbtree-internal (t t float t float t t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{mapbtree-internal (t t float t float t t)}."

  (declare (type fixnum p-heap-objid p-objid-btree
		 immediate-key-start-type-tag immediate-key-end-type-tag))

  (with-dynamic-int-vectors
      ((p-objid-mapper 0)
       (key-value-buffer  (make-int-vector +map-buffer-size+))
       (key-type-buffer   (make-int-vector +map-buffer-size+))
       (data-value-buffer (make-int-vector +map-buffer-size+))
       (data-type-buffer  (make-int-vector +map-buffer-size+)))
    (loop as iterated of-type fixnum = 0
      finally (return iterated)
      for mapped = (multiple-value-setq
                       (mapped p-objid-mapper)
		       (with-handle-lock-conflict
                        #'(lambda (p-heap-objid p-objid-btree)
                            (catch-errors
                              (c-sh-btree-map-first-by-float
                               ;; 1996/1029 HK: For the first
                               ;; argument, see comment in
                               ;; plobbtree.h:
                               0
                               p-objid-mapper p-heap-objid p-objid-btree
                               (coerce immediate-key-start-single-float
                                       'double-float)
                               immediate-key-start-type-tag
			       compare-start-key
                               (coerce immediate-key-end-single-float
                                       'double-float)
                               immediate-key-end-type-tag
			       compare-end-key descending
                               +map-buffer-size+
                               (int-vector-to-c-pointer key-value-buffer)
			       (int-vector-to-c-pointer key-type-buffer)
                               (int-vector-to-c-pointer data-value-buffer)
			       (int-vector-to-c-pointer data-type-buffer))))
                        p-heap-objid p-objid-btree))
      then
      (with-handle-lock-conflict
       #'(lambda (p-heap-objid p-objid-btree)
           (declare (ignore p-objid-btree))
           (catch-errors
             (c-sh-btree-map-next p-objid-mapper p-heap-objid
                                  +map-buffer-size+
                                  (int-vector-to-c-pointer key-value-buffer)
				  (int-vector-to-c-pointer key-type-buffer)
                                  (int-vector-to-c-pointer data-value-buffer)
				  (int-vector-to-c-pointer data-type-buffer))))
       p-heap-objid p-objid-btree)
      thereis
      (loop for i of-type fixnum from 0 below mapped
        do
        (incf iterated)
        (unless (funcall map-function
                         (int-vector key-value-buffer i)
                         (int-vector key-type-buffer i)
                         (int-vector data-value-buffer i)
                         (int-vector data-type-buffer i))
          (when (= mapped +map-buffer-size+)
            (catch-errors (c-sh-btree-map-last p-heap-objid
                                               p-objid-mapper)))
          (return iterated)))
      while (= mapped +map-buffer-size+))))

;;; ---------------------------------------------------------------------------
(defun sh-btree-map-by-double (p-heap-objid p-objid-btree map-function
			       &optional
			       (immediate-key-start-double-float
				most-negative-double-float)
			       (immediate-key-start-type-tag +min-tag+)
			       (compare-start-key +greater-equal+)
			       (immediate-key-end-double-float
				most-positive-double-float)
			       (immediate-key-end-type-tag +max-tag+)
			       (compare-end-key +less-equal+)
			       descending)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-start-double-float}\\ resp.\\ %
       \\funarg{immediate-key-end-double-float}}
      {a double float}
 \\isatypetag{\\funarg{immediate-key-start-type-tag}\\ resp.\\ %
              \\funarg{immediate-key-end-type-tag}}
\\Purposelabel
 C interface function for
 \\fcite{mapbtree-internal (t t float t float t t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{mapbtree-internal (t t float t float t t)}."

  (declare (type fixnum p-heap-objid p-objid-btree
		 immediate-key-start-type-tag immediate-key-end-type-tag))

  (with-dynamic-int-vectors
      ((p-objid-mapper 0)
       (key-value-buffer  (make-int-vector +map-buffer-size+))
       (key-type-buffer   (make-int-vector +map-buffer-size+))
       (data-value-buffer (make-int-vector +map-buffer-size+))
       (data-type-buffer  (make-int-vector +map-buffer-size+)))
    (loop as iterated of-type fixnum = 0
      finally (return iterated)
      for mapped = (multiple-value-setq
                       (mapped p-objid-mapper)
		       (with-handle-lock-conflict
                        #'(lambda (p-heap-objid p-objid-btree)
                            (catch-errors
                              (c-sh-btree-map-first-by-double
                               ;; 1996/1029 HK: For the first
                               ;; argument, see comment in
                               ;; plobbtree.h:
                               0
                               p-objid-mapper p-heap-objid p-objid-btree
                               immediate-key-start-double-float
                               immediate-key-start-type-tag
			       compare-start-key
                               immediate-key-end-double-float
                               immediate-key-end-type-tag
			       compare-end-key descending
                               +map-buffer-size+
                               (int-vector-to-c-pointer key-value-buffer)
			       (int-vector-to-c-pointer key-type-buffer)
                               (int-vector-to-c-pointer data-value-buffer)
			       (int-vector-to-c-pointer data-type-buffer))))
                        p-heap-objid p-objid-btree))
      then
      (with-handle-lock-conflict
       #'(lambda (p-heap-objid p-objid-btree)
           (declare (ignore p-objid-btree))
           (catch-errors
             (c-sh-btree-map-next p-objid-mapper p-heap-objid
                                  +map-buffer-size+
                                  key-value-buffer key-type-buffer
                                  data-value-buffer data-type-buffer)))
       p-heap-objid p-objid-btree)
      thereis
      (loop for i of-type fixnum from 0 below mapped
        do
        (incf iterated)
        (unless (funcall map-function
                         (int-vector key-value-buffer i)
                         (int-vector key-type-buffer i)
                         (int-vector data-value-buffer i)
                         (int-vector data-type-buffer i))
          (when (= mapped +map-buffer-size+)
            (catch-errors (c-sh-btree-map-last p-heap-objid
                                               p-objid-mapper)))
          (return iterated)))
      while (= mapped +map-buffer-size+))))

;;; ---------------------------------------------------------------------------
(defun sh-btree-map-by-string (p-heap-objid p-objid-btree map-function
			       &optional
			       (immediate-key-start-vector " ")
			       (immediate-key-start-type-tag +min-tag+)
			       (compare-start-key +greater-equal+)
			       (immediate-key-end-vector "~")
			       (immediate-key-end-type-tag +max-tag+)
			       (compare-end-key +less-equal+)
			       descending)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-start-vector}\\ resp.\\ %
       \\funarg{immediate-key-end-vector}}
      {a string}
 \\isatypetag{\\funarg{immediate-key-start-type-tag}\\ resp.\\ %
              \\funarg{immediate-key-end-type-tag}}
\\Purposelabel
 C interface function for
 \\fcite{mapbtree-internal (t t string t string t t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{mapbtree-internal (t t string t string t t)}."

  (declare (type fixnum p-heap-objid p-objid-btree
		 immediate-key-start-type-tag immediate-key-end-type-tag))

  (with-dynamic-int-vectors
      ((p-objid-mapper 0)
       (key-value-buffer  (make-int-vector +map-buffer-size+))
       (key-type-buffer   (make-int-vector +map-buffer-size+))
       (data-value-buffer (make-int-vector +map-buffer-size+))
       (data-type-buffer  (make-int-vector +map-buffer-size+)))
    (loop as iterated of-type fixnum = 0
      finally (return iterated)
      for mapped = (multiple-value-setq
                       (mapped p-objid-mapper)
		       (with-handle-lock-conflict
                        #'(lambda (p-heap-objid p-objid-btree)
                            (catch-errors
                              (c-sh-btree-map-first-by-string
                               p-objid-mapper p-heap-objid p-objid-btree
                               immediate-key-start-vector
                               immediate-key-start-type-tag
			       compare-start-key
                               immediate-key-end-vector
                               immediate-key-end-type-tag
			       compare-end-key descending
                               +map-buffer-size+
                               (int-vector-to-c-pointer key-value-buffer)
			       (int-vector-to-c-pointer key-type-buffer)
                               (int-vector-to-c-pointer data-value-buffer)
			       (int-vector-to-c-pointer data-type-buffer))))
                        p-heap-objid p-objid-btree))
      then
      (with-handle-lock-conflict
       #'(lambda (p-heap-objid p-objid-btree)
           (declare (ignore p-objid-btree))
           (catch-errors
             (c-sh-btree-map-next p-objid-mapper p-heap-objid
                                  +map-buffer-size+
                                  (int-vector-to-c-pointer key-value-buffer)
				  (int-vector-to-c-pointer key-type-buffer)
                                  (int-vector-to-c-pointer data-value-buffer)
				  (int-vector-to-c-pointer data-type-buffer))))
       p-heap-objid p-objid-btree)
      thereis
      (loop for i of-type fixnum from 0 below mapped
        do
        (incf iterated)
        (unless (funcall map-function
                         (int-vector key-value-buffer i)
                         (int-vector key-type-buffer i)
                         (int-vector data-value-buffer i)
                         (int-vector data-type-buffer i))
          (when (= mapped +map-buffer-size+)
            (catch-errors (c-sh-btree-map-last p-heap-objid
                                               p-objid-mapper)))
          (return iterated)))
      while (= mapped +map-buffer-size+))))

;;; ---------------------------------------------------------------------------
(defun sh-btree-search-return-value (found
                                     immediate-key-value
                                     immediate-key-type-tag
                                     immediate-data-value
                                     immediate-data-type-tag)
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{found}}
       {one of the values of the constants
        {\\bf +btree-found+} or {\\bf +btree-not-found+}}
  \\isa{\\funarg{immediate-data-value}}
      {either an immediate value or an \\objid}
  \\isatypetag{\\funarg{immediate-data-type-tag}}
\\Valueslabel
 If \\funarg{found}\\ is {\\bf +btree-found+}, return \\lispnil;
 if \\funarg{found}\\ is {\\bf +btree-not-found+}, return
 the three values \\lispnil, \\funarg{immediate-data-value}\\ and
 \\funarg{immediate-data-type-tag};
 otherwise, signal an error.
\\Purposelabel
 Canonicalize the low-level return value \\funarg{found}\\ returned
 from one of the {\\bf sh-btree-search\\ldots} functions so that it
 matches the return value of \\fcite{gethash}.
\\Seealsolabel
 Return value of \\fcite{gethash}."

  (cond ((= found +btree-not-found+)
	 nil)
	((= found +btree-found+)
	 (values t immediate-key-value immediate-key-type-tag
                 immediate-data-value immediate-data-type-tag))
	(t (when (and *verbose* (>= *verbose* 1))
	     (cerror
	      "Ignore return value."
	      "Received unknown return value ~A from (c-sh-btree-search ...)."
	      found))
	   (values))))

;;; ---------------------------------------------------------------------------
(defun sh-btree-search (p-heap-objid p-objid-btree
                                     immediate-key-value
				     immediate-key-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-value}}
      {either an immediate value or an \\objid}
 \\isatypetag{\\funarg{immediate-key-type-tag}}
\\Purposelabel
 C interface function for many methods of
 \\fcite{getbtree-with-data}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{getbtree-with-data}."

  (declare (type fixnum p-objid-btree
		 immediate-key-value immediate-key-type-tag))
  (multiple-value-call #'sh-btree-search-return-value
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid-btree)
         (catch-errors
          (c-sh-btree-search p-heap-objid p-objid-btree
			     immediate-key-value immediate-key-type-tag
                             0 0 0 0)))
     p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-search-by-float (p-heap-objid p-objid-btree
					      immediate-key-single-float)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-single-float}}
      {a single float}
\\Purposelabel
 C interface function for \\fcite{getbtree-with-data (float t t t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{getbtree-with-data (float t t t)}."

  (declare (type fixnum p-objid-btree))
  (multiple-value-call #'sh-btree-search-return-value
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid-btree)
         (catch-errors
          (c-sh-btree-search-by-float p-heap-objid p-objid-btree
				      (coerce immediate-key-single-float
					      'double-float)
				      +dynamic-cfloat-ptr-tag+ 0 0 0 0)))
     p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-search-by-double (p-heap-objid p-objid-btree
					       immediate-key-double-float)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-double-float}}
      {a double float}
\\Purposelabel
 C interface function for \\fcite{getbtree-with-data (float t t t)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{getbtree-with-data (float t t t)}."

  (declare (type fixnum p-objid-btree))
  (multiple-value-call #'sh-btree-search-return-value
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid-btree)
         (catch-errors
          (c-sh-btree-search-by-double p-heap-objid p-objid-btree
				       immediate-key-double-float
				       +dynamic-cdouble-ptr-tag+ 0 0 0 0)))
     p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-search-by-string (p-heap-objid p-objid-btree
					       immediate-key-vector
					       &optional
					       (immediate-key-type-tag
						+dynamic-cstring-ptr-tag+))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-heap-objid}\\ resp.\\ \\funarg{p-objid-btree}}
 \\isa{\\funarg{immediate-key-vector}}
      {a simple vector}
 \\isatypetag{\\funarg{immediate-key-type-tag}}
\\Purposelabel
 C interface function for various methods of
 \\fcite{(setf getbtree-with-data)}.
\\Remarkslabel
 It is checked if \\funarg{p-heap-objid}\\ references a persistent
 object of \\fcite{persistent-heap}. It is checked if there is
 an active transaction on \\funarg{p-heap-objid}.

 It is checked if \\funarg{p-objid-btree}\\ references a persistent
 object of \\fcite{persistent-btree}.
\\Seealsolabel
 \\Fcite{getbtree-with-data (string t t t)};
 \\fcite{getbtree-with-data (symbol t t t)}."

  (declare (type fixnum p-objid-btree immediate-key-type-tag))
  (multiple-value-call #'sh-btree-search-return-value
    (with-handle-lock-conflict
     #'(lambda (p-heap-objid p-objid-btree)
         (catch-errors
          (c-sh-btree-search-by-string p-heap-objid p-objid-btree
				       immediate-key-vector
				       immediate-key-type-tag 0 0 0 0)))
     p-heap-objid p-objid-btree)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-test-mode (p-heap-objid
			   p-objid-btree
			   &optional (new-test-mode +btree-get-test-mode+))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid-btree}}
\\Purposelabel
 C interface function for \\fcite{btree-test (t)}\\ and
 \\fcite{(setf btree-test) (t t)}.
\\Seealsolabel
 \\Fcite{btree-test};
 \\fcite{(setf btree-test)}."

  (declare (type fixnum p-heap-objid p-objid-btree new-test-mode))
  (catch-errors (c-sh-btree-test-mode p-heap-objid
				      p-objid-btree new-test-mode)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-root (p-heap-objid p-objid-btree)
  #+:lisp-doc "
\\Purposelabel
  Get the root BTree page of a BTree.
  Used only for inspecting a BTree."
  (declare (type fixnum p-heap-objid p-objid-btree))
  (let ((p-objid-root
         (catch-errors (c-sh-btree-root p-heap-objid p-objid-btree))))
    (when (/= p-objid-root +null-objid+)
      p-objid-root)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-page-parent (p-heap-objid p-objid-btree-page)
  #+:lisp-doc "
\\Purposelabel
  Get the parent object of a single BTree page.
  Used only for inspecting a BTree."
  (declare (type fixnum p-heap-objid p-objid-btree-page))
  (let ((p-objid-parent
         (catch-errors (c-sh-btreepage-parent p-heap-objid
                                              p-objid-btree-page))))
    (when (/= p-objid-parent +null-objid+)
      p-objid-parent)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-page-count (p-heap-objid p-objid-btree-page)
  #+:lisp-doc "
\\Purposelabel
  Get the number of elements of a single BTree page.
  Used only for inspecting a BTree."
  (declare (type fixnum p-heap-objid p-objid-btree-page))
  (catch-errors (c-sh-btreepage-count p-heap-objid p-objid-btree-page)))

;;; ---------------------------------------------------------------------------
(defun sh-btree-page-map (p-heap-objid p-objid-btree-page
                                       map-function)
  #+:lisp-doc "
\\Purposelabel
  Get the elements of a single BTree page.
  Used only for inspecting a BTree."
  (declare (type fixnum p-heap-objid p-objid-btree-page))
  (let ((count (sh-btree-page-count p-heap-objid p-objid-btree-page)))
    (when count
      (let ((count-plus-1 (1+ count)))
	#-:lispworks4 ;; and hopefully not later
        (declare (dynamic-extent count-plus-1))
        (with-dynamic-int-vectors
            ((key-value-buffer  (make-int-vector count-plus-1))
             (key-type-buffer   (make-int-vector count-plus-1))
             (data-value-buffer (make-int-vector count-plus-1))
             (data-type-buffer  (make-int-vector count-plus-1))
             (next-buffer       (make-int-vector count-plus-1)))
          (let ((mapped (with-handle-lock-conflict
                         #'(lambda (p-heap-objid p-objid-btree-page)
                             (catch-errors
                               (c-sh-btreepage-item
                                p-heap-objid p-objid-btree-page
                                -1 count-plus-1
                                (int-vector-to-c-pointer key-value-buffer)
				(int-vector-to-c-pointer key-type-buffer)
                                (int-vector-to-c-pointer data-value-buffer)
				(int-vector-to-c-pointer data-type-buffer)
                                (int-vector-to-c-pointer next-buffer))))
                         p-heap-objid p-objid-btree-page)))
            (loop for i of-type fixnum from 0 below mapped
              finally (return i)
              do
              (let ((next (int-vector next-buffer i)))
                (unless (funcall map-function
                                 (int-vector key-value-buffer i)
                                 (int-vector key-type-buffer i)
                                 (int-vector data-value-buffer i)
                                 (int-vector data-type-buffer i)
                                 (when (/= next +unbound-type-tag+
                                           +null-objid+)
                                   next))
                  (return (1+ i)))))))))))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
