;;;; -*- Package: CL-USER; Mode: LISP; Syntax: ANSI-Common-Lisp -*------------
;;;; Module	defsystem-plob.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	18.11.93
;;;; Description	PLOB - Persistent Lisp OBjects
;;;;		defsystem file
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
;;;; -------------------------------------------------------------------------

(in-package :cl-user)

;;; --------------------------------------------------------------------------
(defconstant +plob-dir+ ;; "/replace/this/by/your/PLOB/installation/directory"
    #+:linux "/import/home0/hkirschk/plob-2.10"
    #+:solaris2 "/home/hkirschk/plob-2.10"
    #+(or :win32 :mswindows) ;; "h:/plob-2.10"
    "c:/home/hkirschk/plob-2.10"
  #+:lisp-doc "\\plob\\ installation directory.")

;;; --------------------------------------------------------------------------
(defparameter +plob-opsys+ 
    (or #+(or :linux :linux86)
	"linux"
	#+(or :sparc :solaris :solaris2)
	"solaris"
	#+(or :r2000 :irix :irix6)
	"irix"
	#+(or :winnt :win32 :windowsnt :mswindows)
	"win32"
	(error "Cannot guess operating system name of this machine ~
	        from *features* list."))
  #+:lisp-doc "Operating system this LISP system runs on.")

;;; --------------------------------------------------------------------------
(defparameter +plob-obj-suffix+ 
    (or #+:unix
	"o"
	"obj")
  #+:lisp-doc "Suffix of object files.")

;;; --------------------------------------------------------------------------
(defparameter +plob-shared-obj-suffix+ 
    (or #+:unix
	"so"
	"dll")
  #+:lisp-doc "
 Suffix of shared object files (those are called DLLs [Dynamic Link
 Libraries] on Windows/NT).")

;;; --------------------------------------------------------------------------
(defparameter +plob-lisp-directory+
    ;;; 1998/03/31 HK: NT has no symbolic links, sigh. For now, the
    ;;; LispWorks sources for NT are loaded from the allegro
    ;;; directory, too.
    (or
     #+(or :allegro (not :unix)) "allegro"
     #+:lispworks                "lispworks")
    "
\\Purposelabel
 The name of the directory containing the source code for the current LISP
 system as a string.")

;;; --------------------------------------------------------------------------
(defparameter +plob-include-lisp-directory+
  (or
   #+(and :allegro (version>= 6)) "allegro6"
   #+(and :allegro (version>= 5)) "allegro5"
   #+(and :allegro (version>= 4)) "allegro4"
   #+:lispworks4		  "lispworks4"
   #+:lispworks3	     	  "lispworks3")
    "
\\Purposelabel
 The name of the include directory containing the source code for the
 current LISP system as a string. This directory contains the foreign
 function interface files to the C layer.")

;;; --------------------------------------------------------------------------
(defparameter +plob-target-lisp-directory+
  (or
   #+(and :allegro (version>= 6)) "/allegro6"
   #+(and :allegro (version>= 5)) "/allegro5"
   #+(and :allegro (version>= 4)) "/allegro4"
   "")
    "
\\Purposelabel
 The name of the target LISP directory containing the .fasl files.")

;;; --------------------------------------------------------------------------
(setf (logical-pathname-translations "PLOB")
      `(("SOURCE;**;*.lisp"
	 ,(concatenate 'string +plob-dir+ "/src/"
		       +plob-lisp-directory+ "/**/*.lisp"))
	("SOURCE;**;*.fasl"
	 ,(concatenate 'string +plob-dir+ "/src/"
		       +plob-lisp-directory+
		       +plob-target-lisp-directory+
		       "/*.fasl"))
	("UTIL;**;*.lisp"
	 ,(concatenate 'string +plob-dir+ "/src/util/**/*.lisp"))
	("UTIL;**;*.fasl"
	 ,(concatenate 'string +plob-dir+ "/src/util"
		       +plob-target-lisp-directory+
		       "/*.fasl"))
	("EXAMPLE;**;*.*"
	 ,(concatenate 'string +plob-dir+ "/src/example/**/"))
	("CCODE;**;*.o"
	 ,(concatenate 'string +plob-dir+ "/src/**/" +plob-opsys+ "/*."
		       +plob-obj-suffix+))
        ("CLIBS;**;*.so"
	 ,(concatenate 'string +plob-dir+ "/lib/" +plob-opsys+ "/**/*."
		       +plob-shared-obj-suffix+))
        ("INCLUDE;**;*.*"
	 ,(concatenate 'string +plob-dir+ "/src/include/"
		       +plob-include-lisp-directory+ "/**/"))
        ("BIN;**;*.*"
	 ,(concatenate 'string +plob-dir+ "/bin/" +plob-opsys+ "/**/"))
        ("APIMANUAL;**;*.tex"
	 ,(concatenate 'string +plob-dir+ "/tex/eref/**/*.tex"))
        ("INTMANUAL;**;*.tex"
	 ,(concatenate 'string +plob-dir+ "/tex/iref/**/*.tex"))
	))

;;; --------------------------------------------------------------------------
(defconstant +plob-non-documented-members+

  (mapcar #'logical-pathname
	  '("PLOB:SOURCE;plob-defpackage.lisp"
	    "PLOB:SOURCE;ff-mapping.lisp"

	    "PLOB:INCLUDE;plob.lisp"
	    "PLOB:INCLUDE;plobff.lisp"
	    "PLOB:INCLUDE;plobmisc.lisp"
	    "PLOB:INCLUDE;plobtype.lisp"
	    "PLOB:INCLUDE;plobnumber.lisp"
	    "PLOB:INCLUDE;plobsequ.lisp"
	    "PLOB:INCLUDE;plobstruct.lisp"
	    "PLOB:INCLUDE;plobclos.lisp"
	    "PLOB:INCLUDE;plobroot.lisp"
	    "PLOB:INCLUDE;ploblock.lisp"
	    "PLOB:INCLUDE;plobheap.lisp"
	    "PLOB:INCLUDE;plobbtree.lisp"
	    "PLOB:INCLUDE;plobadmin.lisp"

	    ;; "PLOB:SOURCE;plob-test.lisp"
	    ))
  #+:lisp-doc "Contains all file names belonging to the \\plob\\ system which
 are not documented.")

;;; --------------------------------------------------------------------------
(defconstant +plob-documented-members+

  (mapcar #'logical-pathname
	  '("PLOB:SOURCE;plob-url.lisp"
	    "PLOB:SOURCE;plob-defaults.lisp"
            "PLOB:SOURCE;plob-error.lisp"
            "PLOB:SOURCE;plob-extent.lisp"
	    "PLOB:SOURCE;plob-evolution.lisp"
	    "PLOB:SOURCE;plob-sysdep.lisp"
	    "PLOB:SOURCE;plob-metaclass-1.lisp"
	    "PLOB:SOURCE;plob-metaclass-2.lisp"
	    "PLOB:SOURCE;plob-metaclass-3.lisp"
	    "PLOB:SOURCE;plob-defines.lisp"

	    "PLOB:SOURCE;plob-int.lisp"
	    "PLOB:SOURCE;plob-low.lisp"
	    "PLOB:SOURCE;plob-objid-buffer.lisp"

	    "PLOB:SOURCE;plob-writers.lisp"

	    "PLOB:SOURCE;plob-heap.lisp"
	    "PLOB:SOURCE;plob-btree.lisp"
	    "PLOB:SOURCE;plob-btree-mapper.lisp"
	    "PLOB:SOURCE;plob-session.lisp"
	    "PLOB:SOURCE;plob-machine.lisp"

	    "PLOB:SOURCE;plob-misc.lisp"

	    "PLOB:SOURCE;plob-cons.lisp"
	    "PLOB:SOURCE;plob-symbol.lisp"
	    "PLOB:SOURCE;plob-function.lisp"
	    "PLOB:SOURCE;plob-vector.lisp"
	    "PLOB:SOURCE;plob-array.lisp"
	    "PLOB:SOURCE;plob-string.lisp"
	    "PLOB:SOURCE;plob-bit-vector.lisp"
	    "PLOB:SOURCE;plob-numeric.lisp"
	    #+:lispworks3
	    "PLOB:SOURCE;plob-tlatter.lisp"

	    "PLOB:SOURCE;plob-struct-descr.lisp"
	    "PLOB:SOURCE;plob-struct.lisp"
	    "PLOB:SOURCE;plob-struct-slot-descr.lisp"
	    "PLOB:SOURCE;plob-struct-hash-table.lisp"
	    "PLOB:SOURCE;plob-struct-package.lisp"
	    "PLOB:SOURCE;plob-struct-root.lisp"

	    "PLOB:SOURCE;plob-clos-descr.lisp"
	    "PLOB:SOURCE;plob-clos.lisp"
	    "PLOB:SOURCE;plob-clos-slot-descr.lisp"
	    "PLOB:SOURCE;plob-clos-slot-value.lisp"
	    "PLOB:SOURCE;plob-method-descr.lisp"

	    "PLOB:SOURCE;plob-database.lisp"

	    "PLOB:SOURCE;plob-builtin.lisp"
	    "PLOB:SOURCE;plob-inspect.lisp"
	    "PLOB:SOURCE;plob-bootstrap.lisp"

	    "PLOB:UTIL;plob-sexpr.lisp"
	    ))
  #+:lisp-doc "Contains all file names belonging to the \\plob\\ system which
 are documented.")

;;; --------------------------------------------------------------------------
(defconstant +plob-document-only-members+
  (mapcar #'logical-pathname '("PLOB:SOURCE;plob-extra-doc.lisp"))
  #+:lisp-doc "Contains all file names belonging to the \\plob\\ system which
 are only for documentation purpose.")

;;; --------------------------------------------------------------------------
(defconstant +plob-members+
  (concatenate 'list
               +plob-non-documented-members+
               +plob-documented-members+)
  #+:lisp-doc "Contains all file names belonging to the \\plob\\ system.")

;;; --------------------------------------------------------------------------
(defconstant +logical-pathname-class+
    (find-class 'logical-pathname)
   "The \\clsmo\\ of class \\class{logical-pathname}.")

;;; --------------------------------------------------------------------------
(defun namestring-if (pathname)
  (namestring (if (eq (class-of pathname) +logical-pathname-class+)
                  (translate-logical-pathname pathname)
                pathname)))

;;; --------------------------------------------------------------------------
#+:lispworks
(eval `(defsystem::defsystem :plob
	   ()
	 :members ,(mapcar #'namestring-if +plob-members+)
	 :rules
	 ((:in-order-to :compile :all
			(:requires (:load :previous))))))

;;; --------------------------------------------------------------------------
#+:allegro
(eval `(defsystem::defsystem :plob
	   (:default-pathname "PLOB:SOURCE;"
	       :pretty-name "PLOB!")
	 (:serial ,@(mapcar #'namestring +plob-members+))))

;;; --------------------------------------------------------------------------
(defun compile-plob (&optional (optimize nil))
  (let (#+:lispworks3
	(system::*handle-warn-on-redefinition* :warn)
	#+:lispworks4 ;; or later
	(lispworks::*handle-warn-on-redefinition* :warn)
	#+:allegro
	(excl:*enable-package-locked-errors* nil))
    #+:lispworks3
    (declare (special system::*handle-warn-on-redefinition*))
    #+:lispworks4 ;; or later
    (declare (special lispworks::*handle-warn-on-redefinition*))
    #+:allegro
    (declare (special excl:*enable-package-locked-errors*))
    (unwind-protect
        (progn
          (when optimize
            (declaim (optimize (speed 3) (space 0) (safety 1) (debug 1)
			       #-:lispworks4 ;; or later
                               (interruptable 1)
			       (compilation-speed 0))))
          (defsystem::compile-system :plob))
      (declaim (optimize (speed 1) (space 0) (safety 3) (debug 3)
                         #-:lispworks4 ;; or later
                         (interruptable 3)
		         (compilation-speed 0))))))

;;; --------------------------------------------------------------------------
(defun load-plob (&optional(and-use-package t))
  (let (#+:lispworks3
	(system::*handle-warn-on-redefinition* :warn)
	#+:lispworks4 ;; or later
	(lispworks::*handle-warn-on-redefinition* :warn)
	#+:allegro
	(excl:*enable-package-locked-errors* nil))
    #+:lispworks3
    (declare (special system::*handle-warn-on-redefinition*))
    #+:lispworks4 ;; or later
    (declare (special lispworks::*handle-warn-on-redefinition*))
    #+:allegro
    (declare (special excl:*enable-package-locked-errors*))
    (defsystem::load-system :plob))
  (pushnew :plob *features*)
  (when and-use-package
    (use-package :plob)))

;;; --------------------------------------------------------------------------
(defconstant +plob-document-files+
  (concatenate 'list
               +plob-document-only-members+
               +plob-documented-members+))

;;; --------------------------------------------------------------------------
(defconstant +plob-document-api+
  (logical-pathname "PLOB:APIMANUAL;ploberef.tex"))

;;; --------------------------------------------------------------------------
(defconstant +plob-document-internals+
  (logical-pathname "PLOB:INTMANUAL;plobiref.tex"))

;;; --------------------------------------------------------------------------
(defun scan-plob-files (&optional (plob-files +plob-document-files+))
  #+:lisp-doc "
\\Purposelabel
 Scan all \\plob\\ .lisp files except the very low level foreign
 language interface files for their documentation strings and make a
 {\\tt `plobdoc.tex'} file. This \\TeX\\ file is included into the
 \\plob\\ User's Guide and Reference Manual.
\\Seealsolabel
 \\Fcite{scan-files}."

  (labels ((filter-externals (name)
	     (etypecase name
	       (string t)
	       (cons
		(externalp (cadr name)))
	       (symbol
		(externalp name))))
	   (filter-internals (name)
	     (typecase name
	       (string t)
	       (t (not (filter-externals name))))))
  
    (scan-files (logical-pathname "PLOB:EXAMPLE;plob-example.lisp")
		(logical-pathname "PLOB:APIMANUAL;plobexam.tex"))
    (let ((*features* (append *features* (list :document-api))))
      (scan-files plob-files
		  +plob-document-api+
		  #'filter-externals))
    (scan-files plob-files
		+plob-document-internals+
		#'filter-internals)))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
