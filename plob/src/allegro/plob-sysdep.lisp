;;;; -*-Lisp-*-----------------------------------------------------------------
;;;; Module	plob-sysdep.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	24.11.93
;;;; Description	Interface between PLOB and the used LISP system.
;;;;		Here is the interface to many functions found often in the
;;;;		LISP package "SYSTEM".
;;;;		The whole "plob-sysdep" module depends HEAVILY on the
;;;;		actual used LISP system; the packages using this package
;;;;		should be independent from the actual used LISP system.
;;;;
;;;; The bibliography references used are:
;;;; [AP91]	Andreas Paepke:
;;;;		User-Level Language Crafting:
;;;;		Introducing the CLOS Metaobject Protocol
;;;;		HP Laboratories Technical Report HPL-91-169, October, 1991
;;;; and from:
;;;; [MOP91]	Gregor Kiczales, Jim des Rivieres, and Daniel G. Bobrow:
;;;;		The Art of the Metaobject Protocol
;;;;		The MIT Press, Cambridge, Massachusetts, 1991
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
(defun prompt-for (&optional (type-name t) &rest format-and-args)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{type-name}}
      {a symbol denoting a valid \\cl\\ type name}
 \\isa{\\funarg{format-and-args}}
      {an argument list as accepted by the
       \\fcite{format}\\ without the
       {\\sl destination} argument}
\\Valueslabel
 Returns the evaluation of an expression entered by the user whose
 type matches \\funarg{type-name}.
\\Purposelabel
 Prompt the user with \\funarg{format-and-args}\\ and ask for an
 expression whose type matches \\funarg{type-name}.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{prompt-for}."

  #+:allegro
  (apply #'excl::prompt-for-input type-name t format-and-args)
  #+:lispworks
  (progn
    (if format-and-args
	(apply #'format t format-and-args))
    (loop (let* ((result (car (system::prompt-for-value)))
		 (type-of-result (type-of result)))
	    (handler-bind ((error
			    #'(lambda (condition)
				(declare (ignore condition))
				(return result))))
	      (if (subtypep type-of-result type-name)
		  (return result)))
	    (format t "Expected expression with type ~A, not ~A.~%"
		    type-name type-of-result)))))

;;; ---------------------------------------------------------------------------
(defmacro push-on-end (value location)
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{value}}
       {an expression}
  \\isa{\\funarg{location}}
       {a \\lisp{setf}-able expression}
\\Purposelabel
 {\\bf push-on-end} is like the \\fcite{push}\\ except
 it uses the other end.
\\Seealsolabel
 \\Fcite{push-on-end}; \\fcite{push}."
  `(setf ,location (nconc ,location (list ,value))))

;;; ---------------------------------------------------------------------------
(defun mapappend (fun &rest args)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{fun}}
      {a function taking a list argument}
 \\isa{\\funarg{args}}
      {a list}
\\Purposelabel
 {\\bf mapappend} is like the \\fcite{mapcar}\\ except
 that the results are appended together.
\\Seealsolabel
 \\Fcite{mapappend};
 \\fcite{mapcar}."

  (if (some #'null args)
      ()
    (append (apply fun (mapcar #'car args))
	    (apply #'mapappend fun (mapcar #'cdr args)))))

;;; ---------------------------------------------------------------------------
#+:lispworks4 ;; and later
(define-setf-expander assoc (key alist &environment env)
  (multiple-value-bind (temps vals stores store-form access-form)
      (get-setf-expansion alist env)
    (let ((new-value (gensym "NEW-VALUE-"))
          (keyed (gensym "KEYED-"))
          (accessed (gensym "ACCESSED-"))
          (store-new-value (car stores)))
      (values (cons keyed temps)
              (cons key vals)
              `(,new-value)
              `(let* ((,accessed ,access-form)
                      (,store-new-value (assoc ,keyed ,accessed)))
                 (if ,store-new-value
                     (rplacd ,store-new-value ,new-value)
                   (progn
                     (setq ,store-new-value (acons ,keyed ,new-value ,accessed))
                     ,store-form))
                 ,new-value)
              `(assoc ,new-value ,access-form)))))

;;; ---------------------------------------------------------------------------
(defconstant +null-io+
  (or
   #+(and :allegro (version>= 5))
   excl::*null-stream*
   #+(and :lispworks4 (not :lispworks4.1))
   (io::make-null-stream)
   #+:lispworks3
   (system::make-null-stream)
   nil)
  #+:lisp-doc "
\\Purposelabel
 A stream sending its output to nirvana.
\\Remarkslabel
 \\sysdep{constant}

%% Perhaps someone can give me a hint were the term `nirvana' stems
%% from (please, not the suicide-threatened rock-band); send your answer to
%% {\\tt kirschke@kogs26.informatik.uni-hamburg.de}.
%%\\Seealsolabel
%% Section \\fcite{tlatter ...}.
")

;;; ---------------------------------------------------------------------------
(defun compile-silent (name &optional (definition nil definitionp))
  #+:lisp-doc "
\\Purposelabel
 Compile \\funarg{name}\\ with optional \\funarg{definition}\\ without
 writing any annoying messages to \\lisp{*standard-ouput*}.
\\Seealsolabel
 \\Fcite{compile}."

  (let ((standard-output *standard-output*))
    (setf *standard-output* +null-io+)
    (unwind-protect
        (if definitionp
	    (compile name definition)
	  (compile name))
      (setf *standard-output* standard-output))))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defconstant +logical-pathname-class+
    (find-class 'logical-pathname)
   "The \\clsmo\\ of class \\class{logical-pathname}.")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun namestring-if (pathname)
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{pathname}}
       {a [logical] pathname}
\\Valueslabel
 Returns argument \\funarg{pathname}\\ converted to a string.
\\Purposelabel
 Bug-fix for the \\lw\\ \\fcite{namestring}; \\lw\\ does not return a
 correct path expression for logical pathnames.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{namestring}."

  (namestring (if (eq (class-of pathname) +logical-pathname-class+)
                  (translate-logical-pathname pathname)
                pathname)))

;;; ---------------------------------------------------------------------------
#-:lispworks
(defmacro namestring-if (pathname)
  `(namestring ,pathname))

;;; ---------------------------------------------------------------------------
(defun return-nil ()
  #+:lisp-doc "
\\Purposelabel
 Return always \\lispnil."
  nil)

;;; ---------------------------------------------------------------------------
(defun return-t ()
  #+:lisp-doc "
\\Purposelabel
 Return always \\lispt."
  t)

;;; ---------------------------------------------------------------------------
(defconstant +least-negative-p-fixnum+
  (- (ash 1 (1- +p-fixnum-bits+)))
  #+:lisp-doc "The largest representable positive persistent fixnum.")

;;; ---------------------------------------------------------------------------
(defconstant +most-positive-p-fixnum+
  (1- (ash 1 (1- +p-fixnum-bits+)))
  #+:lisp-doc "The largest representable positive persistent fixnum.")


;;; ---------------------------------------------------------------------------
(defun p-fixnump (object)
  #+:lisp-doc "
\\Argumentslabel
  \\isanobject{\\funarg{object}}
\\Purposelabel
  Return \\nonnil\\ if \\funarg{object}\\ is of type fixnum,
  \\lispnil\\ otherwise.
\\Remarkslabel
 \\sysdep{macro}"

  (declare (inline p-fixnump))
  (and (integerp object)
       (<= +least-negative-p-fixnum+ object +most-positive-p-fixnum+)))

;;; ---------------------------------------------------------------------------
(defun get-function-name (function)
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{object}}
       {a function}
\\Purposelabel
 Return the name of \\funarg{function}.
\\Remarkslabel
 \\sysdep{macro}"

  #+:lispworks3
  (typecase function
    (generic-function (generic-function-name function))
    (t (system::function-name function)))

  #-:lispworks3
  (multiple-value-bind (lambda-expression environment-bindings function-name)
      (function-lambda-expression function)
    (declare (ignore lambda-expression environment-bindings))
    function-name))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun push-to-idle-sleep-hook (function-symbol)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{function-symbol}}
      {the name of a function taking no arguments}
\\Purposelabel
 Pushes \\funarg{function-symbol}\\ to the idle sleep hook.
 The symbol-function of \\funarg{function-symbol}\\ is called
 each time when the process goes idle.
\\Remarkslabel
 \\sysdep{function}

 Hint for portability:
 If the \\cl\\ system has no idle sleep hook, raise an error when this
 function is called.
\\Seealsolabel
 \\Fcite{remove-from-idle-sleep-hook};
 \\fcite{is-on-idle-sleep-hook};
 section \\fcite{process ...}."

  (pushnew function-symbol mp:*idle-sleep-hooks*))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun remove-from-idle-sleep-hook (function-symbol)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{function-symbol}}
      {the name of a function taking no arguments}
\\Purposelabel
 Remove \\funarg{function-symbol}\\ from the idle sleep hook.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{push-to-idle-sleep-hook};
 \\fcite{is-on-idle-sleep-hook};
 section \\fcite{process ...}."

  (setf mp:*idle-sleep-hooks*
        (remove function-symbol mp:*idle-sleep-hooks*)))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun is-on-idle-sleep-hook (function-symbol)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{function-symbol}}
      {the name of a function taking no arguments}
\\Purposelabel
 Check if \\funarg{function-symbol}\\ is on the idle sleep hook.
\\Remarkslabel
 \\sysdep{function}

 Hint for portability:
 Should always return \\lispnil\\ if the used \\cl\\ system has no
 idle sleep hook at all.
\\Seealsolabel
 \\Fcite{push-to-idle-sleep-hook};
 \\fcite{remove-from-idle-sleep-hook};
 section \\fcite{process ...}."

  (member function-symbol mp:*idle-sleep-hooks*))

;;; ---------------------------------------------------------------------------
#+:short-float
(defconstant +sfloat-value-bit-offset+
    #+:lispworks3 4
    #-:lispworks3 nil
  #+:lisp-doc "
\\Purposelabel
 Short floats in \\lw\\ are `immediate' values, i.e.\\ they are objects
 which fit into a single 32~bit memory word. The lower 4 bit contain a type
 tag which identifies the memory word as a value of type short-float;
 the type tag's value is
 \\lisp{constants::*sfloat-poi-tag*}\\ (14 for \\lw\\ 3.1.1).
 The remaining 28~bit are the short float's value. This constant is therefore
 the top-secret bit shift amount for the value of a short float itself with
 its type tag removed.

 Hint for \\allegro:
 Short floats and single floats are equivalent in \\allegro.
\\Remarkslabel
 \\sysdep{constant}
\\Seealsolabel
  \\Fcite{fixnum-to-short-float};
  \\fcite{short-float-to-fixnum}.")

;;; ---------------------------------------------------------------------------
#+:short-float
(defconstant +sfloat-poi-tag+
  #+:lispworks3
  constants::*sfloat-poi-tag*
  #+:lisp-doc "
\\Remarkslabel
 Hint for \\lw:
 If \\textbf{constants::*sfloat-poi-tag*} is no longer
 defined: its value in \\lw\\ 3.1 and 3.2 was 14.
\\Seealsolabel
 \\Fcite{+sfloat-value-bit-offset+}.")
  
;;; ---------------------------------------------------------------------------
#+:short-float
(defun short-float-to-fixnum (the-short-float)
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{the-short-float}}
       {a short float}
\\Valueslabel
 Returns \\funarg{the-short-float}\\ converted into an instance
 of class \\class{fixnum}.
\\Purposelabel
 Convert \\funarg{the-short-float}\\ into an instance of class
 fixnum. This is a binary conversion; returned is {\\sl not}
 a rounded value of \\funarg{the-short-float}\\ or so but a
 fixnum instance holding the  binary representation of
 \\funarg{the-short-float}. This depends on the
 fact that a short-float object occupies fewer bits than a
 fixnum object.
\\Remarkslabel
 \\sysdep{function}

 Hint for \\allegro:
 Short floats and single floats are equivalent in \\allegro.
\\Seealsolabel
 \\Fcite{fixnum-to-short-float}."

  (declare (type short-float the-short-float))
  ;; This code relies on LispWork feature, so I'm using LispWorks system
  ;; functions directly. If the expression
  ;;   (system::coerce-to-short-float the-short-float)
  ;; will not work, try the very expensive but always working expression
  ;;   (coerce the-short-float 'short-float)
  (c-short-float-to-fixnum (system::coerce-to-short-float the-short-float)
                           (- +sfloat-value-bit-offset+) 0))

;;; ---------------------------------------------------------------------------
#+:short-float
(defun fixnum-to-short-float (the-fixnum)
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{the-fixnum}}
       {a fixnum}
\\Valueslabel
 Returns \\funarg{the-fixnum}\\ converted into an instance
 of class \\class{short-float}.
\\Purposelabel
 Convert \\funarg{the-fixnum}\\ into an instance of class
 short-float. This is a binary conversion; returned is
 {\\sl not} \\funarg{the-fixnum}\\ coerced
 to type short-float or so but a short-float instance holding
 the binary representation of
 \\funarg{the-fixnum}. This depends on the fact that a
 short-float object occupies fewer bits than a
 fixnum object.
\\Remarkslabel
 \\sysdep{function}

 Hint for \\allegro:
 Short floats and single floats are equivalent in \\allegro.
\\Seealsolabel
 \\Fcite{short-float-to-fixnum}."

  (declare (type fixnum the-fixnum))
  (if +has-short-float-p+
      (c-fixnum-to-short-float the-fixnum
			       +sfloat-value-bit-offset+
			       +sfloat-poi-tag+)
    (coerce (c-short-to-single-float the-fixnum) 'single-float)))

;;; ---------------------------------------------------------------------------
(defconstant +bignum-deref+
    (or
     #+(and :allegro (not (version>= 5)))
     ;; 2001-02-08 HK: ACL 4:
     nil
     #+(and :allegro (not (version>= 6)))
     ;; 2001-02-08 HK: ACL 5:
     t
     #+(and :allegro (version>= 6))
     ;; 2001-02-08 HK: ACL 6:
     nil
     nil)
    #+:lisp-doc "
 \\Purposelabel
  A flag if the pointer to a bignum needs one more dereferencing.
  This is needed for \\allegrocl\\ 5.0; the foreign function mapping
  code in ff-mapping.lisp puts an additional array `around' the bignum
  to address from the C layer.
 \\Remarkslabel
  \\sysdep{constant}")

;;; ---------------------------------------------------------------------------
(defconstant +bignum-header-size+
    (or
     #+(and :allegro (not (version>= 5)))
     ;; 2001-02-08 HK: ACL 4:
     4
     #+(and :allegro (not (version>= 6)))
     ;; 2001-02-08 HK: ACL 5:
     0
     #+(and :allegro (version>= 6))
     ;; 2001-02-08 HK: ACL 6:
     -12 ;; Just a guess ...
     #+:lispworks4 ;; and later
     ;; low:bignum-length
     12
     ;; If constants::*raw-bignum-data-offset* is no longer
     ;; defined: its value in LispWorks 3.1.1 was 8:
     #+:lispworks3
     constants::*raw-bignum-data-offset*
     0)
  #+:lisp-doc "
\\Purposelabel
 Size of \\lw\\ bignum objects header information in bytes.
\\Remarkslabel
 \\sysdep{constant}")

;;; ---------------------------------------------------------------------------
(defconstant +poi-tag+
    (or
     #+(and :allegro (not (version>= 5)))
     ;; 2001-02-08 HK: ACL 4:
     7
     #+(and :allegro (not (version>= 6)))
     ;; 2001-02-08 HK: ACL 5:
     0
     #+(and :allegro (version>= 6))
     ;; 2001-02-08 HK: ACL 6:
     7
     #+:lispworks4 ;; and later
     low:tag-and
     ;; If constants::*poi-tag* is no longer
     ;; defined: its value in LispWorks 3.1.1 was 3:
     #+:lispworks3
     constants::*poi-tag*)
     #+:lisp-doc "
\\Purposelabel
 A mask for adressing a LISP object directly from within C code with
 the help of an \\lisp{:as-is}\\ parameter of the \\lw\\ foreign language
 interface passed down to a C function.
 This is the bitmask which must be used in C for unmasking a C
 \\lisp{:as-is}\\ parameter
 before using that \\lisp{:as-is}\\ parameter in C as a pointer to a
 LISP object; the masked-out bits are a type tag. If they are
 not masked out, a bus error is raised when de-referencing the
 bignum from within a C function.
\\Remarkslabel
 \\sysdep{constant}")

;;; ---------------------------------------------------------------------------
(defconstant +simple-vector-header-size+
    (or
     #+(and :allegro (not (version>= 6)))
     ;; 2001-02-08 HK: ACL 4 & 5:
     0
     #+(and :allegro (version>= 6))
     ;; 2001-02-08 HK: ACL 6: No idea why this must be -12:
     -12
     ;; All other LISP systems:
     0)
  #+:lisp-doc "The header size of a simple-vector.")


;;; ---------------------------------------------------------------------------
(defconstant +structure-class-class+ (find-class 'structure-class)
  #+:lisp-doc "The \\clsmo\\ of class \\class{structure-class}.")

;;; ---------------------------------------------------------------------------
#+:allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +load-llstructs+ t
    "A flag indicating if the llstructs package should be loaded. This
 package is needed only in \\allegrocl\\ 4.x for allocating bignums
 fast. If this package is not available because \\plob\\ should run
 in a runtime environment LISP system, set this constant to
 \\lispnil."))

#+:allegro
(eval-when (compile)
  (when +load-llstructs+
    (require :llstructs)))

;;; ---------------------------------------------------------------------------
(defun find-bignum-size-in-bits (the-bignum)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-bignum}}{a bignum}
\\Valueslabel
 Size of \\funarg{the-bignum}\\ in bits, being negative for a negative
 bignum and positive for a positive bignum.
\\Purposelabel
 Calculates the size of \\funarg{the-bignum}\\ in bits.
\\Remarkslabel
 \\sysdep{function}

 The \\allegrocl\\ code is derived from an example sent to me
 by Duane Rettig \\lisp{duane@Franz.COM}"

  (or

   #+:allegro
   (when +load-llstructs+
	 (let ((the-bignum-size (excl::bm_size the-bignum)))
	   (* (if (minusp (excl::bm_sign the-bignum))
		  (- the-bignum-size)
		the-bignum-size)
	      +bits-per-bigit+)))

   #+:lispworks
   (let ((the-bignum-size (system::bignum-size$bignum the-bignum)))
     (* (if (system::minusp$bignum the-bignum)
	    (- the-bignum-size)
	  the-bignum-size)
	+sizeof-postore-word+ +bits-per-byte+))

   (let ((the-bignum-size (integer-length the-bignum)))
     #+:allegro ;; Align the-bignum-size to the number of bigits:
     (setf the-bignum-size (* (ceiling the-bignum-size +bits-per-bigit+)
			      +bits-per-bigit+))
     #+:lispworks ;; Align the-bignum-size to word size:
     (let ((bits-per-word (* +sizeof-postore-word+ +bits-per-byte+)))
       (setf the-bignum-size (* (ceiling the-bignum-size bits-per-word)
				bits-per-word)))
     (if (minusp the-bignum)
	 (- the-bignum-size)
       the-bignum-size))))

;;; ---------------------------------------------------------------------------
(defun allocate-bignum (size-in-bits)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{size-in-bits}}
      {a fixnum}
\\Valueslabel
 An uninitialized transient object of `class' \\class{bignum}.
\\Purposelabel
 Low-level allocate of a transient instance of `class'
 \\class{bignum}\\ (i.e.\\ {\\sl no} persistent instance
 is allocated) occupying \\funarg{size-in-bits}\\ memory bits.
\\Remarkslabel
 \\sysdep{function}

 Actually, \\class{bignum}\\ is not the name of a class which can be
 located by evaluating \\lisp{(find-class 'bignum)}. It is a fairly
 low-level built-in class which is made up from a collection of low-level
 functions. The `external view' of instances of class
 \\class{bignum}\\ is the class \\class{integer}, but
 \\plob\\ differentiates because of efficency considerations between
 the subclasses \\class{fixnum}\\ (whose instances are handled in an
 `immediate'-fashion) and
 non-fixnum $\\equiv$ \\class{bignum}\\ (whose instances are
 handled in a `non-immediate'-fashion) of class \\class{integer}.

 The \\allegrocl\\ code is derived from an example sent to me
 by Duane Rettig \\lisp{duane@Franz.COM}"

  (declare (type fixnum size-in-bits))
  (or
   #+(and :allegro (not (version>= 5)))
   ;; 1998/11/19 HK: Heaven knows if and how the primcall-stuff works
   ;; for ACL >= 5.0 ...
   (when +load-llstructs+
     (let* ((the-bigits (ceiling (abs size-in-bits) +bits-per-bigit+))
	    (the-bignum (comp::.primcall 'sys::new-bignum the-bigits)))
       (setf (sys::memref the-bignum
			  (sys::target-mdparam 'comp::md-svector-data0-norm)
			  (* 2 (1- the-bigits))
			  :unsigned-word)
	 1)
       the-bignum))
   #+:lispworks
   (system::alloc-bignum$fixnum (ceiling (abs size-in-bits)
					 (* +sizeof-postore-word+
					    +bits-per-byte+))))
  ;; 1998/11/20 HK: Generate the bignum as (1- (expt 2 (abs
  ;; size-in-bits))); to avoid generating maybe a bignum which
  ;; captures the range up to (expt 2 (abs size-in-bits)), the number
  ;; is created as sum of two smaller bignums:
  (let ((summand (ash 1 (1- (abs size-in-bits)))))
    (+ summand (1- summand))))

;;; ---------------------------------------------------------------------------
#+(and :lisp-doc (not :document-api))
(:defdoc
 "array ..."
 "Array Information Functions"
 "
 Arrays are a rather baroque and bizarre `feature' in \\cl,
 especially arrays created with the \\lisp{:displaced-to}\\ option.
 Almost all array information can be obtained by calling the
 appropiate array-specific functions,
 e.g.\\ \\fcite{array-dimensions},
 \\fcite{array-element-type},
 \\fcite{array-rank}\\ etc. The only important
 information which can {\\sl not} be obtained by a well-defined
 \\cl\\ function is the fact if an array is displaced
 to \\cite[\\citepage{444}]{bib:CLtLII} another array
 and to which array it is displaced to.\\footnote{I guess this has been
 omitted from the \\cl\\ standard to give the implementator of a
 \\cl\\ system a big degree of freedom in designing the internal memory
 representation of arrays.}
 The following functions {\\bf array-data-vector},
 {\\bf array-displaced-offset}, {\\bf array-displaced-to} and
 {\\bf array-displaced-to-p} are
 defined here to get around this problem and to allow \\plob\\ to
 cope also with displaced arrays in a \\cl\\ fashion, i.e.\\ to
 obey the references established between displaced arrays.

 Users of these functions should care for the type-declarations
 done for the functions arguments because the called sub-functions
 are very low-low-level functions and expect the declared types;
 \\lw\\ may crash if the argument type is not of the type the
 sub-function expects.
\\Seealsolabel
 \\Fcite{array-data-vector};
 \\fcite{array-displaced-offset};
 \\fcite{array-displaced-to};
 \\fcite{array-displaced-to-p};
 argument \\keyarg{displaced-to}\\ of
 \\fcite{make-array}.")

;;; ---------------------------------------------------------------------------
(defun array-data-vector (the-array)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-array}}
      {an array object}
\\Valueslabel
 Returns a \\cl\\ object holding the elements of \\funarg{the-array}.
\\Purposelabel
 Returns the object which is used by the lisp system to hold the
 elements contained in \\funarg{the-array}.
 If this object cannot be determined return \\lispnil.
\\Remarkslabel
 \\sysdep{function}

 Please note that \\lw\\ may crash if \\funarg{the-array}\\ is not
 of type array or vector.
\\Exampleslabel
 In \\lw\\ this is an instance of class \\class{vector}\\ or
 \\class{bit-vector}\\ with a length of
 \\lisp{(array-total-size \\funarg{the-array})}:
 \\begin{lispcode}
(setf *a* #2a((1 2)(3 4))) ==> #2A((1 2)(3 4))
(array-data-vector *a*)    ==> #(1 2 3 4)
 \\end{lispcode}
\\Seealsolabel
 Section \\fcite{array ...}."

  (declare (type array the-array))
  #+:allegro
  (multiple-value-bind (data-vector displaced-index-offset displaced-to-p)
      (excl::array-base the-array)
    (declare (ignore displaced-index-offset displaced-to-p))
    data-vector)
  #+:lispworks
  (let ((data-vector (system::find-data-vector the-array)))
    (when (vectorp data-vector)
        data-vector)))

;;; ---------------------------------------------------------------------------
(defun array-displaced-to (displaced-array)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{displaced-array}}
      {an array object}
\\Valueslabel
 Two values are returned:
 \\begin{enumerate}

 \\item The first value being \\nonnil\\ means that
  \\funarg{displaced-array}\\ is displaced to another array,
  \\lispnil\\ means that \\funarg{displaced-array}\\ is not displaced
  to any other array.

 \\item The meaning of the second value depends on the first value:

   \\begin{description}

   \\item[First value is \\lispnil]
    A second value being \\lispnil\\ indicates that it could not be
    determined if \\funarg{displaced-array}\\ is displaced to another
    array.
    A second value being \\lispt\\ means that
    \\funarg{displaced-array}\\ is definitely not displaced to any other
    array.

   \\item[First value is \\nonnil]
    A second value of \\lispnil\\ or \\lispt\\ means that
    \\funarg{displaced-array}\\ is definitely displaced to
    another array but this array cannot be determined (in this
    case it is the responsibility of the caller of this function to
    compute the array anyhow). Any other second
    value is interpreted as the another array to which
    \\funarg{displaced-array}\\ is displaced to.

  \\end{description}

 \\end{enumerate}
\\Purposelabel
 Check if \\funarg{displaced-array}\\ is an array
 which is displaced to another array, i.e.\\ if
 \\funarg{displaced-array}\\ was created like
 \\lisp{(make-array ... :displaced-to \\textit{\\lt{}another array\\gt} \\ldots)}.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{array-displaced-offset};
 \\fcite{array-displaced-to-p};
 section \\fcite{array ...};
 argument \\keyarg{displaced-to}\\ of
 \\fcite{make-array}."

  (declare (type array displaced-array))
  #+:allegro
  (multiple-value-bind (data-vector displaced-index-offset displaced-to-p)
      (excl::array-base displaced-array)
    (declare (ignore data-vector))
    (values displaced-to-p t))
  #+:lispworks4
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement displaced-array)
    (declare (ignore displaced-index-offset))
    (values displaced-to (if displaced-to displaced-to t)))
  #+:lispworks3
  (if (or (system:immediatep displaced-array)
	  ;; In LispWorks, (system::not-displaced-p ...) generates
	  ;; an error for immediate objects:
	  (system::not-displaced-p displaced-array))
      ;; then displaced-array is definitely not displaced to <another array>:
      (values nil t)
    ;; else displaced-array is definitely displaced to <another array>. In
    ;; LispWorks the (system::array-data$array displaced-array) de-references
    ;; <another-array> directly if <another-array> was created by
    ;; (setf <another-array> (make-array ... :adjustable t))
    ;; If <another array> was not created with :adjustable t,
    ;; (system::array-data$array displaced-array) de-references
    ;; (system::array-data$array <another-array>) directly; in this case the
    ;; <another array> cannot be determined here.
    ;; LispWorks note: The function system::array-data$array is a very low-
    ;; low-low-level access function which may generate an error if called
    ;; with a non-array. Since only arrays can be displaced to another array,
    ;; the displaced-array is always of type array here because otherwise
    ;; the test (system::not-displaced-p displaced-array) done above would
    ;; have been failed:
    (let ((data (system::array-data$array displaced-array)))
      (if (adjustable-array-p data)
          (values t data)
        (values t t)))))

;;; ---------------------------------------------------------------------------
(defun array-displaced-to-p (displaced-array &optional displaced-to-array)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{displaced-array}\\ resp.\\ \\funarg{displaced-to-array}}
      {an array object}
\\Valueslabel
 The returned value depends on whether the {\\opt} argument
 \\funarg{displaced-to-array}\\ is given:
 \\begin{description}

 \\item [The \\funarg{displaced-to-array}\\ argument is missing:]
  The returned values are like those of
   \\fcite{array-displaced-to}.

 \\item [The \\funarg{displaced-to-array}\\ argument is given:]

  Two values are returned:
  \\begin{enumerate}

  \\item The first value being \\nonnil\\ means that
   \\funarg{displaced-array}\\ is displaced to
   \\funarg{displaced-to-array},
   \\lispnil\\ means that \\funarg{displaced-array}\\ is not displaced
   to \\funarg{displaced-to-array}.

  \\item The second value \\nonnil\\ means that the first value
   could be computed and is valid,
   \\lispnil\\ means that it could not be decided whether
   \\funarg{displaced-array}\\ is displaced to
   \\funarg{displaced-to-array}; therefore, the first value is invalid.

  \\end{enumerate}
 \\end{description}
\\Purposelabel
 Check if \\funarg{displaced-array}\\ is an array which is displaced
 to \\funarg{displaced-to-array}, i.e.\\ if
 \\funarg{displaced-array}\\ was created with
 \\lisp{(make-array ... :displaced-to \\funarg{displaced-to-array} \\ldots)}.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{array-displaced-offset};
 \\fcite{array-displaced-to};
 section \\fcite{array ...};
 argument \\keyarg{displaced-to}\\ of
 \\fcite{make-array}."

  (declare (type array displaced-array))
  (let ((displaced-p (array-displaced-to displaced-array)))
    ;; If displaced-p is nil, displaced-array is surely not displaced
    ;; to any array:
    (values ;; 1st value:
            (if displaced-p
		(if displaced-to-array
		    (and (not (eq displaced-array displaced-to-array))
                         #+:lispworks4
                         (multiple-value-bind
                             (displaced-to displaced-index-offset)
                             (array-displacement displaced-array)
                           (declare (ignore displaced-index-offset))
                           (eq displaced-to-array displaced-to))
                         #-:lispworks4
		         (eq (array-data-vector displaced-array)
		             (array-data-vector displaced-to-array)))
                  t)
              nil)
            ;; 2nd value:
            t)))

;;; ---------------------------------------------------------------------------
(defun array-displaced-offset (displaced-array)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{displaced-array}}
      {an array object}
\\Purposelabel
 Return the displaced offset for \\funarg{displaced-array}.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{array-displaced-to};
 \\fcite{array-displaced-to-p};
 section \\fcite{array ...};
 argument \\keyarg{displaced-index-offset}\\ of
 \\fcite{make-array}."

  #+:allegro
  (multiple-value-bind (data-vector displaced-index-offset displaced-to-p)
      (excl::array-base displaced-array)
    (declare (ignore data-vector))
    (when displaced-to-p
      displaced-index-offset))
  #+:lispworks4
  (multiple-value-bind (displaced-to displaced-index-offset)
      (array-displacement displaced-array)
    (declare (ignore displaced-to))
    displaced-index-offset)
  #+:lispworks3
  (if (array-displaced-to-p displaced-array)
      (system::array-displacement$array displaced-array)))

;;; ---------------------------------------------------------------------------
(defun t-ivectorp (the-vector)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-vector}}
      {a vector}
\\Purposelabel
 Check if \\funarg{the-vector}\\ is a transient immediate vector,
 i.e.\\ a vector holding only immediate values.
\\Remarkslabel
 \\sysdep{function}

 Hint for portability:
 If this cannot be determined, \\lispnil\\ should be returned;
 this will slow down \\plob\\ for arrays with its element type
 specialized to an immediate type (e.g.\\ arrays created by
 \\lisp{(make-array ... :element-type 'single-float \\ldots)}).

 Hint for \\allegro:
 Looks as if in \\allegro\\ the `immediate vectors' are called
 fixed size vectors (see e.g.\\ the definitions in
 \\lisp{/opt/cl/Franz-ACL-4.3/home/misc/lisp.h}), but for now it
 is unclear how a LISP vector is checked to be such a fixed size
 vector."

  #+:lispworks4
  (system::f-i-vectorp the-vector)
  #+:lispworks3
  (system::i-vectorp the-vector)
  #-:lispworks
  (and (not (eq (array-element-type the-vector) t))
       (subtypep (type-of the-vector) 'vector)))

;;; ---------------------------------------------------------------------------
#+(and :lisp-doc (not :document-api))
(:defdoc
 "structure ..."
 "Structure Information Functions"
 "
 Similar to arrays (see section \\fcite{array ...}) there are {\\sl no}
 well-defined information functions at all about types resp.\\ classes
 created by \\lisp{defstruct}\\ statements; so I well-define the information
 functions about \\lisp{defstruct}\\/s I found in \\lw\\ as
 well-defined information functions, in particular these are the functions
 {\\bf class-constructor}, {\\bf (setf class-constructor)},
 method {\\bf class-slots (structure-class)}, the functions
 {\\bf structure-slot-reader},
 {\\bf structure-slot-default-init} and {\\bf structure-slot-type}.

 Because the \\cl\\ standard defines that for each
 \\lisp{defstruct}\\ there has to be a corresponding instance of
 \\class{structure-class}, this \\lisp{defstruct}\\ class representations
 have to be passed as arguments. Since the \\cl\\ LISP standard defines
 no classes for \\lisp{defstruct}\\ slots, they are passed by their
 symbolic names. Most of the \\lisp{defstruct}\\ options and
 \\lisp{defstruct}\\ slot options are not considered since they do not
 seem to be very useful to me. The only slot options handled are the
 slot's default initialization and the slot's type.

\\Remarkslabel
 \\note\\ In \\lw, certain \\lisp{defstruct}\\ options affect
 directly the \\lisp{defstruct}-specific functions which are
 created at evaluating the \\lisp{defstruct}, e.g.\\ the
 \\lisp{defstruct}\\ \\lisp{:type}\\ option
 \\cite[\\citepage{481}]{bib:CLtLII} leads to the generation of a totally
 different constructor function compared to the same
 \\lisp{defstruct}\\ created with no \\lisp{:type}\\ option.
 Furthermore, these options are not
 explicitly represented in \\lw\\ \\lisp{defstruct}\\ descriptions.

 Hint for non-\\lw: If you are adapting these functions to a
 non-\\lwcl\\ and you cannot locate the sub-functions used in the
 \\lw\\ version, try to generate a very new structure instance,
 \\lisp{format}\\ it into a string, and parse that string for the
 slot names and the slot default initializations (very onerous,
 but this should work at least).
\\Seealsolabel
 \\Fcite{class-constructor};
 \\fcite{(setf class-constructor)};
 \\fcite{class-slots (structure-class)};
 \\fcite{structure-slot-reader};
 \\fcite{structure-slot-default-init};
 \\fcite{structure-slot-type}.")

;;; ---------------------------------------------------------------------------
(defgeneric class-constructor (the-class)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isastrcls{\\funarg{the-class}}
\\Purposelabel
 Get the constructor function symbol of \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{(setf class-constructor)};
 section \\fcite{structure ...};
 argument \\keyarg{constructor}\\ of \\fcite{defstruct}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-constructor) (new-constructor the-class)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{new-constructor}}
      {a symbol bound to a function which generates an instance of
       class \\funarg{the-class}}
 \\isastrcls{\\funarg{the-class}}
\\Valueslabel
 \\retarg{\\funarg{new-constructor}}
\\Purposelabel
 Set the constructor function of \\funarg{the-class}\\ to
 \\funarg{new-constructor}. The function bound to
 \\funarg{new-constructor}\\ will be called each time \\plob\\ loads
 an instance of \\funarg{the-class}\\ from the persistent
 heap into the transient \\cl\\ system and must therefore create
 a transient instance of \\funarg{the-class}. For each slot of
 \\funarg{the-class}, a {\\key{}} argument with the keyword being
 the slot name interned to package \\lisp{:keyword}\\ is passed.

 This function {\\bf (setf class-constructor)} is meant to be called
 from top-level to declare the
 constructor function of predefined structure classes found in the
 \\cl\\ system packages which should be handled by \\plob\\ and
 whose instances cannot be created by the default constructor function; 
 e.g.\\ some structure classes need  a very special handling for
 creating their instances (like mandatory initialization arguments,
 initialization arguments which depend of each other etc.).
\\Exampleslabel
 The function {\\bf make-logical-pathname-by-plob} should be called by
 \\plob\\ to create transient instances of class
 \\class{logical-pathname}\\ instead of the default constructor function
 {\\bf make-logical-pathname}\\ provided by the transient \\cl\\ system at
 evaluating the \\lisp{defstruct}\\ statement:
 \\begin{lispcode}
(defun make-logical-pathname-by-plob
     (&key host directory name type version)
  (system::make-logical-pathname-from-components
   host directory name type version))
\underline{(setf (class-constructor (find-class 'logical-pathname))
      'make-logical-pathname-by-plob)}
 \\end{lispcode}
\\Seealsolabel
 \\Fcite{class-constructor};
 section \\fcite{structure ...};
 argument \\keyarg{constructor}\\ of \\fcite{defstruct}."))

;;; ---------------------------------------------------------------------------
(defun assert-structure-class (class-of-structure)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{class-of-structure}}
\\Purposelabel
 Assert that \\funarg{class-of-structure}\\ is the \\clsmo\\ of a
 \\lisp{defstruct}\\ class; raise an error otherwise."

  (assert (eq (class-of class-of-structure) +structure-class-class+)
      ()
      "~A is no structure class" class-of-structure))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defconstant +lispworks-structure-definition-index+ 13
  #+:lisp-doc "
\\Purposelabel
 The index into the vector returned by the \\lw\\ function
 {\\bf structure::get-structure-definition} which contains the
 structure definition.
\\Remarkslabel
 \\sysdep{constant}")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defconstant +lispworks-structure-definition-location-constructor+ 13
  #+:lisp-doc "
\\Purposelabel
 The index into the \\lw\\ structure definition which contains the
 constructor function symbol of the structure.
\\Remarkslabel
 \\sysdep{constant}")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun structure-constructor-internal (the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isastrcls{\\funarg{the-class}}
\\Purposelabel
 Returns the symbol bound to the constructor function of
 \\funarg{the-class}.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 \\Fcite{class-constructor};
 section \\fcite{structure ...};
 argument \\keyarg{constructor}\\ of \\fcite{defstruct}."

  (let* ((definition
          #+:lispworks4 ;; or later
          (low:coerce-record-to-vector (clos::class-wrapper the-class))
          #+:lispworks3
	  (svref (structure::get-structure-definition the-class)
		 +lispworks-structure-definition-index+))
         (single-constructor
          (svref definition
                 +lispworks-structure-definition-location-constructor+)))
    (if single-constructor
        single-constructor
      (caar (svref definition
	           (1+ +lispworks-structure-definition-location-constructor+))))))
      

;;; ---------------------------------------------------------------------------
(defvar *class->constructor-table* (make-hash-table :test #'eq)
  #+:lisp-doc "
\\Purposelabel
 A variable for mapping structure classes to constructor
 symbols.
\\Seealsolabel
 \\Fcite{(setf class-constructor)};
 \\fcite{class-constructor}.")

;;; ---------------------------------------------------------------------------
(defmethod (setf class-constructor) (constructor the-class)
  (setf (gethash the-class *class->constructor-table*)
        constructor))

;;; ---------------------------------------------------------------------------
(defmethod class-constructor (the-class)
  (multiple-value-bind (constructor found)
      (gethash the-class *class->constructor-table*)
    (cond
     (found
      constructor)
     ((next-method-p)
      (call-next-method)))))

;;; ---------------------------------------------------------------------------
(defmethod class-constructor ((the-class structure-class))
  #+:lisp-doc "
 Return the constructor symbol either as the symbol stored by a top-level
 call to \\fcite{(setf class-constructor)}\\ or, if this call returned
 \\lispnil, try to find the system-generated constructor symbol
 generated from the \\lisp{defstruct}\\ statement.
\\Remarkslabel
 \\sysdep{method}"

  (let ((constructor (call-next-method)))
    (unless constructor
      #+(and :allegro (not (version>= 5)))
      (let ((defstruct-description
		(get (class-name the-class) 'excl::%structure-definition)))
	(setf constructor
	  (slot-value defstruct-description 'excl::constructor))
	(unless constructor
	  ;; Take the first BOA constructor found:
	  (setf constructor
	    (caar
	     (slot-value defstruct-description 'excl::boa-constructors)))))
      #+(and :allegro (version>= 5))
      (let ((defstruct-description
		(get (class-name the-class) 'excl::%structure-definition)))
	(setf constructor
	  (svref defstruct-description 5))
	(unless constructor
	  ;; Take the first BOA constructor found:
	  (setf constructor
	    (caar
	     (svref defstruct-description 6)))))
      #+:lispworks
      (setf constructor (structure-constructor-internal the-class)))
    constructor))

;;; ---------------------------------------------------------------------------
(defun structure-slot-reader (class-of-structure slot-name-symbol)
  #+:lisp-doc "
\\Argumentslabel
 \\isastrcls{\\funarg{class-of-structure}}
 \\isa{\\funarg{slot-name-symbol}}
      {a symbol naming a slot of \\funarg{class-of-structure}}
\\Purposelabel
 Return the symbol bound to the slot reading function for
 \\funarg{slot-name-symbol}\\ in \\funarg{class-of-structure}.
\\Remarkslabel
 \\sysdep{function}

 Hint for \\allegro:
 For now, this function creates a symbolic name for the
 structure slot reader
 function regardless of a specified \\lisp{:conc-name}. A better
 idea would be to look if the
 structure slot reader
 is represented somewhere associated with the structure's \\clsmo.
\\Seealsolabel
 Section \\fcite{structure ...};
 structure slot readers as described for the
 \\fcite{defstruct};
 argument \\keyarg{conc-name}\\ of
 \\fcite{defstruct}."

  (assert-structure-class class-of-structure)
  #+(and :allegro (not (version>= 5)))
  (progn
    (unless (symbolp slot-name-symbol)
      (setf slot-name-symbol (slot-definition-name slot-name-symbol)))
    (let* ((slots (slot-value (get (class-name class-of-structure)
				   'excl::%structure-definition)
			      'excl::slots))
	   (slot (find slot-name-symbol
		       slots
		       :key #'(lambda (s) (slot-value s 'excl::name))))
	   (slot-reader (when slot (slot-value slot 'excl::accessor))))
      slot-reader))
  #+(and :allegro (version>= 5))
  (progn
    (unless (symbolp slot-name-symbol)
      (setf slot-name-symbol (slot-definition-name slot-name-symbol)))
    (when (find slot-name-symbol
		(class-slots class-of-structure)
		:key #'(lambda (s) (slot-value s
					       #-(version>= 6)
					       'clos::name
					       #+(version>= 6)
					       'excl::name)))
      (let* ((defstruct-description (get (class-name class-of-structure)
					 'excl::%structure-definition))
	     (conc-name (svref defstruct-description 4)))
	(intern (concatenate 'string (symbol-name conc-name)
			     (symbol-name slot-name-symbol))
		(symbol-package conc-name)))))
  #+:lispworks
  (multiple-value-bind (slot-p slot-type slot-reader slot-default-init)
      (structure:structure-slot-details class-of-structure slot-name-symbol)
    (declare (ignore slot-p slot-type slot-default-init))
    slot-reader))

;;; ---------------------------------------------------------------------------
(defun structure-slot-default-init (class-of-structure slot-name-symbol)
  #+:lisp-doc "
\\Argumentslabel
 \\isastrcls{\\funarg{class-of-structure}}
 \\isa{\\funarg{slot-name-symbol}}
      {a symbol naming a slot of \\funarg{class-of-structure}}
\\Purposelabel
 Return the slot default initialization value for
 \\funarg{slot-name-symbol}\\ in \\funarg{class-of-structure}.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 Section \\fcite{structure ...};
 structure slot default initialization as described for the
 \\fcite{defstruct}."

  (assert-structure-class class-of-structure)
  #+:allegro
  (let ((slot (if (symbolp slot-name-symbol)
		  (find slot-name-symbol
			(class-slots class-of-structure)
			:key #'slot-definition-name)
		slot-name-symbol)))
    (slot-definition-initform slot))
  #+:lispworks
  (multiple-value-bind (slot-p slot-type slot-reader slot-default-init)
      (structure:structure-slot-details class-of-structure slot-name-symbol)
    (declare (ignore slot-p slot-type slot-reader))
    slot-default-init))

;;; ---------------------------------------------------------------------------
#+:allegro
(defmethod slot-definition-initargs
    ((slot
      #-(version>= 6)
      clos::structure-effective-slot-definition
      #+(version>= 6)
      excl::structure-effective-slot-definition))
  #+:lisp-doc "
  \\Remarkslabel
 \\sysdep{method}

 Hint for \\allegro:
 For now, this function creates a list with a symbolic name for the
 structure slot definition initarg.
 A better idea would be to look if the
 structure slot definition initarg
 is represented somewhere associated with the structure's
 \\clsmo\\ (but it seems as if \\allegro\\ doesn't do this)."

  (let ((initiargs (list (intern (slot-definition-name slot)
				 :keyword))))
    initiargs))

;;; ---------------------------------------------------------------------------
#+:allegro
(defmethod slot-definition-initfunction
    ((slot 
      #-(version>= 6)
      clos::structure-effective-slot-definition
      #+(version>= 6)
      excl::structure-effective-slot-definition))
  #+:lisp-doc "
  \\Remarkslabel
 \\sysdep{method}"

  (let ((initform (slot-definition-initform slot)))
    initform))

;;; ---------------------------------------------------------------------------
#+:allegro
(defmethod slot-definition-allocation
    ((slot 
      #-(version>= 6)
      clos::structure-effective-slot-definition
      #+(version>= 6)
      excl::structure-effective-slot-definition))
  #+:lisp-doc "
  \\Remarkslabel
 \\sysdep{method}"

  :instance)

;;; ---------------------------------------------------------------------------
(defun structure-slot-type (class-of-structure slot-name-symbol)
  #+:lisp-doc "
\\Argumentslabel
 \\isastrcls{\\funarg{class-of-structure}}
 \\isa{\\funarg{slot-name-symbol}}
      {a symbol naming a slot of \\funarg{class-of-structure}}
\\Purposelabel
 Return the slot type for
 \\funarg{slot-name-symbol}\\ in \\funarg{class-of-structure}.
 This is the value of the structure slot option
 \\lisp{:type}\\ specified in the \\lisp{defstruct}\\ statement of
 \\funarg{class-of-structure}\\ for the structure slot named
 \\funarg{slot-name-symbol};
 if no such option was specifed, type \\lispt\\ is assumed.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 Section \\fcite{structure ...};
 structure slot options as described for the
 \\fcite{defstruct}."

  (assert-structure-class class-of-structure)
  #+:allegro
  (let ((slot (if (symbolp slot-name-symbol)
		  (find slot-name-symbol
			(class-slots class-of-structure)
			:key #'slot-definition-name)
		slot-name-symbol)))
    (slot-definition-type slot))
  #+:lispworks
  (multiple-value-bind (slot-p slot-type slot-reader slot-default-init)
      (structure:structure-slot-details class-of-structure slot-name-symbol)
    (declare (ignore slot-p slot-reader slot-default-init))
    slot-type))

;;; ---------------------------------------------------------------------------
;;; Function code type
;;; ---------------------------------------------------------------------------
(defconstant +lisp-code-type+
    #+:lispworks :wfasl
    #+:allegro   :fasl
  #+:lisp-doc "
\\Purposelabel
 Contains the kind of compiled \\cl\\ code for this \\cl\\ system.
\\Remarkslabel
 \\sysdep{constant}

 Hint for portability:
 For each code kind an own keyword symbol should be defined;
 The value of \\lisp{+lisp-code-type+}\\ reflects the kind of
 code this \\cl\\ compiler generates.
\\Exampleslabel
 For \\lwcl\\ running on SUN SPARCs,
 the code kind is \\lisp{:wfasl}.")

;;; ---------------------------------------------------------------------------
;;; Support for built-in TLatters
;;; ---------------------------------------------------------------------------

#+:lispworks3
(defmacro tlatter-get-key (tlatter)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{tlatter}}
      {an instance of class \\class{tlatter}}
\\Purposelabel
 Return the {\\sl key} field of \\funarg{tlatter}.
\\Remarkslabel
 \\sysdep{macro}
\\Seealsolabel
 Section \\fcite{tlatter ...}."

  `(system::tlatter-key ,tlatter))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defmacro tlatter-set-key (tlatter new-key)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{tlatter}}
       {an instance of class \\class{tlatter}}
 \\isanobject{\\funarg{new-key}}
\\Purposelabel
 Set the {\\sl key} field of \\funarg{tlatter}\\ to \\funarg{new-key}.
\\Remarkslabel
 \\sysdep{macro}
\\Seealsolabel
 Section \\fcite{tlatter ...}."

  `(system::set-tlatter-key ,tlatter ,new-key))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defmacro tlatter-get-data (tlatter)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{tlatter}}
       {an instance of class \\class{tlatter}}
\\Purposelabel
 Return the {\\sl data} field of \\funarg{tlatter}.
\\Remarkslabel
 \\sysdep{macro}
\\Seealsolabel
 Section \\fcite{tlatter ...}."

  `(system::tlatter-value ,tlatter))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defmacro tlatter-set-data (tlatter new-data)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{tlatter}}
       {an instance of class \\class{tlatter}}
 \\isanobject{\\funarg{new-data}}
\\Purposelabel
 Set the {\\sl data} field of \\funarg{tlatter}\\ to \\funarg{new-data}.
\\Remarkslabel
 \\sysdep{macro}
\\Seealsolabel

 Section \\fcite{tlatter ...}."

  `(system::set-tlatter-value ,tlatter ,new-data))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defmacro tlatter-get-next (tlatter)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{tlatter}}
       {an instance of class \\class{tlatter}}
\\Purposelabel
 Return the {\\sl next} field of \\funarg{tlatter}.
\\Remarkslabel
 \\sysdep{macro}
\\Seealsolabel
 Section \\fcite{tlatter ...}."

  `(system::tlatter-next ,tlatter))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defmacro tlatter-set-next (tlatter new-next)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{tlatter}\\ resp.\\ \\funarg{new-next}}
       {an instance of class \\class{tlatter}}
\\Purposelabel
 Set the {\\sl next} field of \\funarg{tlatter}\\ to \\funarg{new-next}.
\\Remarkslabel
 \\sysdep{macro}
\\Seealsolabel
 Section \\fcite{tlatter ...}."

  `(system::set-tlatter-next ,tlatter ,new-next))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defmacro make-tlatter (key data next)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{key}}
 \\isanobject{\\funarg{data}}
 \\isa{\\funarg{next}}
       {an instance of class \\class{tlatter}}
\\Purposelabel
 Returns a new allocated and initialized instance of class
 \\class{tlatter}.
\\Remarkslabel
 \\sysdep{macro}
\\Seealsolabel
 Section \\fcite{tlatter ...}."

  `(system::alloc-tlatter ,key ,data ,next))

;;; ---------------------------------------------------------------------------
#+:lispworks3
(defun tlatter-p (tlatter)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{tlatter}}
\\Purposelabel
 Check if \\funarg{tlatter}\\ is an instance of class \\class{tlatter}.
\\Remarkslabel
 \\sysdep{macro}

 Hint for \\allegro:
 It is unknown if \\allegro\\ has something similar to a TLatter.
\\Seealsolabel
 Section \\fcite{tlatter ...}."

  #+:lispworks3
  (system::tlatterp tlatter)
  #-:lispworks3
  nil)

;;; ---------------------------------------------------------------------------
;;; Processes
;;; ---------------------------------------------------------------------------
#+(and :lisp-doc (not :document-api))
(:defdoc
 "process ..."
 "Process Functions"
 "
 Although there is no standard defined on \\cl\\ processes, many
 \\cl\\ systems (including \\lw) implement the
 \\symbolics\\ multi-processing interface. The \\plob\\ functions
 {\\bf is-on-idle-sleep-hook}, {\\bf make-process-variable},
 {\\bf process-alive-p}, {\\bf process-pid},
 {\\bf process-wait-with-timeout}, {\\bf push-to-idle-sleep-hook} and
 {\\bf remove-from-idle-sleep-hook} are implemented using that
 \\symbolics\\ pseudo-standard multi-processing interface.
\\Remarkslabel
 A lisp-internal process is also called a `session' in this document to
 distinguish it from the \\cl\\ process in the sense of \\unix.
\\Seealsolabel
 \\Fcite{ensure-process-cleanup};
 \\fcite{is-on-idle-sleep-hook};
 \\fcite{make-process-variable};
 \\fcite{process-alive-p};
 \\fcite{process-pid};
 \\fcite{process-wait-with-timeout};
 \\fcite{push-to-idle-sleep-hook};
 \\fcite{remove-from-idle-sleep-hook}.")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun ensure-process-cleanup (cleanup-form
                               &optional (process (process-pid)))
  #+:lisp-doc "
\\Purposelabel
 Ensures that \\funarg{cleanup-form}\\ is executed when the
 \\funarg{process}\\ terminates.
\\Seealsolabel
 \\Fcite{ensure-process-cleanup}"
  (mp:ensure-process-cleanup cleanup-form process))

;;; ---------------------------------------------------------------------------
(defun process-wait-with-timeout
     (reason timeout &optional end-wait-predicate
             &rest end-wait-predicate-args)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{reason}}
      {a string}
 \\isa{\\funarg{timeout}}
      {a fixnum}
 \\isa{\\funarg{end-wait-predicate}}
      {a function taking
       \\lisp{(length \\funarg{end-wait-predicate-args})}\\ arguments}
 \\isa{\\funarg{end-wait-predicate-arg}}
      {an argument list for \\funarg{end-wait-predicate}}
\\Purposelabel
 Wait until \\funarg{timeout}\\ seconds have been passed or
 \\funarg{end-wait-predicate}\\ applied
 to \\funarg{end-wait-predicate-args}\\ becomes \\lispt.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 Section \\fcite{process ...};
 \\lw\\ function {\\bf mp::process-wait-with-timeout}."

  (apply #'mp::process-wait-with-timeout
         `(,reason ,timeout ,end-wait-predicate
                   ,@end-wait-predicate-args)))

;;; ---------------------------------------------------------------------------
(defun process-pid ()
  #+:lisp-doc "
\\Purposelabel
 Return the lisp-internal PID ({\\sl P}\\/rocess {\\sl ID}\\/entifier)
 of the current \\cl\\ session.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 Section \\fcite{process ...};
 \\fcite{process-name};
 \\lw\\ variable {\\bf mp:*current-process*}."

  (if mp:*current-process*
      mp:*current-process*
    t))

;;; ---------------------------------------------------------------------------
(defun process-name (&optional (pid (process-pid)))
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{pid}}
      {a process object}
\\Purposelabel
 Return the lisp-internal process name of the \\cl\\ process
 identified by \\funarg{pid}.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 Section \\fcite{process ...};
 \\fcite{process-pid};
 \\lw\\ structure slot accessor {\\bf mp:process-name}."

  (when (and pid (not (eq pid t)))
      (mp:process-name pid)))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defun process-alive-p (the-process)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{the-process}}
      {a process object}
\\Purposelabel
 Test if the lisp-internal session represented by
 \\funarg{the-process}\\ is alive.
\\Remarkslabel
 \\sysdep{function}
\\Seealsolabel
 Section \\fcite{process ...}."

  (or (eq the-process t)
      (not (eq (mp::process-name the-process) :dead-process))))

;;; ---------------------------------------------------------------------------
(defun make-process-variable (variable
			      &optional (initform nil initformp)
					on-end)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{variable}}
      {a symbol}
 \\isa{\\funarg{initform}}
      {either a symbol optional bound to a function
       or a list with its \\lisp{car}\\ being a symbol bound to a function
       and the \\lisp{cdr}\\ being an argument list for that function}
\\Purposelabel
 If the \\funarg{initform}\\ argument is missing,
 remove the \\funarg{variable}\\ from the list of lisp-internal
 process-local (i.e.\\ session-local) variables for the each new
 created process.

 If the \\funarg{initform}\\ argument is given,
 makes the \\funarg{variable}\\ a lisp-internal process-local
 (i.e.\\ session-local) variable for the each new created process.

 If the first form of \\funarg{initform}\\ is passed,
 each new \\funarg{variable}\\ is either initialized to
 \\funarg{initform}\\ if \\funarg{initform}\\ is not bound to a
 function or it is initialized to the evaluation of
 \\funarg{initform}\\ with no arguments if \\funarg{initform}\\ is
 bound to a function.

 If the second form of \\funarg{initform}\\ is passed,
 each new \\funarg{variable}\\ is initialized to the
 evaluation of \\funarg{initform}.
\\Remarkslabel
 \\sysdep{function}

 Hint for \\allegro:
 The multi-processing interface of \\allegro\\ is almost the same
 as the one of \\lw; the variable called 
 \\textbf{mp:*process-initial-bindings*} in \\lw is called
 \\textbf{excl:*cl-default-special-bindings*} in \\allegro.
\\Exampleslabel
 This call makes \\lisp{*default-persistent-heap*}\\ a lisp-internal
 process-local variable initialized to the evaluation of
 \\lisp{(make-persistent-heap)}\\ each time \\lw\\ creates a new
 lisp-internal process, i.e.\\ every lisp-internal process has
 `an own' value for \\lisp{*default-persistent-heap*}:
 \\begin{lispcode}
(make-process-variable '*default-persistent-heap*
                       '(make-persistent-heap))
 \\end{lispcode}
\\Seealsolabel
 Section \\fcite{process ...};
 \\lw\\ variable \\textbf{mp:*process-initial-bindings*};
 \\allegro\\ variable \\textbf{excl:*cl-default-special-bindings*}."

  (let ((found (assoc variable
		      #+:allegro excl:*cl-default-special-bindings*
		      #+:lispworks mp:*process-initial-bindings*)))
    (cond
     (found
      (if initformp
	  (rplacd found initform)
        (setf #+:allegro excl:*cl-default-special-bindings*
	      #+:lispworks mp:*process-initial-bindings*
              (delete found
		      #+:allegro excl:*cl-default-special-bindings*
		      #+:lispworks mp:*process-initial-bindings*))))
     (initformp
      (if on-end
	  (push-on-end `(,variable ,@initform)
		       #+:allegro excl:*cl-default-special-bindings*
		       #+:lispworks mp:*process-initial-bindings*)
	(push `(,variable ,@initform)
	      #+:allegro excl:*cl-default-special-bindings*
	      #+:lispworks mp:*process-initial-bindings*)))))
  t)

;;; ---------------------------------------------------------------------------
;;; External and internal symbol and package names:
;;; ---------------------------------------------------------------------------
(defun external-to-internal-name (external-name)
  #+:lisp-doc "
\\Purposelabel
 With \\allegrocl\\ version 5.0, a case sensitive mode has been introduced
 for reading symbol and package names. Since \\plob\\ uses uppercase for
 all names of persistent symbols and packages, a conversion is necessary
 between \\emph{external} (that means, a name in the persistent namespace)
 and \\emph{internal} (that means, a name in the transient namespace) names.
\\Seealsolabel
 \\Fcite{internal-to-external-name}"
  #+:allegro
  (if (eq excl:*current-case-mode* :case-sensitive-lower)
      (string-downcase external-name)
    external-name)
  #-:allegro
  external-name)

(defun internal-to-external-name (internal-name)
  #+:lisp-doc "
\\Purposelabel
 See comment at \\fcite{external-to-internal-name}.
\\Seealsolabel
 \\Fcite{external-to-internal-name}"
  #+:allegro
  (if (eq excl:*current-case-mode* :case-sensitive-lower)
      (string-upcase internal-name)
    internal-name)
  #-:allegro
  internal-name)

;;; ---------------------------------------------------------------------------
;;; Extents of system-dependent structures:
;;; ---------------------------------------------------------------------------

#+:allegro
(setf (package-extent (find-package :inspect)) :transient)
#+:allegro
(setf (package-extent (find-package :excl)) :transient)

#+:lispworks
(setf (package-extent (find-package :clue)) :transient)
#+:lispworks
(setf (package-extent (find-package :compiler)) :transient)
#+:lispworks
(setf (package-extent (find-package :mp)) :transient)
#+:lispworks
(setf (package-extent (find-package :system)) :transient)
#+:lispworks
(setf (package-extent (find-package :toolkit)) :transient)
#+:lispworks
(setf (package-extent (find-package :xlib)) :transient)
#+:lispworks
(setf (class-extent (find-class 'pathname)) :cached)
#+:lispworks
(setf (class-constructor (find-class 'pathname)) 'make-pathname)

;;; ---------------------------------------------------------------------------
;;; Some informations to PLOB about LispWorks logical pathnames:
;;; ---------------------------------------------------------------------------

#+(or :lispworks :lisp-doc)
(defun make-logical-pathname-by-plob (&key host directory name type version)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\keyarg{host}\\ %
       resp.\\ \\keyarg{directory}\\ %
       resp.\\ \\keyarg{name}\\ %
       resp.\\ \\keyarg{type}\\ %
       resp.\\ \\keyarg{version}}
      {a string}
\\Purposelabel
 Create a \\lw\\ logical pathname which is loaded by \\plob.
\\Seealsolabel
 \\Fcite{(setf class-constructor)}."

  (system::make-logical-pathname-from-components
   host directory name type version))

#+:lispworks
(setf (class-constructor (find-class 'logical-pathname))
      'make-logical-pathname-by-plob)

#+:lispworks
(setf (class-extent (find-class 'logical-pathname)) :cached)

#+:lispworks
(setf (slot-extent 'system::device (find-class 'logical-pathname))
      :transient)

;;; ---------------------------------------------------------------------------
;;; Some informations to PLOB about LispWorks standard-class
;;; ---------------------------------------------------------------------------
(setf (class-extent (find-class 'class)) :transient)
(setf (class-extent (find-class 'standard-class)) :transient)

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
