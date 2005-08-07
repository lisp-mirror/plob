;;;; -*- Package: USER; Mode: LISP; Syntax: ANSI-Common-Lisp -*-------------
;;;; Module	plob-doc.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	27.4.94
;;;; Description	PLOB - Persistent Lisp OBjects
;;;;                           =          =    ==
;;;;		Extract doc strings from sources & prepare the extracted
;;;;		documentation for TeX
;;;; --------------------------------------------------------------------------

(in-package "USER")

;;; ---------------------------------------------------------------------------
;;; Sample documented function
;;; ---------------------------------------------------------------------------
#+:never
(defun sample ()
  "
\\Argumentslabel
 \\isa{\\funarg{}}
      {}
 \\isa{\\funarg{}}
      {}
 \\isabool{\\funarg{}}
 \\isacls{\\funarg{}}
 \\isanobject{\\funarg{}}
 \\isanobjid{\\funarg{}}

\\Valueslabel
 \\retarg{\\funarg{}}
\\Purposelabel

\\Remarkslabel

\\Exampleslabel

\\Seealsolabel
 \\Fcite{};
 \\fcite{}."
  )

;;; ---------------------------------------------------------------------------
;;; Sample 'free' item
;;; ---------------------------------------------------------------------------
#+:lisp-doc
(:defdoc
 "free item ..."	; This could be also a symbol
 "Document Items with `free' Text"
 "
This is a document item not bound to a programming construct:
\\begin{lispcode}
#+lisp-doc
(:defdoc
 \"free item ...\"      ; This could be also a symbol
 \"Document Items with `free' Text\"
 \"This is a document item not bound to a programming construct:
  ...\")
\\end{lispcode}

 It is only `seen' by the LISP reader when \\lisp{:lisp-doc}\\ is on the
 \\lisp{*features*}\\ list; this is accomplished by
 \\lisp{lisp-doc}\\ during scanning a LISP file.

 This item is sorted accroding to its first sort-key `parameter' behind
 the \\lisp{:defdoc}\\ statement.

 You can also use \\verb|\\fcite{}| on these items by using the sort key
 as argument, e.g.\\ \\verb|\\fcite{free item ...}| gives
 `\\fcite{free item ...}'.")

;;; ---------------------------------------------------------------------------
(defconstant +default-syntax-label+ "\\Syntaxlabel"
  "The default \\TeX\\ command string for generating the
 {\\sc Syntax} label.")

;;; ---------------------------------------------------------------------------
(defconstant +default-syntax-postfix+ ""
  "The default postfix to append after a syntax text.")

;;; ---------------------------------------------------------------------------
(defconstant +internal-label+ "Internal"
  "The string used as {\\it Internal} label in a document item header.")

;;; ---------------------------------------------------------------------------
(defconstant +exported-label+ "External"
  "The string used as {\\it External} label in a document item header.")

;;; ---------------------------------------------------------------------------
(defclass lisp-comment ()
  (
   (com-midfix
    :initarg :midfix
    :initform nil
    :accessor comment-midfix
    :documentation "
 The text to put into the \\verb|\\begin..com| command.")

   (key
    :initarg :key
    :initform nil
    :accessor comment-key
    :documentation "
 The Keyword of the comment.
 The documentation is sorted according to this keyword.")

   (key-attribute
    :initarg :key-attribute
    :initform nil
    :accessor comment-key-attribute
    :documentation "
 The text attribute for key; normally, this contains either the
 {\\it Internal} or {\\it Exported} label.")

   (syntax-label
    :initarg :syntax-label
    :initform +default-syntax-label+
    :accessor comment-syntax-label
    :documentation "
 The \\TeX\\ command string to generate the {\\sc Syntax} label for
 the document item.")

   (syntax
    :initarg :syntax
    :initform nil
    :accessor comment-syntax
    :documentation "
 The syntax description for the documented item.")

   (text
    :initarg :text
    :initform nil
    :accessor comment-text
    :documentation "
 Contains a textual description of key. This is normally the literal
 read documentation string.")
   )
  (:documentation "
\\Purposelabel
 A class for representing comments. For each documented item,
 an instance of {\\bf lisp-comment} is created."))

;;; ---------------------------------------------------------------------------
(defconstant +default-documentation-midfix+ "dc"
  "The text to put into the \\verb|\\begin..com| command for
 documented items.")

;;; ---------------------------------------------------------------------------
(defclass comment-documentation (lisp-comment)
  (
   (com-midfix
    :initarg :midfix
    :initform +default-documentation-midfix+
    :accessor comment-midfix)
   )
  (:documentation "
\\Purposelabel
 A class for documenting-only items.
\\Seealsolabel
 \\Fcite{lisp-comment}."))

;;; ---------------------------------------------------------------------------
(defconstant +default-constant-midfix+ "cn"
  "The text to put into the \\verb|\\begin..com| command for
 constants.")

;;; ---------------------------------------------------------------------------
(defclass comment-constant (lisp-comment)
  (
   (com-midfix
    :initarg :midfix
    :initform +default-constant-midfix+
    :accessor comment-midfix)
   )
  (:documentation "
\\Purposelabel
 A class for documenting constants.
\\Seealsolabel
 \\Fcite{lisp-comment}."))

;;; ---------------------------------------------------------------------------
(defconstant +default-parameter-midfix+ "pm"
  "The text to put into the \\verb|\\begin..com| command for
 parameters.")

;;; ---------------------------------------------------------------------------
(defclass comment-parameter (lisp-comment)
  (
   (com-midfix
    :initarg :midfix
    :initform +default-parameter-midfix+
    :accessor comment-midfix)
   )
  (:documentation "
\\Purposelabel
 A class for documenting parameters.
\\Seealsolabel
 \\Fcite{lisp-comment}."))

;;; ---------------------------------------------------------------------------
(defconstant +default-variable-midfix+ "vr"
  "The text to put into the \\verb|\\begin..com| command for
 variables.")

;;; ---------------------------------------------------------------------------
(defclass comment-variable (lisp-comment)
  (
   (com-midfix
    :initarg :midfix
    :initform +default-variable-midfix+
    :accessor comment-midfix)
   )
  (:documentation "
\\Purposelabel
 A class for documenting variables.
\\Seealsolabel
 \\Fcite{lisp-comment}."))

;;; ---------------------------------------------------------------------------
(defconstant +default-macro-midfix+ "mc"
  "The text to put into the \\verb|\\begin..com| command for
 macros.")

;;; ---------------------------------------------------------------------------
(defclass comment-macro (lisp-comment)
  (
   (com-midfix
    :initarg :midfix
    :initform +default-macro-midfix+
    :accessor comment-midfix)
   )
  (:documentation "
\\Purposelabel
 A class for documenting macros.
\\Seealsolabel
 \\Fcite{lisp-comment}."))

;;; ---------------------------------------------------------------------------
(defconstant +default-class-midfix+ "cl"
  "The text to put into the \\verb|\\begin..com| command for
 classes.")

;;; ---------------------------------------------------------------------------
(defconstant +default-superclasses-label+ "\\Directsuperclasseslabel"
  "The default \\TeX\\ command string for generating the
 {\\sc Direct Superclasses} label.")

;;; ---------------------------------------------------------------------------
(defconstant +default-metaclass-label+ "\\Metaclasslabel"
  "The default \\TeX\\ command string for generating the
 {\\sc Metaobject Class} label.")

;;; ---------------------------------------------------------------------------
(defconstant +default-slots-label+ "\\Directslotslabel"
  "The default \\TeX\\ command string for generating the
 {\\sc Direct Slots} label.")

;;; ---------------------------------------------------------------------------
(defconstant +default-class-options-label+ "\\Classoptionslabel"
  "The default \\TeX\\ command string for generating the
 {\\sc Class Options} label.")

;;; ---------------------------------------------------------------------------
(defclass comment-class (lisp-comment)
  (
   (com-midfix
    :initarg :midfix
    :initform +default-class-midfix+
    :accessor comment-midfix)

   (superclasses-label
    :initarg :superclasses-label
    :initform +default-superclasses-label+
    :accessor comment-superclasses-label
    :documentation "Contains the \\TeX\\ label to use for slot
 {\\bf superclasses}.")
   (superclasses
    :initarg :superclasses
    :initform nil
    :accessor comment-superclasses
    :documentation "Contains a list of the direct superclasses of
 the documented class.")

   (metaclass-label
    :initarg :metaclass-label
    :initform +default-metaclass-label+
    :accessor comment-metaclass-label
    :documentation "Contains the \\TeX\\ label to use for slot
 {\\bf metaclass}.")
   (metaclass
    :initarg :metaclass
    :initform nil
    :accessor comment-metaclass
    :documentation "Contains the metaclass of the documented class.")

   (slots-label
    :initarg :slots-label
    :initform +default-slots-label+
    :accessor comment-slots-label
    :documentation "Contains the \\TeX\\ label to use for slot
 {\\bf slots}.")
   (slots
    :initarg :slots
    :initform nil
    :accessor comment-slots
    :documentation "Contains a list of the direct slots of
 the documented class.")

   (class-options-label
    :initarg :class-options-label
    :initform +default-class-options-label+
    :accessor comment-class-options-label
    :documentation "Contains the \\TeX\\ label to use for slot
 {\\bf class-options}.")
   (class-options
    :initarg :class-options
    :initform nil
    :accessor comment-class-options
    :documentation "Contains the class options of the documented class.")
   )
  (:documentation "
\\Purposelabel
 A class for documenting classes.
\\Seealsolabel
 \\Fcite{lisp-comment}."))

;;; ---------------------------------------------------------------------------
(defclass comment-slot (lisp-comment)
  (
   (metaobject
    :initarg :metaobject
    :initform nil
    :accessor method-metaobject
    :documentation "The method metaobject of the documented slot.")
   (syntax-label
    :initarg :syntax-label
    :initform nil
    :accessor comment-syntax-label)
   )
  (:documentation "
\\Purposelabel
 A class for documenting slots of classes.
\\Seealsolabel
 \\Fcite{lisp-comment}."))

;;; ---------------------------------------------------------------------------
(defconstant +default-function-midfix+ "fn"
  "The text to put into the \\verb|\\begin..com| command for
 functions.")

;;; ---------------------------------------------------------------------------
(defclass comment-function (lisp-comment)
  (
   (com-midfix
    :initarg :midfix
    :initform +default-function-midfix+
    :accessor comment-midfix)
   )
  (:documentation "
\\Purposelabel
 A class for documenting functions.
\\Seealsolabel
 \\Fcite{lisp-comment}."))

;;; ---------------------------------------------------------------------------
(defconstant +default-generic-function-midfix+ "gf"
  "The text to put into the \\verb|\\begin..com| command for
 generic functions.")

;;; ---------------------------------------------------------------------------
(defconstant +default-methods-label+ "\\Methodslabel"
  "The default \\TeX\\ command string for generating the
 {\\it Methods} label.")

;;; ---------------------------------------------------------------------------
(defclass comment-generic-function (lisp-comment)
  (
   (com-midfix
    :initarg :midfix
    :initform +default-generic-function-midfix+
    :accessor comment-midfix)

   (methods-label
    :initarg :methods-label
    :initform +default-methods-label+
    :accessor comment-methods-label
    :documentation "Contains the \\TeX\\ label to use for slot
 {\\bf methods}.")
   (methods
    :initarg :methods
    :initform nil
    :accessor comment-methods
    :documentation "
 Contains a list of method documentations belonging to the
 generic function.")
   )
  (:documentation "
\\Purposelabel
 A class for documenting generic functions.
\\Seealsolabel
 \\Fcite{lisp-comment}."))

;;; ---------------------------------------------------------------------------
(defclass comment-method (lisp-comment)
  (
   (metaobject
    :initarg :metaobject
    :initform nil
    :accessor method-metaobject
    :documentation "The method metaobject of the documented method.")
   (qualifiers
    :initarg :qualifiers
    :initform nil
    :accessor method-qualifiers)
   (specializers
    :initarg :specializers
    :initform nil
    :accessor method-specializers)
   (syntax-label
    :initarg :syntax-label
    :initform nil
    :accessor comment-syntax-label
    :documentation "
 Since methods don't have a {\\sc Syntax} label, this slot is always
 initialized to \\lispnil.")
   )
  (:documentation "
\\Purposelabel
 A class for documenting methods.
\\Seealsolabel
 \\Fcite{lisp-comment}."))

;;; ---------------------------------------------------------------------------
;;; Generic functions
;;; ---------------------------------------------------------------------------
(defgeneric make-lisp-comment (keyword expression &optional filter)
  (:documentation "
\\Argumentslabel
 \\isa{\\funarg{keyword}}
      {a symbol}
 \\isa{\\funarg{expression}}
      {a list}
 \\isa{\\funarg{filter}}
      {a funcallable}
\\Valueslabel
 Returns either \\lispnil\\ or an instance of [a subclass of]
 \\fcite{lisp-comment}\\ which contain comment items for
 \\funarg {keyword}.
\\Purposelabel
 Make a comment from \\funarg{keyword}; \\funarg{expression}\\ contains
 the expression read from the source file. If \\funarg{filter}\\ is
 non-\\lispnil, it should be called with the item's name as single
 argument; if the call returns \\lispnil, the item should not be
 documented at all."))

;;; ---------------------------------------------------------------------------
(defgeneric ship-out (comment to-stream)
  (:documentation "
\\Argumentslabel
 \\isa{\\funarg{comment}}
      {an instance of [a subclass of] \\fcite{lisp-comment}}
 \\isa{\\funarg{to-stream}}
      {a \\cl\\ output stream}
\\Purposelabel
 Write \\funarg{comment}\\ as \\TeX\\ command sequence to
 \\funarg{to-stream}."))

;;; ---------------------------------------------------------------------------
;;; Functions
;;; ---------------------------------------------------------------------------

(defconstant +reader-lambda-list+ '(instance)
  "The lambda list of a reader generic function.")
(defconstant +writer-lambda-list+ '(new-value instance)
  "The lambda list of a writer generic function.")

;;; ---------------------------------------------------------------------------
(defvar *generic-function-name->comment* nil
  "A hash table mapping a generic function name to its
 documenting object.")

;;; ---------------------------------------------------------------------------
(defvar *all-structure-classes* nil
  "A list containing all scanned structure classes.")

;;; ---------------------------------------------------------------------------
(defvar *all-clos-classes* nil
  "A list containing all scanned \\clos\\ classes.")

;;; ---------------------------------------------------------------------------
(defun make-slot-comment (class-name
                          expression
                          &optional (first-option nil first-option-p))
  "
\\Argumentslabel
 \\isa{\\funarg{expression}}
      {a slot-defining expression}
\\Purposelabel
 Returns \\funarg{expression}\\ transformed into an instance of
 \\fcite{comment-slot}."
  (declare (special *generic-function-name->comment*))
  (let ((name (if (consp expression)
                  (car expression)
                expression))
        (options-string (when first-option-p
                          (format nil "~S" first-option)))
        (gf-comments nil)
        (text nil))
    (when (consp expression)
      (let ((options (cdr expression)))
	(setf text (cadr (member :documentation options)))
        (do* ((option options (cddr option))
              (o (car option) (car option))
              (ov (cadr option) (cadr option)))
	    ((null option))
          (when (or (eq o :accessor) (eq o :reader) (eq o :writer))
            (let ((gf-comment (gethash ov *generic-function-name->comment*)))
	      (unless gf-comment
	        (case o
		  (:accessor
                   (setf gf-comment
		     (make-lisp-comment
		      'defgeneric
		      `(defgeneric
			   ,ov
			   ,+reader-lambda-list+
			   ,(list :documentation
			     (format
			      nil
			      "\\GfCreatedByOption{reader}{:accessor}{~A}{~A}"
			      (car expression) class-name)))))
                   (when gf-comment
                     (push gf-comment gf-comments))
                   (setf gf-comment
		     (make-lisp-comment
		      'defgeneric
		      `(defgeneric
			   (setf ,ov)
			   ,+writer-lambda-list+
			   ,(list :documentation
			     (format
			      nil
			      "\\GfCreatedByOption{writer}{:accessor}{~A}{~A}"
			      (car expression) class-name))))))
                  (:reader
                   (setf gf-comment
		     (make-lisp-comment
		      'defgeneric
		      `(defgeneric
			   ,ov
			   ,+reader-lambda-list+
			   ,(list :documentation
			     (format
			      nil
			      "\\GfCreatedByOption{reader}{:reader}{~A}{~A}"
			      (car expression) class-name))))))
                  (:writer
                   (setf gf-comment
		     (make-lisp-comment
		      'defgeneric
		      `(defgeneric
			   (setf ,ov)
			   ,+writer-lambda-list+
			   ,(list :documentation
			     (format
			      nil
			      "\\GfCreatedByOption{writer}{:writer}{~A}{~A}"
			      (car expression) class-name)))))))
                (when gf-comment
                  (push gf-comment gf-comments)))))
          (unless (eq o :documentation)
            (let ((option-string (format nil "~S ~S" o ov)))
              (setf options-string
		(if options-string
		    (concatenate 'string
		      options-string
		      (format nil "\\newline ~A" option-string))
		  option-string)))))))
    (unless options-string
      (setf options-string ""))
    (values (make-instance 'comment-slot
	      :key name
	      :metaobject nil
	      :syntax (format nil "\\Defdslot {~A} {~A}~A~%"
			      name options-string
			      +default-syntax-postfix+)
	      :text text)
            gf-comments)))

;;; ---------------------------------------------------------------------------
(defun quote-tex-characters (raw-string)
  "
\\Argumentslabel
 \\isa{\\funarg{raw-string}}
      {a string with \\TeX\\ commands}
\\Purposelabel
 Put a backslash before all \\TeX\\ special characters."
  (if (or (find #\& raw-string) (find #\# raw-string))
      (let ((cooked-string (make-string (+ 80 (length raw-string))))
	    (n 0))
	(map nil
	     #'(lambda (char)
		 (cond
                  ((or (eq char #\&) (eq char #\#))
		   (setf (char cooked-string n) #\\)
		   (incf n)))
		 (setf (char cooked-string n) char)
		 (incf n))
	     raw-string)
	(subseq cooked-string 0 n))
    raw-string))

;;; ---------------------------------------------------------------------------
(defun make-class-options-comment (class-options)
  "
\\Argumentslabel

 \\isa{\\funarg{class-options}}
      {a list with class-option value pairs}
\\Purposelabel
 Returns \\funarg{class-options}\\ transformed into a \\TeX\\ command
 sequence."
  (labels
      ((option-value-list-to-string
	(option-value-list)
	(if (consp option-value-list)
	    (let ((option-value-list-string nil))
	      (dolist (option-value option-value-list)
		(let ((option-value-string
		       (quote-tex-characters
			(format nil "~S" option-value))))
		  (setf option-value-list-string
			(if option-value-list-string
			    (concatenate 'string
					 option-value-list-string
					 (format nil " ~A"
						 option-value-string))
			  option-value-string))))
	      option-value-list-string)
	  (format nil "~S" option-value-list))))
                         
    (let ((options-string nil))
      (dolist (option class-options)
	(let ((o (car option)))
	  (unless (find o '(:include :metaclass :documentation))
	    (let ((option-string 
		   (if (eq o :print-function)
		       (format nil "~S {\\rm\\sl omitted here}" o)
		     (format nil "~S ~A"
			     o (option-value-list-to-string (cdr option))))))
	      (setf options-string
		    (if options-string
			(concatenate 'string
				     options-string
				     (format nil "\\newline ~A" option-string))
		      option-string))))))
      (when options-string
	(format nil "\\Defmethod{}{\\tt}{~A}{}~A~%"
		options-string
		+default-syntax-postfix+)))))

;;; ---------------------------------------------------------------------------
(defun make-lambda-list-comment (lambda-list)
  "
\\Argumentslabel
 \\isa{\\funarg{lambda-list}}
      {a $\\lambda$-list}
\\Purposelabel
 Returns \\funarg{lambda-list}\\ transformed into a \\TeX\\ command
 sequence."
  (let ((in-specialized-lambda-list t)
        (in-key-lambda-list nil)
        (lambda-list-string nil))
    (dolist (arg lambda-list)
      (let ((arg-string nil))
        (case arg
          (&allow-other-keys
           (setf in-specialized-lambda-list nil)
           (setf in-key-lambda-list nil)
           (setf arg-string "{\\allowotherkeys}"))
          (&body
           (setf in-specialized-lambda-list nil)
           (setf in-key-lambda-list nil)
           (setf arg-string "{\\body}"))
          (&key
           (setf in-specialized-lambda-list nil)
           (setf in-key-lambda-list t)
           (setf arg-string "\\key"))
          (&optional
           (setf in-specialized-lambda-list nil)
           (setf in-key-lambda-list nil)
           (setf arg-string "{\\opt}"))
          (&rest
           (setf in-specialized-lambda-list nil)
           (setf in-key-lambda-list nil)
           (setf arg-string "{\\rest}"))
          (t
           (setf arg-string
                 (cond
                  ((and in-specialized-lambda-list (consp arg))
                   (let ((specializer (cadr arg)))
                     (when (and (consp specializer)
                                (eq (car specializer) 'eql))
                       (let ((eql-specializer (cadr specializer)))
                         (cond
                          ((consp eql-specializer)
			   (setf eql-specializer (cadr eql-specializer))
                           (setf specializer
                                 (format nil "(eql '~S)"
					 eql-specializer)))
                          ((symbolp eql-specializer)
                           (setf specializer
                                 (format nil "(eql ~S)" eql-specializer))))))
                     (format nil "(\\funarg{~A}\\ \\ObjectWithRef{~A})"
                             (car arg) specializer)))
                  ((and (not in-specialized-lambda-list) (consp arg))
                   (format nil "{\\~Aarg{~A}}"
                           (if in-key-lambda-list "key" "fun")
                           (if (consp (car arg))
			       (caar arg)
                             (car arg))))
		  (in-specialized-lambda-list
		   (format nil "{\\funarg{~A}}" arg))
		  (t
		   (format nil "{\\~Aarg{~A}}"
                           (if in-key-lambda-list
                               "key"
                             "fun")
			   (if (consp arg)
                               (if (consp (car arg))
			           (caar arg)
                                 (car arg))
                             arg)))))))
        ;; (when lambda-list-string
        ;;  (setf arg-string (format nil "\\linebreak[3]\\ ~A"
        ;;                           arg-string)))
        (setf lambda-list-string
              (if lambda-list-string
                  (concatenate 'string
                               lambda-list-string " " arg-string)
                arg-string))))
    lambda-list-string))

;;; ---------------------------------------------------------------------------
(defun make-function-comment (function-name lambda-list)
  "
\\Argumentslabel
 \\isa{\\funarg{function-name}}
      {a symbol}
 \\isa{\\funarg{lambda-list}}
      {a $\\lambda$-list}
\\Purposelabel
 Transform the function named by \\funarg{function-name}\\ with
 $\\lambda$-list \\funarg{lambda-list}\\ into a \\TeX\\ command sequence.
\\Seealsolabel
 \\Fcite{make-lambda-list-comment}."

  (format nil "{~A} {~A}"
          function-name
          (if lambda-list
              (make-lambda-list-comment lambda-list)
            "")))

;;; ---------------------------------------------------------------------------
(defun externalp (symbol)
  "Check if \\funarg{symbol}\\ is an external symbol."
  (multiple-value-bind (found external)
      (intern #-:lispworks symbol
              #+:lispworks (symbol-name symbol)
              (symbol-package symbol))
    (declare (ignore found))
    (eq external :external)))
  
;;; ---------------------------------------------------------------------------
(defun make-key-attribute (name)
  "
\\Purposelabel
 Check if \\funarg{name}\\ is an external name
 resp.\\ to {\\tt *package*}.
\\Seealsolabel
 \\Fcite{+internal-label+};
 \\fcite{+exported-label+}."
  (let ((symbol (if (consp name)
                    (cadr name)
                  name)))
    (if (externalp symbol)
	+exported-label+
      +internal-label+)))

;;; ---------------------------------------------------------------------------
(defun make-sort-key (raw-key)
  "
\\Argumentslabel
 \\isa{\\funarg{raw-key}}
      {either a symbol or a \\lisp{setf}-expression}
\\Purposelabel
 Transform \\funarg{raw-key}\\ into a string which is used for sorting
 the document items.
\\Seealsolabel
 \\Fcite{make-lambda-list-comment}."
  (flet ((remove-redundant-characters
          (raw-string)
          "Remove redundant characters (like leading and trailing *s) from
           RAW-STRING."
          (cond
           ((find #\* raw-string)
	    (concatenate 'string (remove #\* raw-string) "*"))
           ((find #\+ raw-string)
	    (concatenate 'string (remove #\+ raw-string) "+"))
           (t
            raw-string))))
    (cond
     ((stringp raw-key)
      (remove-redundant-characters raw-key))
     ((symbolp raw-key)
      (remove-redundant-characters (symbol-name raw-key)))
     ((and (consp raw-key)
	   (eq (car raw-key) 'setf))
      (concatenate 'string
                   (remove-redundant-characters (symbol-name (cadr raw-key)))
                   " SETF"))
     (t
      raw-key))))

;;; ---------------------------------------------------------------------------
(defconstant +clos-classes+
    (mapcar #'find-class
	    '(t
	      clos::standard-object clos:metaobject
	      clos::class
	      clos::built-in-class
	      clos::standard-class
	      clos:slot-definition
	      clos:standard-slot-definition
	      clos:standard-direct-slot-definition
	      clos:standard-effective-slot-definition
	      clos::method
	      clos::standard-method
	      clos:standard-accessor-method
	      clos:standard-reader-method
	      clos:standard-writer-method
	      clos::method-combination))
  "A list containing all public CLOS and MOP classes.")

;;; ---------------------------------------------------------------------------
(defun insert-class (the-class class-list)
  "Insert \\funarg{the-class}\\ into \\funarg{class-list}."
  (labels ((insert-superclasses (the-class class-list)
	     (when (member the-class +clos-classes+)
	       (setf class-list (pushnew the-class class-list)))
	     (let ((superclasses (clos:class-direct-superclasses the-class)))
	       (mapc #'(lambda (class)
			 (setf class-list (insert-superclasses
					   class class-list)))
		     superclasses)
	       class-list)))
	   
    (if (member the-class class-list)
	class-list
      (progn
	(setf class-list (push the-class class-list))
	(insert-superclasses the-class class-list)))))

;;; ---------------------------------------------------------------------------
(defun ship-out-class-tree (class-list name to-stream)
  "Write \\funarg{class-list}\\ as a class tree to \\funarg{to-stream}."
  (let ((text (format nil "~%")))

    (labels

	((top-level-class-p
	     (class class-list)
	   (loop for other-class in class-list
	       finally (return t)
	       unless (eq class other-class)
	       when (subtypep class other-class)
	       do (return nil)))

	 (top-level-classes
	     (class-list)
	   (let* ((copied-class-list (copy-seq class-list))
		  (top-level
		   (mapcan #'(lambda (class)
			       (when (top-level-class-p class class-list)
				 (delete class copied-class-list)
				 (list class)))
			   class-list)))
	     (setf top-level
	       (sort top-level #'(lambda (name-1 name-2)
				   (string< (symbol-name name-1)
					    (symbol-name name-2)))
		     :key #'class-name))
	     (values top-level copied-class-list)))

	 (descend-from-top-level (top-class rest-classes
				  &optional (level 0))
	   (let ((subclasses
		  (loop for subclass in rest-classes
		      nconc (when (and (not (eq subclass top-class))
				       (subtypep subclass top-class))
			      (list subclass))))
		 (top-name (class-name top-class)))
	     (setf text (concatenate 'string
			  text
			  (format
			   nil
			   "\\ClassHierarchyLine{~A}{~A ~A Class}{~A}~%"
			   level
			   (make-key-attribute top-name) name
			   top-name)))
	     (multiple-value-bind (top-level rest-classes)
		 (top-level-classes subclasses)
	       (loop for top-class in top-level
		 do (descend-from-top-level top-class rest-classes
					    (1+ level)))))))

    (multiple-value-bind (top-level rest-classes)
	(top-level-classes class-list)
      (loop for top-class in top-level
	  do (descend-from-top-level top-class rest-classes))
      (ship-out (make-instance 'comment-documentation
		  :key (format nil "~A Class Hierarchy" name)
		  :key-attribute "Class Hierarchy"
		  :text text)
		to-stream))))
  (values))

;;; ---------------------------------------------------------------------------
;;; Methods
;;; ---------------------------------------------------------------------------

; Following defgeneric is only seen when this file is scanned by lisp-doc:
#+lisp-doc
(defgeneric clos::print-object (clos::instance stream)
  (:documentation "
 See \\fcite{print-object}."))

;;; ---------------------------------------------------------------------------
(defmethod print-object ((object lisp-comment) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A" (comment-key object))))

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment (keyword expression &optional filter)
  "Return always \\lispnil; this will generate no document text at all."
  nil)

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment ((keyword (eql :defdoc)) expression
			      &optional filter)
  (let* ((name (cadr expression))
         (attribute (caddr expression))
         (text (when (stringp (cadddr expression))
		 (cadddr expression))))
    (when (or (null filter)
	      (funcall filter name))
      (make-instance 'comment-documentation
	:key name
	:key-attribute attribute
	:text text))))

;;; ---------------------------------------------------------------------------
(defconstant +undocumented-item-prompt+
  ";; *** Warning: Undocumented ~A ~S~%"
  "Prompt for \\lisp{undocumented ...}\\ warnings.")

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment ((keyword (eql 'defconstant)) expression
			      &optional filter)
  (let* ((name (cadr expression))
         (text (when (stringp (cadddr expression))
		 (cadddr expression))))
    (when (or (null filter)
	      (funcall filter name))
      (if text
	  (unless (search "\\Purposelabel" text)
	    (setf text (format nil "~%\\Purposelabel~%~A" text)))
	(format t +undocumented-item-prompt+ keyword name))
      (setf text
	(let ((*print-pretty* t))
	  (format nil "~%\\Valuelabel~%\\begin{CompactCode}~%~
			 ~S~%\\end{CompactCode}~%~A"
		  (caddr expression)
		  (if text text ""))))
      (make-instance 'comment-constant
	:key name
	:key-attribute (make-key-attribute name)
	:text text))))

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment ((keyword (eql 'defparameter)) expression
			      &optional filter)
  (let* ((name (cadr expression))
         (text (when (stringp (cadddr expression))
		 (cadddr expression))))
    (when (or (null filter)
	      (funcall filter name))
      (if text
	  (unless (search "\\Purposelabel" text)
	    (setf text (format nil "~%\\Purposelabel~%~A" text)))
	(format t +undocumented-item-prompt+ keyword name))
      (setf text
	(let ((*print-pretty* t))
	  (format nil "~%\\Initialvaluelabel~%\\begin{CompactCode}~%~
			 ~S~%\\end{CompactCode}~%~A"
		  (caddr expression)
		  (if text text ""))))
      (make-instance 'comment-parameter
	:key name
	:key-attribute (make-key-attribute name)
	:text text))))

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment ((keyword (eql 'defvar)) expression
			      &optional filter)
  (let* ((name (cadr expression))
         (text (when (stringp (cadddr expression))
		 (cadddr expression))))
    (when (or (null filter)
	      (funcall filter name))
      (if text
	  (unless (search "\\Purposelabel" text)
	    (setf text (format nil "~%\\Purposelabel~%~A" text)))
	(format t +undocumented-item-prompt+ keyword name))
      (setf text
	(let ((*print-pretty* t))
	  (format nil "~%\\Initialvaluelabel~%\\begin{CompactCode}~%~
			 ~S~%\\end{CompactCode}~%~A"
		  (caddr expression)
		  (if text text ""))))
      (make-instance 'comment-variable
	:key name
	:key-attribute (make-key-attribute name)
	:text text))))

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment ((keyword (eql 'defmacro)) expression
			      &optional filter)
  (let* ((name (cadr expression))
         (lambda-list (cddr expression))
         (text (when (stringp (cadr lambda-list))
		 (cadr lambda-list)))
         (macro-comment
          (format nil "\\Defmac ~A~A~%"
		  (make-function-comment name (car lambda-list))
		  +default-syntax-postfix+)))
    (when (or (null filter)
	      (funcall filter name))
      (unless text
	(format t +undocumented-item-prompt+ keyword name))
      (make-instance 'comment-macro
	:key name
	:key-attribute (make-key-attribute name)
	:syntax macro-comment
	:text text))))

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment ((keyword (eql 'defstruct)) expression
			      &optional filter)
  (declare (special *all-structure-classes*))
  (let* ((name (if (consp (cadr expression))
                   (caadr expression)
		 (cadr expression)))
	 (the-class (find-class name nil))
         (options (when (consp (cadr expression))
                    (cdadr expression)))
         (text (when (stringp (caddr expression))
                 (caddr expression)))
         (slots (if text
                    (cdddr expression)
		  (cddr expression)))
         (superclasses (mapcan #'(lambda (option)
                                   (when (eq (car option) :include)
                                     (cdr option)))
                               options)))
    (when (or (null filter)
	      (funcall filter name))
      (when the-class
	(setf *all-structure-classes*
	  (insert-class the-class *all-structure-classes*)))
      (unless text
	(format t +undocumented-item-prompt+ keyword name))
      (make-instance 'comment-class
	:key name
	:key-attribute
	(format nil "~A Structure" (make-key-attribute name))
	:superclasses
	(when superclasses
	  (format nil "\\Defmethod{}{\\tt}{~A}{}~A~%"
		  (loop for s in superclasses
		      as text = ""
		      finally (return text)
		      do
			(setf text
			  (concatenate 'string
			    text
			    (format nil "\\ObjectWithRef{~A}" s))))
		  +default-syntax-postfix+))
	:slots (mapcar #'(lambda (slot)
			   (if (consp slot)
			       (make-slot-comment
				name
				(cons (car slot) (cddr slot))
				(cadr slot))
			     (make-slot-comment
			      name slot)))
		       slots)
	:class-options (make-class-options-comment options)
	:text text))))

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment ((keyword (eql 'defclass)) expression
			      &optional filter)
  (declare (special *all-clos-classes*))
  (let* ((name (cadr expression))
	 (the-class (find-class name nil))
         (superclasses (caddr expression))
         (slots (cadddr expression))
         (options (cddddr expression))
         (metaclass (cadr (find-if #'(lambda (x)
				       (and (consp x)
				            (eq (car x) :metaclass)))
			           options)))
         (text (cadr (find-if #'(lambda (x)
				  (and (consp x)
				       (eq (car x) :documentation)))
			      options))))
    (when (or (null filter)
	      (funcall filter name))
      (when the-class
	(setf *all-clos-classes* (insert-class the-class *all-clos-classes*)))
      (unless text
	(format t +undocumented-item-prompt+ keyword name))
      (let ((slot-comments ())
	    (slot-generics ()))
	(loop for slot in slots do
	      (multiple-value-bind (slot-comment slot-generic)
		  (make-slot-comment name slot)
		(when slot-comment
		  (push slot-comment slot-comments))
		(cond
		 ((consp slot-generic)
		  (setf slot-generics (nconc slot-generic slot-generics)))
		 (slot-generic
		  (push slot-generic slot-generics)))))
	(cons
	 (make-instance 'comment-class
	   :key name
	   :key-attribute (make-key-attribute name)
	   :superclasses
	   (when superclasses
	     (format nil "\\Defmethod{}{\\tt}{~A}{}~A~%"
		     (loop for s in superclasses
			 collect
			   (format nil "\\ObjectWithRef{~A}" s))
		     +default-syntax-postfix+))
	   :metaclass
	   (when metaclass
	     (format nil "\\Defmethod{}{\\tt}{\\ObjectWithRef{~A}}{}~A~%"
		     metaclass +default-syntax-postfix+))
	   :slots (nreverse slot-comments)
	   :class-options (make-class-options-comment options)
	   :text text)
	 slot-generics)))))

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment ((keyword (eql 'defun)) expression
			      &optional filter)
  (let* ((name (cadr expression))
         (lambda-list (cddr expression))
         (text (when (stringp (cadr lambda-list))
		 (cadr lambda-list)))
         (function-comment
          (format nil "\\Defun ~A~A~%"
		  (make-function-comment name (car lambda-list))
		  +default-syntax-postfix+)))
    (when (or (null filter)
	      (funcall filter name))
      (unless text
	(format t +undocumented-item-prompt+ keyword name))
      (make-instance 'comment-function
	:key name
	:key-attribute (make-key-attribute name)
	:syntax function-comment
	:text text))))

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment ((keyword (eql 'defgeneric)) expression
			      &optional filter)
  (declare (special *generic-function-name->comment*))
  (let* ((name (cadr expression))
         (lambda-list (caddr expression))
         (text (cadr (find-if #'(lambda (x)
				  (and (consp x)
				       (eq (car x) :documentation)))
			      expression)))
         (function-comment (format nil "\\Defgen ~A~A~%"
                                   (make-function-comment name lambda-list)
                                   +default-syntax-postfix+))
         (comment (gethash name *generic-function-name->comment*))
         (return-comment nil))
    (when (or (null filter)
	      (funcall filter name))
      (unless text
	(format t +undocumented-item-prompt+ keyword name))
      (if comment
	  (progn
	    (setf (comment-key comment) name)
	    (setf (comment-syntax comment) function-comment)
	    (setf (comment-text comment) text))
	(progn
	  (setf return-comment
	    (make-instance 'comment-generic-function
	      :key name
	      :key-attribute (make-key-attribute name)
	      :syntax function-comment
	      :text text))
	  (setf (gethash name *generic-function-name->comment*)
	    return-comment)))
      return-comment)))

;;; ---------------------------------------------------------------------------
(defconstant +default-method-text+
  "%
% No behavior is specified for this method beyond that which is specified
% for the generic function."
  "The default text for methods without any documentation.")

;;; ---------------------------------------------------------------------------
(defmethod make-lisp-comment ((keyword (eql 'defmethod)) expression
			      &optional filter)
  (declare (special *generic-function-name->comment*))
  (let* ((name (cadr expression))
         (gf-comment (gethash name *generic-function-name->comment*))
         (return-comment nil)
         (qualifiers nil)
         (lambda-rest (cddr expression))
         (lambda-list ())
         (specializers nil))
    (when (or (null filter)
	      (funcall filter name))
      ;; Extract the qualifier:
      (unless (consp (car lambda-rest))
	(setf qualifiers (car lambda-rest))
	(setf lambda-rest (cdr lambda-rest)))
      (setf lambda-list (car lambda-rest))
      (setf specializers (clos::extract-specializer-names lambda-list))
      (let* ((text (when (stringp (cadr lambda-rest))
		     (cadr lambda-rest)))
	     (signature
	      (format nil "~A ~A~A"
		      name
		      (if qualifiers
			  (format nil ":~A " qualifiers)
			"")
		      specializers))
	     (function-comment
	      (format nil "\\flabel{\\protect\\mtd}{~A}{}~%{\\~A ~A}~A"
		      signature
		      (cond
		       ((null qualifiers)
			"Defmeth")
		       ((eq qualifiers :around)
			"Defmetharound")
		       ((eq qualifiers :before)
			"Defmethb")
		       ((eq qualifiers :after)
			"Defmetha")
		       (t
			(format
			 nil
			 "Defmethod{~A}{\TT}{~A}{~A-Method}\\startindent"
			 name lambda-list qualifiers)))
		      (make-function-comment name lambda-list)
		      +default-syntax-postfix+))
	     (comment
	      (make-instance
		  'comment-method
		:key name
		:metaobject
		(find-method (ensure-generic-function name)
			     (when qualifiers
			       (list qualifiers))
			     (map 'list
			       #'(lambda (specializer)
				   (cond
				    ((symbolp specializer)
				     (find-class specializer nil))
				    ((and (eq (car specializer) 'eql)
                                          (or (consp (cadr specializer))
                                              (boundp (cadr specializer))))
				     (list 'eql
					   (eval (cadr specializer))))
				    (t
				     specializer)))
			       specializers)
			     nil)
		:qualifiers qualifiers
		:specializers specializers
		:syntax function-comment
		:text (if text
			  text
			+default-method-text+))))
	(unless gf-comment
	  (setf gf-comment
	    (make-lisp-comment 'defgeneric
			       `(defgeneric
				    ,name
				    ,(clos::extract-lambda-list lambda-list)
				    (:documentation :unknown))))
	  (setf return-comment gf-comment))
	(push comment
	      (comment-methods gf-comment)))
      return-comment)))

;;; ---------------------------------------------------------------------------
(defun ship-out-label-and-argument (label argument to-stream)
  "
\\Purposelabel
 Write \\funarg{argument}\\ as \\TeX\\ command sequence following
 the \\funarg{label}\\ to \\funarg{to-stream}.
\\Seealsolabel
 \\Fcite{ship-out}."
  (when argument
    (when label
      (format to-stream "~%~A~%" label))
    (format to-stream "~A~%" argument))
  (values))

;;; ---------------------------------------------------------------------------
(defmethod ship-out ((c lisp-comment) to-stream)
  (ship-out-label-and-argument (comment-syntax-label c)
			       (comment-syntax c)
			       to-stream)
  (let ((text (comment-text c)))
    (when text
      (ship-out-label-and-argument nil
                                   (format nil
                                           "~A~%"
                                           text)
                                   to-stream))))

;;; ---------------------------------------------------------------------------
(defmethod ship-out :before (comment to-stream)
  "Write out the \\verb|\\begin..com| command."
  (let ((midfix (comment-midfix comment)))
    (when midfix
      (let ((attribute (comment-key-attribute comment)))
        (format to-stream
	        "\\begin~Acom~A~A~A{~A}~%"
	        midfix
                (if attribute
                    "["
                  "")
                (if attribute
	            attribute
                  "")
                (if attribute
                    "]"
                  "")
		(comment-key comment))))))

;;; ---------------------------------------------------------------------------
(defmethod ship-out :after (comment to-stream)
  "Write out the \\verb|\\endcom| command."
  (when (comment-midfix comment)
    (format to-stream
	    "~%\\endcom~%~%")))

;;; ---------------------------------------------------------------------------
(defmethod ship-out ((c comment-class) to-stream)
  (call-next-method)
  (ship-out-label-and-argument (comment-superclasses-label c)
			       (comment-superclasses c)
			       to-stream)
  (let ((slots (comment-slots c)))
    (when slots
      (format to-stream "~%~A~%~%"
              (comment-slots-label c))
      (dolist (slot slots c)
	(ship-out slot to-stream))))
  (ship-out-label-and-argument (comment-metaclass-label c)
			       (comment-metaclass c)
			       to-stream)
  (ship-out-label-and-argument (comment-class-options-label c)
			       (comment-class-options c)
			       to-stream))

;;; ---------------------------------------------------------------------------
(defconstant +qualifier-ordering+
  '(:around :before nil :after)
  "A list imposing an ordering on method qualifiers for computing
   the order of methods to be printed.")

;;; ---------------------------------------------------------------------------
(defconstant +unknown-qualifier-continue-prompt+
  "Dont't order the methods."
  "The continue prompt for unknown method qualifiers.")

;;; ---------------------------------------------------------------------------
(defconstant +unknown-qualifier-error-prompt+
  "Found unknown qualifier ~A in method ~A."
  "The error prompt for unknown method qualifiers.")

;;; ---------------------------------------------------------------------------
(defmethod ship-out :around ((c comment-generic-function) to-stream)
  (if (comment-methods c)
      (call-next-method)
    (let ((name (comment-key c)))
      (format to-stream
	      "\\message{^^JGeneric function ~S has no methods.^^J}~%"
              name)
      (format t ";; *** Warning: Generic function ~A has no methods.~%"
	      name))))

;;; ---------------------------------------------------------------------------
(defmethod ship-out ((c comment-generic-function) to-stream)
  (labels

      ((method-description-more-specific-p
        (method-1 method-2)
        (mapc
         #'(lambda (spec-1 spec-2)
	     (cond
	      ((and (consp spec-1) (consp spec-2))
	       (return-from method-description-more-specific-p
                 (not (string-greaterp
                       (format nil "~S" (cadr spec-1))
                       (format nil "~S" (cadr spec-2))))))
	      ((consp spec-1)
	       (return-from method-description-more-specific-p t))
	      ((consp spec-2)
	       (return-from method-description-more-specific-p nil))
	      (t
	       (let ((class-1 (find-class spec-1 nil))
		     (class-2 (find-class spec-2 nil)))
		 (unless (and class-1 class-2)
		   (return-from method-description-more-specific-p t))
		 (unless (eq class-1 class-2)
		   (let ((class-name-1 (class-name class-1))
			 (class-name-2 (class-name class-2)))
		     (return-from method-description-more-specific-p
		       (cond
			((subtypep class-name-1 class-name-2)
			 t)
			((subtypep class-name-2 class-name-1)
			 nil)
			(t
			 (not (string-greaterp (symbol-name class-name-1)
                                               (symbol-name class-name-2))))))))))))
	 (method-specializers method-1)
	 (method-specializers method-2))
        t)
        
       (method-not-greater-p
        (method-1 method-2)
        (let* ((qualifiers-1 (method-qualifiers method-1))
               (ord-1 (position qualifiers-1 +qualifier-ordering+))
               (qualifiers-2 (method-qualifiers method-2))
               (ord-2 (position qualifiers-2 +qualifier-ordering+)))
          (cond
           ((null ord-1)
            (cerror +unknown-qualifier-continue-prompt+
                    +unknown-qualifier-error-prompt+ qualifiers-1 method-1)
            t)
           ((null ord-2)
            (cerror +unknown-qualifier-continue-prompt+
                    +unknown-qualifier-error-prompt+ qualifiers-2 method-2)
            nil)
           ((< ord-1 ord-2)
            t)
           ((> ord-1 ord-2)
            nil)
           (t
            (method-description-more-specific-p method-1 method-2)))))

       (sort-methods
        (methods-list)
        (sort methods-list
              #'method-not-greater-p)))

    (let ((name (comment-key c))
          (methods (sort-methods (comment-methods c))))
      (when (eq (comment-text c) :unknown)
	(format t ";; *** Warning: Generic function ~A has no :documentation.~%"
		name)
	(setf (comment-text c)
	      (format nil
		      "\\IfFLabelExists{~S}~%~
		      {\\SeeAt\\ \\fcite{~S}.}~%~
                      {\\message{^^JUndocumented generic function ~S.^^J}~%~
		       \\nodefgeneric}"
		      name name name)))
      (call-next-method)
      (format to-stream "~%~A~%~%"
	      (comment-methods-label c))
      (dolist (method methods)
	(ship-out method to-stream)))))

;;; ---------------------------------------------------------------------------
(defun scan-one-file (filename &optional filter)
  "
\\Argumentslabel
 \\isa{\\funarg{filename}}
      {a pathname}
\\Purposelabel
 Scan file named by \\funarg{filename}\\ and extract its documentation.
\\Seealsolabel
 \\Fcite{scan-files}."
  (let ((all-comments ())
        (current-features *features*)
        (current-package *package*))
    (unwind-protect
        (with-open-file (stream filename :direction :input)
          (let ((expr nil))
            (push :lisp-doc *features*)
            (loop
		do (setf expr (read stream nil :eof))
		until (eq expr :eof)
		when (consp expr)
		do (if (eq (car expr) 'in-package)
		       (eval `(in-package ,(cadr expr)))
		     (let ((one-comment
			    (make-lisp-comment (car expr) expr filter)))
		       (cond
			((consp one-comment)
			 (setf all-comments (nconc one-comment all-comments)))
			(one-comment
			 (push one-comment all-comments))))))))
      (progn
        (setf *features* current-features)
        (unless (eq current-package *package*)
          (eval `(in-package ,(package-name current-package))))))
    all-comments))

;;; ---------------------------------------------------------------------------
(defun scan-myself (&optional (my-path "."))
  "
\\Argumentslabel
 \\isa{\\funarg{my-path}}
      {a path expression}
\\Purposelabel
 Extract the documentation strings from module \\lisp{lisp-doc}.
 The \\funarg{my-path}\\ argument contains the path information
 where the \\lisp{lisp-doc}\\ file is located.
\\Seealsolabel
 \\Fcite{scan-files}."

  (scan-file (concatenate 'string my-path "/lisp-doc.lisp")
             (concatenate 'string my-path "/ldocrefg.tex")))

;;; ---------------------------------------------------------------------------
(defun scan-file (&rest all-args)
  "See \\fcite{scan-files}."
  (apply #'scan-files all-args))

;;; ---------------------------------------------------------------------------
(defun scan-files (file-list &optional to-stream filter)
"
\\Argumentslabel
 \\isa{\\funarg{file-list}}
      {a list with LISP source file names without
       \\lisp{.lisp}\\ extension}
 \\isa{\\funarg{to-stream}}
      {either \\lispnil\\ or a string or a \\cl\\ output stream}
\\Valueslabel
 The output stream which was used for creating the \\TeX\\ file is
 returned.
\\Purposelabel
 Scan the LISP source files in \\funarg{file-list}\\ and write their
 documentation to \\funarg{to-stream}."

  (let ((*print-case* :downcase)
	(*generic-function-name->comment* (make-hash-table :test 'equal))
	(*all-structure-classes* nil)
	(*all-clos-classes* nil))

    (labels ((ship-out-all-comments
		 (all-comments to-stream)
	       (format to-stream "%
% This file was generated from the LISP module `lisp-doc' by extracting
% the LISP documentation strings of the files
%
")
	      (mapc #'(lambda (f)
			(format to-stream "%   ~A~%" f))
		    file-list)
	      (multiple-value-bind (second minute hour date month year)
		  (decode-universal-time (get-universal-time))
		(declare (ignore second))
		(format to-stream "%
% as of ~D/~2,'0D/~2,'0D ~D:~2,'0D    (year/mm/dd hh:mm)
% Changes done here will be lost.
%
% lisp-doc (C) 1994--1998 Heiko Kirschke
%
\\let\\tempa\\relax%
\\ifx\\flabel\\undefined%
 \\let\\tempa\\endinput%
 Missing document substyle {\\tt `crossref.sty'}; ending input.
 \\message{^^J^^JMissing document substyle `crossref.sty'; ending input.^^J^^J}%
\\fi%
\\ifx\\Seealsolabel\\undefined%
 \\let\\tempa\\endinput%
 Missing document substyle {\\tt `lispdoc.sty'}; ending input.
 \\message{^^J^^JMissing document substyle `lispdoc.sty'; ending input.^^J^^J}%
\\fi%
\\tempa\\def\\tempa{}%

"
			year month date hour minute))
	      (when *all-structure-classes*
		(ship-out-class-tree *all-structure-classes* "Structure"
				     to-stream))
	      (when *all-clos-classes*
		(ship-out-class-tree *all-clos-classes* "CLOS"
				     to-stream))
	      (dolist (one-comment all-comments)
		(format t "; Writing out ~S~%" (comment-key one-comment))
		(ship-out one-comment to-stream))))

    (let ((all-comments ()))
      (unless (consp file-list)
        (setf file-list (list file-list)))
      (dolist (file file-list)
        (let ((one-comment
	       (scan-one-file (merge-pathnames file ".lisp") filter)))
          (when one-comment
            (setf all-comments (nconc all-comments one-comment)))))
      (setf all-comments
            (sort all-comments
                  #'string-lessp
                  :key #'(lambda (item)
                           (make-sort-key (comment-key item)))))
      (if (streamp to-stream)
          (ship-out-all-comments all-comments to-stream)
        (with-open-file (temp-stream (if to-stream to-stream "doc.tex")
                                     :direction :output
                                     :if-exists :supersede)
          (setf to-stream temp-stream)
          (ship-out-all-comments all-comments to-stream))))))
  to-stream)

;;; ---------------------------------------------------------------------------
(export '(scan-myself scan-file scan-files))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
