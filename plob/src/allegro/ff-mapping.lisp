;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	ff-mapping.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	1997/03/04
;;;; Description	PLOB functions for foreign function interface
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

(defvar *ff-name-to-entry-table* (make-hash-table :test 'equal))

(defun register-c-callable (name entry)
  (setf (gethash name *ff-name-to-entry-table*) entry))

(defun argument-name (argument)
  (car argument))
(defun argument-type (argument)
  (cadr argument))
(defun argument-reference (argument)
  (caddr argument))
(defun argument-buffer (argument)
  (cadddr argument))
(defun argument-buffer-alloc (argument)
  (cadddr (cdr argument)))

(defun pass-by-reference-p (argument)
  (let ((reference (argument-reference argument)))
    (or (eq reference :value-out) (eq reference :vector-out))))

(defun ff-map-extract-arguments (arguments-list)
  (map 'list #'argument-name arguments-list))

(defun set-sequence-length (sequence new-length)
  "Change length of \\funarg{sequence}\\ to \\funarg{new-length}."
  (if (and new-length (< new-length (length sequence)))
      (delete-if #'(lambda (item)
		     (declare (ignore item))
		     t)
		 sequence
		 :start new-length)
    sequence))

(defun gensym-with-prefix (&optional prefix)
  (if prefix
      (gensym (concatenate 'string (symbol-name prefix) "-"))
    (gensym)))

#+(and allegro (not (version>= 5)))
(defmacro define-foreign-function
    (result-type c-function-name lisp-function-name function-args)

  (labels
      (
       (map-foreign-arguments
        (arguments-list)
        (map 'list
	     #'(lambda (argument)
	         (let* ((type (argument-type argument))
		        (cl-type (intern (symbol-name type) :cl))
		        (reference (argument-reference argument)))
		   (cond
		    ((eq type :lisp) type)
		    ((eq reference :value-out) `(simple-array ,cl-type (1)))
		    (t cl-type))))
	     arguments-list))

       (map-reference
        (raw-reference)
        (ecase raw-reference
          (:value-out :by-address)
          ((:value-in :vector-out :vector-in) :by-value)))

       (map-extract-pass-types
        (arguments-list)
        (map 'list #'(lambda (argument)
                       (let ((type (argument-type argument))
                             (reference (argument-reference argument)))
                         (cond
                          ((eq type :lisp) (map-reference reference))
                          (t :by-value))))
             arguments-list))
	 
       (make-foreign-function
        (result-type c-function-name lisp-function-name arguments-list)
        `(ff:defforeign
          (quote ,lisp-function-name)
          :entry-point ,c-function-name
          :arguments (quote ,(map-foreign-arguments function-args))
          :pass-types (quote ,(map-extract-pass-types function-args))
	  :language :c
          :return-type ,result-type))
	   
       (map-set-foreign-arguments
        (arguments-list)
        (mapcan #'(lambda (argument)
                    (let ((type (argument-type argument))
                          (reference (argument-reference argument)))
                      (when (and (not (eq type :string))
                                 (eq reference :value-out))
                        (let ((name (argument-name argument))
                              (cl-type (intern (symbol-name type) :cl)))
                          (when (eq type :integer)
                            (warn "Passing INTEGER by-address does not work; ~
			           using FIXNUM instead.")
                            (setf cl-type 'fixnum))
                          `((setf ,name (make-array
                                         1
                                         :element-type (quote ,cl-type)
                                         :initial-element ,name)))))))
                arguments-list))

       (map-return-values
        (arguments-list)
        (mapcan #'(lambda (argument)
                    (when (pass-by-reference-p argument)
                      (let ((name (argument-name argument))
                            (type (argument-type argument))
                            (reference (argument-reference argument)))
                        (cond
                         ((eq type :string)
                          `((set-sequence-length
                             ,name
                             (position #\Null ,name))))
                         ((eq reference :value-out)
                          `((aref ,name 0)))
                         (t (list name))))))
                arguments-list))
       )
    (let ((set-arguments (map-set-foreign-arguments function-args))
	  (return-values (map-return-values function-args)))
      (if (or set-arguments return-values)
	  (let* ((c-result (unless (eq result-type :void)
			     (gensym-with-prefix 'c-result)))
		 (lisp-stub-function-name
		  (gensym-with-prefix lisp-function-name))
		 (call-stub-expr `(,lisp-stub-function-name
				   ,@(ff-map-extract-arguments
				      function-args)))
		 (lisp-call-stub-expr
		  (if c-result
		      `((let ((,c-result ,call-stub-expr))
			  (values ,c-result ,@return-values)))
		    `(,call-stub-expr
		      (values ,@return-values)))))
	    `(progn
	       (eval-when (:compile-toplevel :load-toplevel :execute)
		 ,(make-foreign-function result-type c-function-name
					 lisp-stub-function-name
					 function-args))
	       (defun ,lisp-function-name
		      ,(ff-map-extract-arguments function-args)
		 ,@set-arguments
		 ,@lisp-call-stub-expr)))
	(make-foreign-function result-type c-function-name
			       lisp-function-name function-args)))))

#+(and :allegro (not (version>= 5)))
(defmacro define-foreign-callable
    (result-type c-function-name lisp-function-name function-args)
  (flet ((map-callable-types (arguments-list)
	   (map 'list
	     #'(lambda (argument)
		 (list (car argument) :lisp))
	     arguments-list)))
    (let ((lisp-stub-function-name (gensym-with-prefix lisp-function-name)))
      `(progn
	 (eval-when (:compile-toplevel :load-toplevel)
	   (ff:defun-c-callable ,lisp-stub-function-name
	       ,(map-callable-types function-args)
	     (,lisp-function-name
	      ,@(ff-map-extract-arguments function-args))))
	 (eval-when (:load-toplevel)
	   (register-c-callable ,c-function-name
				(ff:register-function
				 (quote ,lisp-stub-function-name))))))))

#+(and :allegro (version>= 5))
(defun map-foreign-type-to-c-type (foreign-type reference)
  (if (eq foreign-type :string)
       #-(version>= 6)
       ;; 2001-02-08 HK: ACL 5:
      '(* :char)
       #+(version>= 6)
       ;; 2001-02-08 HK: ACL 6:
       (if (or (eq reference :value-out)
	       (eq reference :vector-out))
	   :int
	   '(* :char))
       (case reference
      (:value-in foreign-type)
      (t 
       #-(version>= 6)
       ;; 2001-02-08 HK: ACL 5:
       `(* ,foreign-type)
       #+(version>= 6)
       ;; 2001-02-08 HK: ACL 6:
       (if (eq foreign-type :lisp)
	   ;; 2001-02-08 HK: In ACL 6, :lisp objects are always passed
	   ;; by reference: (really?)
	   foreign-type
	 `(* ,foreign-type))
       ))))

#+(and :allegro (version>= 5))
(defun map-foreign-type-to-lisp-type (foreign-type reference)
  (let ((lisp-type (case foreign-type
		     (:int 'integer)
		     (:float 'single-float)
		     (:double 'double-float)
		     (:lisp t)
		     (t (intern (symbol-name foreign-type) :cl)))))
    (cond
     #+(version>= 6)
     ;; 2001-02-08 HK: ACL 6:
     ((eq lisp-type t)
      lisp-type)
     #+(version>= 6)
     ;; 2001-02-08 HK: ACL 6:
     ((eq lisp-type 'string)
      (when (or (eq reference :value-out)
		(eq reference :vector-out))
	  'integer))
     ((or (eq reference :value-out)
	  (and (eq lisp-type t)
	       (or (eq reference :vector-in)
		   (eq reference :vector-out))))
      `(simple-array ,(if (eq lisp-type 'integer)
			  '(signed-byte 32)
			lisp-type)
		     (1)))
     (t lisp-type))))

#+(and :allegro (version>= 5))
(defun map-foreign-arguments (arguments-list)
  (map 'list
    #'(lambda (argument)
	(let* ((name (argument-name argument))
	       (type (argument-type argument))
	       (reference (argument-reference argument))
	       (lisp-type 
		(map-foreign-type-to-lisp-type type reference)))
	  `(,name
	    ,(map-foreign-type-to-c-type type reference)
	    ,@(when lisp-type
		(list lisp-type)))))
    arguments-list))

#+(and :allegro (version>= 5))
(defmacro define-foreign-function
    (result-type c-function-name lisp-function-name function-args)

  (labels
      (
       (make-foreign-function
	   (result-type c-function-name lisp-function-name arguments-list)
	 (let ((body `(ff:def-foreign-call
			  (,lisp-function-name ,c-function-name)
			  ,(map-foreign-arguments arguments-list)
			:convention :c
			:returning
			,(map-foreign-type-to-c-type result-type :value-in)
			#+(version>= 6)
			;; 2001-02-08 HK: ACL 6:
			:strings-convert
			#+(version>= 6)
			;; 2001-02-08 HK: ACL 6:
			t)))
	   body))

       (map-let-foreign-arguments
        (arguments-list)
        (mapcan #'(lambda (argument)
                    (let ((buffer (argument-buffer argument))
                          (buffer-alloc (argument-buffer-alloc argument)))
                      (when buffer
			`((,buffer ,buffer-alloc)))))
                arguments-list))

       (map-set-foreign-arguments
	   (arguments-list)
	 (mapcan #'(lambda (argument)
		     (let ((type (argument-type argument))
			   (reference (argument-reference argument)))
		       (nconc
			(when (and #-(version>= 6)
				   ;; 2001-02-08 HK: ACL 5:
				   t
				   #+(version>= 6)
				   ;; 2001-02-08 HK: ACL 6:
				   (not (eq type :lisp))
				   (or (and (not (eq type :string))
					    (eq reference :value-out))
				       (and (eq type :lisp)
					    (or (eq reference :vector-in)
						(eq reference :vector-out)))))
			  (let ((name (argument-name argument))
				(cl-type (map-foreign-type-to-lisp-type
					  type :value-in)))
			    (if (eq cl-type 'integer)
				(setf cl-type '(signed-byte 32)))
			    `((setf ,name (make-array
					   1
					   :element-type (quote ,cl-type)
					   :initial-element ,name)))))
			#+(version>= 6)
			;; 2001-02-08 HK: ACL 6:
			(when (and (eq type :string)
				   (pass-by-reference-p argument))
			  (let ((name (argument-name argument))
				(string-buffer
				 (gensym-with-prefix 'string-buffer)))
			    ;; Append name of string buffer argument:
			    (nconc argument (list string-buffer))
			    (nconc argument
				   `((excl:aclmalloc
				      (1+ (length ,name))))))
			  nil))))
		 arguments-list))

       (map-return-values
	   (arguments-list)
	 (mapcan #'(lambda (argument)
		     (when (pass-by-reference-p argument)
		       (let ((name (argument-name argument))
			     (type (argument-type argument))
			     (reference (argument-reference argument)))
			 (cond
			  ((eq type :string)
			   #-(version>= 6)
			   ;; 2001-02-08 HK: ACL 5:
			   `((set-sequence-length
			      ,name
			      (position #\Null ,name)))
			   #+(version>= 6)
			   ;; 2001-02-08 HK: ACL 6:
			   (let ((string-buffer (argument-buffer argument)))
			     `((progn
				 (setf ,name
				   (excl:native-to-string
				    ,string-buffer :string ,name))
				 (setf ,name
				   (set-sequence-length
				    ,name
				    (excl::char*-string-length
				     ,string-buffer)))
				 (excl:aclfree ,string-buffer)
				 ,name))))
			  ((and #-(version>= 6)
				;; 2001-02-08 HK: ACL 5:
				t
				#+(version>= 6)
				;; 2001-02-08 HK: ACL 6:
				(not (eq type :lisp))
				(eq reference :value-out))
			   `((aref ,name 0)))
			  (t (list name))))))
		 arguments-list)))
    
    (let ((set-arguments (map-set-foreign-arguments function-args))
	  (let-arguments (map-let-foreign-arguments function-args))
	  (return-values (map-return-values function-args)))
      (if (or set-arguments let-arguments return-values)
	  (let* ((c-result (unless (eq result-type :void)
			     (gensym-with-prefix 'c-result)))
		 (lisp-stub-function-name
		  (gensym-with-prefix lisp-function-name))
		 (call-stub-expr `(,lisp-stub-function-name
				   ,@(map 'list
				       #'(lambda (argument)
					   (let ((buffer (argument-buffer
							  argument)))
					     (if buffer
						 buffer
					       (argument-name argument))))
				       function-args)))
		 (lisp-call-stub-expr
		  (if (or let-arguments c-result)
		      `((let* (,@let-arguments
			       ,@(if c-result
				     `((,c-result ,call-stub-expr))))
			  ,@(unless c-result (list call-stub-expr))
			  (values ,@(when c-result (list c-result))
				  ,@return-values)))
		    `(,call-stub-expr
		      (values ,@return-values)))))
	    `(progn
	       (eval-when (:compile-toplevel :load-toplevel :execute)
		 ,(make-foreign-function result-type c-function-name
					 lisp-stub-function-name
					 function-args))
	       (defun ,lisp-function-name
		      ,(ff-map-extract-arguments function-args)
		 ,@set-arguments
		 ,@lisp-call-stub-expr)))
	(make-foreign-function result-type c-function-name
			       lisp-function-name function-args)))))

#+(and :allegro (version>= 5))
(defmacro define-foreign-callable
    (result-type c-function-name lisp-function-name function-args)
  (let ((lisp-stub-function-name (gensym-with-prefix lisp-function-name)))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
	 (ff:defun-foreign-callable ,lisp-stub-function-name
	     ,(map-foreign-arguments function-args)
	   (,lisp-function-name
	    ,@(ff-map-extract-arguments function-args))))
       (register-c-callable ,c-function-name
			    (ff:register-foreign-callable
			     (quote ,lisp-stub-function-name))))))

#+:lispworks3
(defmacro define-foreign-function
    (result-type c-function-name lisp-function-name function-args)
  (labels
      (
       (map-reference
        (raw-reference)
        (ecase raw-reference
          (:value-out :reference-return)
          ((:value-in :vector-out :vector-in) nil)))

       (make-foreign-function
        (result-type c-function-name lisp-function-name arguments-list)
        `(foreign:define-foreign-function
          (,lisp-function-name ,c-function-name)
          ,(map 'list
                #'(lambda (argument)
                    (let ((name (argument-name argument))
                          (type (argument-type argument))
                          (reference
                           (map-reference (argument-reference argument))))
                      `(,name ,type ,@(when reference
                                        (list reference)))))
                arguments-list)
          :result-type ,result-type
          :language :ansi-c))

       (requires-out-handling-p
        (argument)
        (let ((type (argument-type argument))
              (reference (argument-reference argument)))
          (and (eq type :string)
               (eq reference :vector-out))))

       (map-extract-setq-args
        (arguments-list)
	(mapcan #'(lambda (argument)
                    (let ((name (argument-name argument))
                          (reference (argument-reference argument)))
                      (when (eq reference :value-out)
                        (list name))))
		arguments-list))

       (map-extract-return-args
        (arguments-list)
        (let* ((requires-out-handling nil)
               (argument-name-alist
                (mapcan #'(lambda (argument)
                            (when (requires-out-handling-p argument)
                              (setf requires-out-handling t))
                            (when (pass-by-reference-p argument)
                              (let ((name (argument-name argument)))
                                `(,(cons name (gensym-with-prefix name))))))
                        arguments-list)))
          (when requires-out-handling
            argument-name-alist)))

       (map-extract-out-args
        (arguments-list argument-name-alist)
         (mapcan #'(lambda (argument-from-alist)
                     (let* ((argument (assoc (car argument-from-alist)
					     arguments-list))
		            (name (argument-name argument))
		            (reference (argument-reference argument)))
		       (when (eq reference :value-out)
		         (list (cdr argument-from-alist)))))
             argument-name-alist))

       (map-return-values
        (arguments-list argument-name-alist)
         (map 'list
             #'(lambda (argument-from-alist)
                 (let ((argument (assoc (car argument-from-alist)
					arguments-list)))
                   (if (requires-out-handling-p argument)
                       (let ((name (argument-name argument)))
                         `(set-sequence-length
			   ,name (position #\Null ,name)))
                     (cdr argument-from-alist))))
             argument-name-alist))
       )
    (let ((argument-name-alist (map-extract-return-args function-args)))
      (if argument-name-alist
	  (let* ((c-result (when result-type
			     (gensym-with-prefix 'c-result)))
		 (lisp-stub-function-name
                  (gensym-with-prefix lisp-function-name))
		 (call-stub-expr `(,lisp-stub-function-name
				   ,@(ff-map-extract-arguments
                                      function-args)))
                 (out-args (map-extract-out-args
                            function-args argument-name-alist))
		 (lisp-call-stub-expr
		  (if c-result
		      `((multiple-value-bind
                            (,c-result ,@out-args)
                            ,call-stub-expr
			  (values ,c-result
			          ,@(map-return-values
				     function-args argument-name-alist))))
		      `((multiple-value-bind
                            (,@out-args)
                            ,call-stub-expr
			  (values ,@(map-return-values
                                     function-args argument-name-alist)))))))
	    `(progn
               ,(make-foreign-function result-type c-function-name
                                       lisp-stub-function-name
                                       function-args)
	       (defun ,lisp-function-name
		   ,(ff-map-extract-arguments function-args)
		 ,@lisp-call-stub-expr)))
	(make-foreign-function result-type c-function-name
			       lisp-function-name function-args)))))

#+:lispworks3
(defmacro define-foreign-callable
    (result-type c-function-name lisp-function-name function-args)
  `(progn
     (foreign:foreign-callable
      ,lisp-function-name
      ,(map 'list #'argument-type function-args)
      :result-type ,result-type
      :foreign-name ,c-function-name)
     (register-c-callable ,c-function-name
                          (foreign:foreign-symbol-address ,c-function-name))))

#+(and :lispworks4 (not :win32))
(defconstant +max-ffi-string-length+ 1024
  "Maximum length of a LispWorks 4 string return parameter.")

#+:lispworks4
(defmacro define-foreign-function
    (result-type c-function-name lisp-function-name function-args)

  (labels
      (
       (map-foreign-arguments
        (arguments-list)
        (map 'list
             #'(lambda (argument)
                 (let ((name (argument-name argument))
                       (type (argument-type argument))
                       (reference (argument-reference argument)))
                   (case type
                     (:const-c-string
                      ;; Pass a const C string always as w:lpcstr:
                      #+:win32 `(,name w:lpcstr)
                      #-:win32 `(,name (:reference
                                        (:ef-mb-string :external-format :ascii)
                                        :lisp-to-foreign-p t
                                        :foreign-to-lisp-p nil))
                      )
                     (:c-string
                      `(,name ,@(ecase reference
                                  ((:vector-in)
                                   #+:win32 `(w:lpcstr)
                                   #-:win32 '((:reference
                                               (:ef-mb-string
                                                :external-format :ascii)
                                               :lisp-to-foreign-p t
                                               :foreign-to-lisp-p nil))
                                   )
                                  ((:vector-out)
                                   #+:win32 `(w:lpstr)
                                   #-:win32 '((:reference
                                               (:ef-mb-string
                                                :external-format :ascii
                                                :limit +max-ffi-string-length+)
                                               :lisp-to-foreign-p nil
                                               :foreign-to-lisp-p t))
                                   ))))
                     ((:pointer :as-is)
                      ;; Pass pointers always by value:
                      `(,name ,type))
                     (t (ecase reference
                          (:value-in `(,name ,type))
                          (:value-out `(,name (:reference-return ,type))))))))
             arguments-list))

       (make-foreign-function
        (result-type c-function-name lisp-function-name arguments-list)
        `(fli:define-foreign-function
             (,lisp-function-name ,c-function-name)
             ,(map-foreign-arguments arguments-list)
           :calling-convention :cdecl
           :language :ansi-c
           :result-type ,result-type))

       (requires-out-handling-p
        (argument)
        (let ((type (argument-type argument))
              (reference (argument-reference argument)))
          (and (eq type :c-string)
               (eq reference :vector-out))))

       (map-extract-return-args
        (arguments-list)
        (let* ((requires-out-handling nil)
               (argument-name-alist
                (mapcan #'(lambda (argument)
                            (when (requires-out-handling-p argument)
                              (setf requires-out-handling t))
                            (when (pass-by-reference-p argument)
                              (let ((name (argument-name argument)))
                                `(,(cons name (gensym-with-prefix name))))))
                        arguments-list)))
          (when requires-out-handling
            argument-name-alist)))

       (map-return-values
        (arguments-list argument-name-alist)
        (mapcan #'(lambda (argument-from-alist)
                    (let ((argument (assoc (car argument-from-alist)
					   arguments-list)))
                      (cond
                       ((requires-out-handling-p argument)
                        (let ((name (argument-name argument)))
                          `((progn
                              (replace ,name ,(cdr argument-from-alist))
                              (set-sequence-length
                               ,name
                               (length ,(cdr argument-from-alist)))))))
                       ((pass-by-reference-p argument)
                        (list (cdr argument-from-alist))))))
             argument-name-alist))
       )
    (let ((argument-name-alist (map-extract-return-args function-args)))
      (if argument-name-alist
	  (let* ((c-result (when result-type
			     (gensym-with-prefix 'c-result)))
		 (lisp-stub-function-name
                  (gensym-with-prefix lisp-function-name))
		 (call-stub-expr `(,lisp-stub-function-name
				   ,@(ff-map-extract-arguments
                                      function-args)))
                 (out-args (map 'list #'cdr argument-name-alist))
		 (lisp-call-stub-expr
		  (if c-result
		      `((multiple-value-bind
                            (,c-result ,@out-args)
                            ,call-stub-expr
			  (values ,c-result
                                  ,@(map-return-values
                                     function-args argument-name-alist))))
		      `((multiple-value-bind
                            (,@out-args)
                            ,call-stub-expr
			  (values ,@(map-return-values
                                     function-args argument-name-alist)))))))
	    `(progn
               ,(make-foreign-function result-type c-function-name
                                       lisp-stub-function-name
                                       function-args)
	       (defun ,lisp-function-name
		   ,(ff-map-extract-arguments function-args)
		 ,@lisp-call-stub-expr)))
	(make-foreign-function result-type c-function-name
			       lisp-function-name function-args)))))

#+:lispworks4
(defmacro define-foreign-callable
    (result-type c-function-name lisp-function-name function-args)
  (labels
      (
       (map-foreign-arguments
        (arguments-list)
        (map 'list
             #'(lambda (argument)
                 (let ((name (argument-name argument))
                       (type (argument-type argument))
                       (reference (argument-reference argument)))
                   (case type
                     (:const-c-string
                      ;; Pass a const C string always as w:lpcstr:
                      #+:win32 `(,name (:reference-return w:str))
                      #-:win32 `(,name (:reference
                                        (:ef-mb-string
                                         :external-format :ascii
                                         :limit +max-ffi-string-length+)
                                        :lisp-to-foreign-p nil
                                        :foreign-to-lisp-p t))
                      )
                     (:c-string
                      `(,name ,@(ecase reference
                                  ((:vector-in)
                                   #+:win32 `(w:lpcstr)
                                   #-:win32 '((:reference
                                               (:ef-mb-string
                                                :external-format :ascii
                                                :limit +max-ffi-string-length+)
                                               :lisp-to-foreign-p nil
                                               :foreign-to-lisp-p t))
                                   )
                                  ((:vector-out)
                                   #+:win32 `(w:lpstr)
                                   #-:win32 '((:reference
                                               (:ef-mb-string
                                                :external-format :ascii
                                                :limit +max-ffi-string-length+)
                                               :lisp-to-foreign-p t
                                               :foreign-to-lisp-p nil))
                                   ))))
                     ((:pointer :as-is)
                      ;; Pass pointers always by value:
                      `(,name ,type))
                     (t (ecase reference
                          (:value-in `(,name ,type))
                          (:value-out `(,name (:reference-return ,type))))))))
             arguments-list))
       )
    (let ((c-stub-function-name (gensym-with-prefix lisp-function-name)))
      `(progn
         (fli:define-foreign-callable
             (,(symbol-name c-stub-function-name)
              :calling-convention :cdecl
              :language :ansi-c
              :result-type ,result-type
              )
             ,(map-foreign-arguments function-args)
           (funcall (function ,lisp-function-name)
                    ,@(ff-map-extract-arguments function-args)))
         (register-c-callable
          ,c-function-name
          (fli:make-pointer :symbol-name
                            ,(symbol-name c-stub-function-name)))))))

(defvar *lib-plob-loaded* nil
   "Flag if the libraries are already loaded. Is set by \\fcite{sh-load}.")

(defconstant +lib-rpc-client-plob+
  "PLOB:CLIBS;librpclientplob.so"
  "Logical pathname of RPC client library.")

(defconstant +lib-local-client-plob+
  "PLOB:CLIBS;libloclientplob.so"
  "Logical pathname of local client library.")

(defun sh-load (&optional (lib +lib-local-client-plob+))
  "Load the \\plob\\ client code object files.
\\Remarkslabel
  HK 1996/10/24: Strange, loading \\lisp{librpclientplob.a}\\ will
  result in undefined references signalled by \\lispworks;
  loading each single object code file will do its work.

  Even more strange, \\lispworks\\ doesn't like the underscore
  character in \\lisp{.o}\\ object file names, since these are
  expected to obey the rules for logical-filenames. A rather tough
  requirement for non-LISP files \\ldots

  HK 1997/03/07: The C compiler must not have been called with
  the \\lisp{-fPIC}\\ option (for \\lisp{gcc}) resp.\\ the
  \\lisp{-KPIC}\\ option (for Sun \lisp{cc}) to build the
  object code, since \\lispworks\\ won't load such object code."

  (cond
   ((and *lib-plob-loaded* (not (equal lib *lib-plob-loaded*)))
    (cerror "Use the already loaded library."
	    "Request to load library ~a with an already loaded library ~a."
	    lib *lib-plob-loaded*))
   (*lib-plob-loaded* nil)
   (t 
    (when (and *verbose* (>= *verbose* 9))
      (format t "; Start loading foreign library ~A ...~%" lib))
    #+:lispworks4
    (fli:register-module :plob-dll
			 :real-name
			 (translate-logical-pathname lib)
			 :connection-style :automatic)
    #+:lispworks3
    (foreign:read-foreign-modules (translate-logical-pathname lib))
    #+:allegro
    (load (translate-logical-pathname lib))
    (setf *lib-plob-loaded* lib)
    (when (and *verbose* (>= *verbose* 9))
      (format t "; Finished loading foreign library ~A~%" lib))
    (when (and *verbose* (>= *verbose* 9))
      (format t "; Start registering c-callables ...~%"))
    (when (> (hash-table-count *ff-name-to-entry-table*) 0)
      (if (and *verbose* (>= *verbose* 9))
	  (maphash #'(lambda (key value)
		       (format t "; Registering c-callable ~A with address 0x~X ...~%"
			       key value)
		       (register-c-callable-to-c key value)
		       (format t "; Registered c-callable ~A with address 0x~X~%"
			       key value))
		   *ff-name-to-entry-table*)
	  (maphash #'register-c-callable-to-c *ff-name-to-entry-table*)))
    (when (and *verbose* (>= *verbose* 9))
      (format t "; Finished registering c-callables.~%")))))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
