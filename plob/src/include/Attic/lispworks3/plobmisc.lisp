
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "Aug 25 1999" "05:10:16"
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------


  (in-package  :plob)

;;;; --------------------------------------------------------------------------
;;;; For further comments look into file plob.h
;;;; --------------------------------------------------------------------------


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+c-false+")
           	  0
           	  "not-T for C."))     ;

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+c-true+")
           	  1
           	  "T for C."))     ;


  (define-foreign-function
            #.:fixnum
           	"fnShiftLeftAndSet"
           	 #.(read-from-string    "c-short-float-to-fixnum")
      
		 ( (wObject    #.:as-is            :vector-in)
             

		   (nShiftLeft    #.:fixnum           :value-in)
             

		   (wOrMask    #.:fixnum           :value-in)
                   ))    ;


  (define-foreign-function
            #.:as-is
           	"fnShiftLeftAndSet"
           	 #.(read-from-string    "c-fixnum-to-short-float")
      
		 ( (wObject    #.:fixnum           :value-in)
             

		   (nShiftLeft    #.:fixnum           :value-in)
             

		   (wOrMask    #.:fixnum           :value-in)
                   ))    ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+max-url+")
           	  256
      
		  "Maximum length of an URL."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *DAYLIGHTSAVINGTIME*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *DAYLIGHTSAVINGTIME*)
          (let ((last-enum-hash-table   *DAYLIGHTSAVINGTIME*))
  (setf  (gethash     0     last-enum-hash-table)         "not on dst")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-none+")
           	   0
           	   "not on dst"))

  (setf  (gethash     1     last-enum-hash-table)         "USA style dst")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-usa+")
           	   1
           	   "USA style dst"))

  (setf  (gethash     2     last-enum-hash-table)         "Australian style dst")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-aust+")
           	   2
           	   "Australian style dst"))

  (setf  (gethash     3     last-enum-hash-table)         "Western European dst")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-wet+")
           	   3
           	   "Western European dst"))

  (setf  (gethash     4     last-enum-hash-table)         "Middle European dst")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-met+")
           	   4
           	   "Middle European dst"))

  (setf  (gethash     5     last-enum-hash-table)         "Eastern European dst")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-Eet+")
           	   5
           	   "Eastern European dst"))

  (setf  (gethash     6     last-enum-hash-table)         "Canada")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-can+")
           	   6
           	   "Canada"))

  (setf  (gethash     7     last-enum-hash-table)         "Great Britain and Eire")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-gb+")
           	   7
           	   "Great Britain and Eire"))

  (setf  (gethash     8     last-enum-hash-table)         "Rumania")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-rum+")
           	   8
           	   "Rumania"))

  (setf  (gethash     9     last-enum-hash-table)         "Turkey")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-tur+")
           	   9
           	   "Turkey"))

  (setf  (gethash     10     last-enum-hash-table)
               "Australian style with shift in 1986")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-dst-aust-alt+")
           	   10
           	                  "Australian style with shift in 1986"))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_DAYLIGHTSAVINGTIME
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *DEPENDENTMODE*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *DEPENDENTMODE*)
          (let ((last-enum-hash-table   *DEPENDENTMODE*))
  (setf  (gethash
	       -1      last-enum-hash-table)
	       "Get the current dependent flag.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flag-dependent-get+")
           	   	       -1
           	   	       "Get the current dependent flag."))

  (setf  (gethash
	       0      last-enum-hash-table)
	       "Unset the dependent flag.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flag-dependent-none+")
           	   	       0
           	   	       "Unset the dependent flag."))

  (setf  (gethash
	       (parse-integer  "04" :radix    16)          last-enum-hash-table)
	       "Set the dependent flag to read.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flag-dependent-read+")
           	   	       (parse-integer  "04" :radix    16)
           	   	       "Set the dependent flag to read."))

  (setf  (gethash
	       (parse-integer  "08" :radix    16)          last-enum-hash-table)
	       "Set the dependent flag to read.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flag-dependent-write+")
           	   	       (parse-integer  "08" :radix    16)
           	   	       "Set the dependent flag to read."))

  (setf  (gethash
	       (logior  (parse-integer  "04" :radix    16)        (parse-integer  "08" :radix    16))         last-enum-hash-table)
	       "Set the dependent flag to read-write.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flag-dependent-read-write+")
           	   	       (logior  (parse-integer  "04" :radix    16)        (parse-integer  "08" :radix    16))
           	   	       "Set the dependent flag to read-write."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_DEPENDENTMODE
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.C2L_DEPENDENTMODE
           	"fnShortMakeDependent"
           	 #.(read-from-string    "c-sh-dependent")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortSelf    #.C2L_SHORTOBJID      :value-in)
             

		   (nDependentMode    #.C2L_DEPENDENTMODE      :value-in)
                   ))    ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *NUMERICSTDSTREAM*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *NUMERICSTDSTREAM*)
          (let ((last-enum-hash-table   *NUMERICSTDSTREAM*))
  (setf  (gethash     0     last-enum-hash-table)
	       "Tag for PLOB C stream stdin.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-numeric-stdin+")
           	   0
           	   	       "Tag for PLOB C stream stdin."))

  (setf  (gethash     1     last-enum-hash-table)
	       "Tag for PLOB C stream stdin.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-numeric-stdout+")
           	   1
           	   	       "Tag for PLOB C stream stdin."))

  (setf  (gethash     2     last-enum-hash-table)
	       "Tag for PLOB C stream stderr.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+c-numeric-stderr+")
           	   2
           	   	       "Tag for PLOB C stream stderr."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_NUMERICSTDSTREAM
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *FLUSHMODE*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *FLUSHMODE*)
          (let ((last-enum-hash-table   *FLUSHMODE*))
  (setf  (gethash    		-1     last-enum-hash-table)
	       "Don't change current flush mode; just return current mode.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flush-get-mode+")
           	  		-1
           	   	       "Don't change current flush mode; just return current mode."))

  (setf  (gethash    		0      last-enum-hash-table)
	       "Flush never (mostly unsecure,but fastest possible mode).")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flush-never+")
           	  		0
           	   	       "Flush never (mostly unsecure,but fastest possible mode)."))

  (setf  (gethash     		1      last-enum-hash-table)
	       "Flush seldom.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flush-seldom+")
           	   		1
           	   	       "Flush seldom."))

  (setf  (gethash     	2      last-enum-hash-table)
	       "Flush sometimes.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flush-sometimes+")
           	   	2
           	   	       "Flush sometimes."))

  (setf  (gethash    		3      last-enum-hash-table)
	       "Flush often.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flush-often+")
           	  		3
           	   	       "Flush often."))

  (setf  (gethash    		4      last-enum-hash-table)
	       "Flush always (mostly secure,but slowest possible mode).")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flush-always+")
           	  		4
           	   	       "Flush always (mostly secure,but slowest possible mode)."))

  (setf  (gethash    	0      last-enum-hash-table)
	       "The default flush mode.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flush-default-mode+")
           	  	0
           	   	       "The default flush mode."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_FLUSHMODE
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.C2L_FLUSHMODE
           	"fnFlushMode"
           	 #.(read-from-string    "c-sh-flush-mode")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (nMode    #.C2L_FLUSHMODE      :value-in)
                   ))    ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *FLAGMODE*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *FLAGMODE*)
          (let ((last-enum-hash-table   *FLAGMODE*))
  (setf  (gethash    			-1     last-enum-hash-table)
	       "Don't change current flag; just return current value.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flag-get+")
           	  			-1
           	   	       "Don't change current flag; just return current value."))

  (setf  (gethash    			 0     last-enum-hash-table)
	       "Set flag value")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+flag-set+")
           	  			 0
           	   	       "Set flag value"))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_FLAGMODE
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.:fixnum
           	"fnFlagWord"
           	 #.(read-from-string    "c-sh-flag-word")
      
		 ( (nGetOrSet    #.C2L_FLAGMODE      :value-in)
             

		   (nFlagWord    #.:fixnum           :value-in)
                   ))    ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+match-any+")
           	  -1
      
		 "Match-any value for the component of a host address."))     ;

  (define-foreign-function
            #.C2L_BOOL
           	"fnGetHostAddr"
           	 #.(read-from-string    "c-sh-get-host-addr")
      
		 ( (szHost    #.:string           :vector-in)
             

		   (pnAddr    #.:simple-vector            :vector-out)
                   ))    ;


  (define-foreign-function
            #.:integer
           	"fnLISPmalloc"
           	 #.(read-from-string    "c-malloc")
      
		 ( (nSizeInBytes    #.:fixnum           :value-in)
                   ))    ;


  (define-foreign-function
            #.nil
           	"fnLISPfree"
           	 #.(read-from-string    "c-free")
      
		 ( (pMemory    #.:integer           :value-in)
                   ))    ;

