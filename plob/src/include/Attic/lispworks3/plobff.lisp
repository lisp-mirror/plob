
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "Aug 25 1999" "05:10:19"
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------


  (in-package  :plob)

;;;; --------------------------------------------------------------------------
;;;; For further comments look into file cplobff.h
;;;; --------------------------------------------------------------------------


  (define-foreign-function
            #.nil
           	"fnRegisterCcallable"
           	 #.(read-from-string    "register-c-callable-to-c")
      
		 ( (lpszFunctionName    #.:string           :vector-in)
             

		   (lpfnFunctionCode    #.:integer           :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnGetErrorMessage"
           	 #.(read-from-string    "sh-get-error-message")
      
		 ( (pszBuffer    #.:string          :vector-out)
             

		   (nBuffer    #.:fixnum           :value-in)
                   ))    ;

  (define-foreign-function
            #.:fixnum
           	"fnGetErrorContinue"
           	 #.(read-from-string    "sh-get-error-continue")
      
		 ( (pszBuffer    #.:string          :vector-out)
             

		   (nBuffer    #.:fixnum           :value-in)
                   ))    ;

  (define-foreign-function
            #.:fixnum
           	"fnGetErrorLevel"
           	 #.(read-from-string    "sh-get-error-level")
      
		 (   ))    ;


  (define-foreign-callable
            #.nil
           	"fnLISPerrorCallback"
           	 #.(read-from-string    "sh-error-callback")
      
		 ( (eLevel    #.C2L_ERRLVL      :value-in)
             

		   (pszContinue    #.:string           :vector-in)
             

		   (pszErrorMsg    #.:string           :vector-in)
                   ))    ;
