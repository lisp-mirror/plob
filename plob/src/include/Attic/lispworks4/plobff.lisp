
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "Aug 25 1999" "05:10:52"
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
      
		 ( (lpszFunctionName    #.:const-c-string           :vector-in)
             

		   (lpfnFunctionCode    #.:pointer           :value-in)
                   ))    ;


  (define-foreign-function
            #.:int
           	"fnGetErrorMessage"
           	 #.(read-from-string    "sh-get-error-message")
      
		 ( (pszBuffer    #.:c-string          :vector-out)
             

		   (nBuffer    #.:int           :value-in)
                   ))    ;

  (define-foreign-function
            #.:int
           	"fnGetErrorContinue"
           	 #.(read-from-string    "sh-get-error-continue")
      
		 ( (pszBuffer    #.:c-string          :vector-out)
             

		   (nBuffer    #.:int           :value-in)
                   ))    ;

  (define-foreign-function
            #.:int
           	"fnGetErrorLevel"
           	 #.(read-from-string    "sh-get-error-level")
      
		 (   ))    ;


  (define-foreign-callable
            #.nil
           	"fnLISPerrorCallback"
           	 #.(read-from-string    "sh-error-callback")
      
		 ( (eLevel    #.C2L_ERRLVL      :value-in)
             

		   (pszContinue    #.:const-c-string           :vector-in)
             

		   (pszErrorMsg    #.:const-c-string           :vector-in)
                   ))    ;

