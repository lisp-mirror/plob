
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "Feb 13 2001" "10:36:50"
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------


(in-package  :plob)

;;;; -------------------------------------------------------------------------
;;;; For further comments look into file cplobff.h
;;;; -------------------------------------------------------------------------


  (define-foreign-function
            #.:void
           	"fnRegisterCcallable"
           	 #.(read-from-string    "register-c-callable-to-c")
      
		 ( (lpszFunctionName    #.:string           :vector-in)
             

		   (lpfnFunctionCode    #.:int            :value-in)
                   ))    ;


  (define-foreign-function
            #.:int
           	"fnGetErrorMessage"
           	 #.(read-from-string    "sh-get-error-message")
      
		 ( (pszBuffer    #.:string          :vector-out)
             

		   (nBuffer    #.:int           :value-in)
                   ))    ;

  (define-foreign-function
            #.:int
           	"fnGetErrorContinue"
           	 #.(read-from-string    "sh-get-error-continue")
      
		 ( (pszBuffer    #.:string          :vector-out)
             

		   (nBuffer    #.:int           :value-in)
                   ))    ;

  (define-foreign-function
            #.:int
           	"fnGetErrorLevel"
           	 #.(read-from-string    "sh-get-error-level")
      
		 (   ))    ;


  (define-foreign-callable
            #.:void
           	"fnLISPerrorCallback"
           	 #.(read-from-string    "sh-error-callback")
      
		 (   ))    ;

