
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "May 22 2000" "13:23:37"
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------


  (in-package  :plob)

;;;; --------------------------------------------------------------------------
;;;; For further comments look into file plobroot.h
;;;; --------------------------------------------------------------------------

(defparameter +plob-version+
              (parse-integer  "209"    :radix    10)
              "PLOB version number")


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+root-type-tag+")
           	  (parse-integer  "88" :radix    16)
      
		 "Type tag for plob objects of type root."))     ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+user-type-tag+")
           	  (parse-integer  "100" :radix    16)
      
		 "Type tag for plob objects of type user."))     ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+machine-type-tag+")
           	  (parse-integer  "110" :radix    16)
      
		 "Type tag for plob objects of type machine."))     ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbReadRoot"
           	 #.(read-from-string    "c-sh-read-root")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbWriteRoot"
           	 #.(read-from-string    "c-sh-write-root")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjId    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbSessions"
           	 #.(read-from-string    "c-sh-sessions")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *GETVERSION*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *GETVERSION*)
          (let ((last-enum-hash-table   *GETVERSION*))
  (setf  (gethash     1     last-enum-hash-table)
	       "Get the version number of the currently opened database.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+get-database-version+")
           	   1
           	   	       "Get the version number of the currently opened database."))

  (setf  (gethash     2     last-enum-hash-table)
	       "Get the version number of the server's code.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+get-server-code-version+")
           	   2
           	   	       "Get the version number of the server's code."))

  (setf  (gethash     3     last-enum-hash-table)
	       "Get the version number of the client's C code.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+get-client-c-code-version+")
           	   3
           	   	       "Get the version number of the client's C code."))

  (setf  (gethash     4     last-enum-hash-table)
	       "Get the version number of the client's LISP code.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+get-client-lisp-code-version+")
           	   4
           	   	       "Get the version number of the client's LISP code."))

)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_GETVERSION
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.:fixnum
           	"fnClientGetVersion"
           	 #.(read-from-string    "c-sh-get-version")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (eWhat    #.C2L_GETVERSION      :value-in)
                   ))    ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *MACHLOGINP*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *MACHLOGINP*)
          (let ((last-enum-hash-table   *MACHLOGINP*))
  (setf  (gethash     0     last-enum-hash-table)
	       "Leave it unspecified if the login from the machine is allowed or denied.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+login-ignore+")
           	   0
           	   	       "Leave it unspecified if the login from the machine is allowed or denied."))

  (setf  (gethash     1     last-enum-hash-table)
	       "Allow logins from machine.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+login-allow+")
           	   1
           	   	       "Allow logins from machine."))

  (setf  (gethash     2     last-enum-hash-table)
	       "Deny logins from machine.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+login-deny+")
           	   2
           	   	       "Deny logins from machine."))

  (setf  (gethash     3     last-enum-hash-table)
	       "Get login allow/deny flag of machine.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+login-get-flag+")
           	   3
           	   	       "Get login allow/deny flag of machine."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_MACHLOGINP
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbCreateMachine"
           	 #.(read-from-string    "c-sh-create-machine")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (nAddr    #.:fixnum              :vector-in)
             

		   (eLoginP    #.C2L_MACHLOGINP      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_MACHLOGINP
           	"fnClientMachineLoginP"
           	 #.(read-from-string    "c-sh-machine-loginp")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortMachine    #.C2L_SHORTOBJID      :value-in)
             

		   (eLoginP    #.C2L_MACHLOGINP      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_BOOL
           	"fnClientMachineAddr"
           	 #.(read-from-string    "c-sh-machine-addr")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortMachine    #.C2L_SHORTOBJID      :value-in)
             

		   (
			      pnAddr    #.:fixnum              :vector-out)
                   ))    ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbMachineSearch"
           	 #.(read-from-string    "c-sh-search-machine")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (nAddr    #.:fixnum              :vector-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbMachineDelete"
           	 #.(read-from-string    "c-sh-delete-machine")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (nAddr    #.:fixnum              :vector-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbMachineInsert"
           	 #.(read-from-string    "c-sh-insert-machine")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortMachine    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbMachines"
           	 #.(read-from-string    "c-sh-machines")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *LISPROOTIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *LISPROOTIDX*)
          (let ((last-enum-hash-table   *LISPROOTIDX*))
  (setf  (gethash
	       (+   1     0)        last-enum-hash-table)
	       "Index into LISP root object containing the version number actual when the root was formatted. It is a fixnum.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-version+")
           	   	       (+   1     0)
           	   	       "Index into LISP root object containing the version number actual when the root was formatted. It is a fixnum."))

  (setf  (gethash
	       (+   1     1)        last-enum-hash-table)
	       "Index into LISP root object containing the LISP system which formatted the root. It is a persistent keyword symbol.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-lisp-formatted+")
           	   	       (+   1     1)
           	   	       "Index into LISP root object containing the LISP system which formatted the root. It is a persistent keyword symbol."))

  (setf  (gethash
	       (+   1     2)        last-enum-hash-table)
	       "Index into LISP root object containing the time the LISP root was formatted. Its format is Common LISP Universal Time,i.e. a bignum.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-time-formatted+")
           	   	       (+   1     2)
           	   	       "Index into LISP root object containing the time the LISP root was formatted. Its format is Common LISP Universal Time,i.e. a bignum."))

  (setf  (gethash
	       (+   1     3)        last-enum-hash-table)
	       "Index into LISP root object containing the table mapping names (i.e. strings) to package objects.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-name->package-table+")
           	   	       (+   1     3)
           	   	       "Index into LISP root object containing the table mapping names (i.e. strings) to package objects."))

  (setf  (gethash
	       (+   1     4)        last-enum-hash-table)
	       "Index into LISP root object containing the table mapping symbols to structure resp. class descriptions.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-symbol->class-table+")
           	   	       (+   1     4)
           	   	       "Index into LISP root object containing the table mapping symbols to structure resp. class descriptions."))

  (setf  (gethash
	       (+   1     5)        last-enum-hash-table)
	       "Index into LISP root object containing the structure description object.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-structure-description+")
           	   	       (+   1     5)
           	   	       "Index into LISP root object containing the structure description object."))

  (setf  (gethash
	       (+   1     6)        last-enum-hash-table)
	       "Index into LISP root object containing the structure slot description object.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-structure-slot-description+")
           	   	       (+   1     6)
           	   	       "Index into LISP root object containing the structure slot description object."))

  (setf  (gethash
	       (+   1     7)        last-enum-hash-table)
	       "Index into LISP root object containing the package description object.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-package-description+")
           	   	       (+   1     7)
           	   	       "Index into LISP root object containing the package description object."))

  (setf  (gethash
	       (+   1     8)        last-enum-hash-table)
	       "Index into LISP root object containing the plob description object.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-plob-description+")
           	   	       (+   1     8)
           	   	       "Index into LISP root object containing the plob description object."))

  (setf  (gethash
	       (+   1     9)        last-enum-hash-table)
	      "Index into LISP root object containing the class description object.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-class-description+")
           	   	       (+   1     9)
           	   	      "Index into LISP root object containing the class description object."))

  (setf  (gethash
	       (+   1     10)        last-enum-hash-table)
	       "Index into LISP root object containing the slot description object.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-slot-description+")
           	   	       (+   1     10)
           	   	       "Index into LISP root object containing the slot description object."))

  (setf  (gethash
	       (+   1     11)        last-enum-hash-table)
	       "Index into LISP root object containing the direct slot description object.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-direct-slot-description+")
           	   	       (+   1     11)
           	   	       "Index into LISP root object containing the direct slot description object."))

  (setf  (gethash
	       (+   1     12)        last-enum-hash-table)
	       "Index into LISP root object containing the effective slot description object.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-effective-slot-description+")
           	   	       (+   1     12)
           	   	       "Index into LISP root object containing the effective slot description object."))

  (setf  (gethash
	       (+   1     13)        last-enum-hash-table)
	       "Index into LISP root object containing the method description object.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-method-description+")
           	   	       (+   1     13)
           	   	       "Index into LISP root object containing the method description object."))

  (setf  (gethash
	       (+   1     14)        last-enum-hash-table)
	       "Index into LISP root object containing the Persistent File System.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-location-pfs+")
           	   	       (+   1     14)
           	   	       "Index into LISP root object containing the Persistent File System."))

  (setf  (gethash
	       (+   1     15)        last-enum-hash-table)
	       "Size of LISP root vector.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+root-size+")
           	   	       (+   1     15)
           	   	       "Size of LISP root vector."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_LISPROOTIDX
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;

