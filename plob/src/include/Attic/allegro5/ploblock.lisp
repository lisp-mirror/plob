
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "Aug 25 1999" "05:12:17"
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------


  (in-package  :plob)

;;;; --------------------------------------------------------------------------
;;;; For further comments look into file ploblock.h
;;;; --------------------------------------------------------------------------


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+sum-lock-tag+")
           	  (parse-integer  "A8" :radix    16)
      
		 "Type tag for plob objects of type summarizing lock."))     ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+one-lock-tag+")
           	  (parse-integer  "B0" :radix    16)
      
		 "Type tag for plob objects of type single lock."))     ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+queue-entry-tag+")
           	  (parse-integer  "D0" :radix    16)
      
		 "Type tag for plob objects of type queue entry."))     ;


  (define-foreign-function
            #.:int
           	"fnClientLockPrint"
           	 #.(read-from-string    "c-sh-lock-print")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortSelf    #.C2L_SHORTOBJID      :value-in)
             

		   (nLevel    #.C2L_SHLOCK      :value-in)
             

		   (nStdStream    #.C2L_NUMERICSTDSTREAM      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientTransactionLockGet"
           	 #.(read-from-string    "c-sh-get-lock")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdLockedByP    #.C2L_SHORTOBJID      :value-in)
             

		   (nLevelP    #.C2L_SHLOCK      :value-in)
             

		   (oShortLockedP    #.C2L_SHORTOBJID      :value-in)
             

		   (nTypeTagLockedP    #.C2L_SHTYPETAG      :value-in)
             

		   (nIndexP    #.:int           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientTransactionLockInsert"
           	 #.(read-from-string    "c-sh-insert-lock")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdLockBy    #.C2L_SHORTOBJID      :value-in)
             

		   (nLock    #.C2L_SHLOCK      :value-in)
             

		   (oShortToLock    #.C2L_SHORTOBJID      :value-in)
             

		   (nTypeTagToLock    #.C2L_SHTYPETAG      :value-in)
             

		   (nIndex    #.:int           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientTransactionLockSet"
           	 #.(read-from-string    "c-sh-set-lock")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdLockBy    #.C2L_SHORTOBJID      :value-in)
             

		   (nLock    #.C2L_SHLOCK      :value-in)
             

		   (oShortToLock    #.C2L_SHORTOBJID      :value-in)
             

		   (nTypeTagToLock    #.C2L_SHTYPETAG      :value-in)
             

		   (nIndex    #.:int           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnShortUnlock"
           	 #.(read-from-string    "c-sh-unlock")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdLockedBy    #.C2L_SHORTOBJID      :value-in)
             

		   (nLock    #.C2L_SHLOCK      :value-in)
             

		   (oShortToUnlock    #.C2L_SHORTOBJID      :value-in)
             

		   (nIndex    #.:int           :value-in)
                   ))    ;


  (define-foreign-function
            #.:int
           	"fnShortUnlockAll"
           	 #.(read-from-string    "c-sh-unlock-all")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdLockedBy    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortToUnlock    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.:int
           	"fnShortUnlockAllAll"
           	 #.(read-from-string    "c-sh-unlock-all-all")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdLockedBy    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *READONLYMODE*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *READONLYMODE*)
          (let ((last-enum-hash-table   *READONLYMODE*))
  (setf  (gethash     -1     last-enum-hash-table)
	       "Get current read-only mode.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+read-only-p+")
           	   -1
           	   	       "Get current read-only mode."))

  (setf  (gethash     0     last-enum-hash-table)
	       "Set current mode of object to read-write.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+read-write+")
           	   0
           	   	       "Set current mode of object to read-write."))

  (setf  (gethash     1     last-enum-hash-table)
	       "Set current mode of object to read-only.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+read-only+")
           	   1
           	   	       "Set current mode of object to read-only."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_READONLYMODE
           	:int
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.C2L_BOOL
           	"fnShortMakeReadOnly"
           	 #.(read-from-string    "c-sh-read-only")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortSelf    #.C2L_SHORTOBJID      :value-in)
             

		   (nReadOnlyP    #.C2L_READONLYMODE      :value-in)
                   ))    ;

