
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "Feb 13 2001" "10:36:16"
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------


(in-package  :plob)

;;;; -------------------------------------------------------------------------
;;;; For further comments look into file plobheap.h
;;;; -------------------------------------------------------------------------


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_TRACTID
           	:fixnum
           	"Constant defined by C macro DefineType."))     ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+null-transaction-id+")
           	  0
      
		 "NULL (i.e. always invalid) transaction-id."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHHEAPIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHHEAPIDX*)
          (let ((last-enum-hash-table   *SHHEAPIDX*))
  (setf  (gethash     0     last-enum-hash-table)
	       "Index of plob heap heap field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+heap-location-self+")
           	   0
           	   	       "Index of plob heap heap field."))

  (setf  (gethash     1     last-enum-hash-table)
	       "Index of plob heap transaction id field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+heap-location-tract-id+")
           	   1
           	   	       "Index of plob heap transaction id field."))

  (setf  (gethash     2     last-enum-hash-table)
	       "Index of plob heap transaction log id field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+heap-location-tract-log-id+")
           	   2
           	   	       "Index of plob heap transaction log id field."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHHEAPIDX
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+heap-type-tag+")
           	  (parse-integer  "90" :radix    16)
      
		 "Type tag for plob objects of type HEAP."))     ;


  (define-foreign-function
            #.C2L_TRACTID
           	"fnClientTransactionBegin"
           	 #.(read-from-string    "c-sh-begin-transaction")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (bIgnoreError    #.C2L_BOOL      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_TRACTID
           	"fnClientTransactionCancel"
           	 #.(read-from-string    "c-sh-cancel-transaction")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (bIgnoreError    #.C2L_BOOL      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_TRACTID
           	"fnClientTransactionEnd"
           	 #.(read-from-string    "c-sh-end-transaction")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (bIgnoreError    #.C2L_BOOL      :value-in)
                   ))    ;


  (define-foreign-function
            #.:void
           	"fnClientTransactionFlush"
           	 #.(read-from-string    "c-sh-flush-transaction")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_TRACTID
           	"fnClientDbTransactionP"
           	 #.(read-from-string    "c-sh-in-transaction-p")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (nTractId    #.C2L_TRACTID      :value-in)
                   ))    ;

