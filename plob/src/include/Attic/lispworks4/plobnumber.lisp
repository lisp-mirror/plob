
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "Feb 14 2001" "11:38:38"
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------


(in-package  :plob)

;;;; -------------------------------------------------------------------------
;;;; For further comments look into file plobnumber.h
;;;; -------------------------------------------------------------------------


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
		 "+single-float-value-size+")
           	  1
      
		 "Size of a single float number in words."))     ;


  (define-foreign-function
            #.:double-float
           	"fnShortToSingleFloat"
           	 #.(read-from-string    "c-short-to-single-float")
      
		 ( (oShortFloat    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbMakeSingleFloat"
           	 #.(read-from-string    "c-sh-make-single-float")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (fFrom    #.:double-float           :value-in)
                   ))    ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
		 "+double-float-value-size+")
           	  2
      
		 "Size of a double float number in words."))     ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbMakeDoubleFloat"
           	 #.(read-from-string    "c-sh-make-double-float")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (fFrom    #.:double-float           :value-in)
                   ))    ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHRATIOIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHRATIOIDX*)
          (let ((last-enum-hash-table   *SHRATIOIDX*))
  (setf  (gethash     0     last-enum-hash-table)
	       "Index of plob ratio numerator.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+ratio-location-numerator+")
           	   0
           	   	       "Index of plob ratio numerator."))

  (setf  (gethash     1     last-enum-hash-table)
	       "Index of plob ratio denominator.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+ratio-location-denominator+")
           	   1
           	   	       "Index of plob ratio denominator."))

  (setf  (gethash     2     last-enum-hash-table)
	       "Size of plob ratio in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+ratio-size+")
           	   2
           	   	       "Size of plob ratio in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHRATIOIDX
           	:int
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHCOMPLEXIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHCOMPLEXIDX*)
          (let ((last-enum-hash-table   *SHCOMPLEXIDX*))
  (setf  (gethash     0     last-enum-hash-table)
	       "Index of plob complex real part.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+complex-location-real-part+")
           	   0
           	   	       "Index of plob complex real part."))

  (setf  (gethash     1     last-enum-hash-table)
	       "Index of plob complex imaginary part.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+complex-location-imag-part+")
           	   1
           	   	       "Index of plob complex imaginary part."))

  (setf  (gethash     2     last-enum-hash-table)
	       "Size of plob complex in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+complex-size+")
           	   2
           	   	       "Size of plob complex in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHCOMPLEXIDX
           	:int
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHBIGNUMIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHBIGNUMIDX*)
          (let ((last-enum-hash-table   *SHBIGNUMIDX*))
  (setf  (gethash     0     last-enum-hash-table)
	       "Index of plob bignum format. This dereferences the symbol \\lisp{:ALLEGRO}\\ for allegro-type bignums and \\lisp{:LISPWORKS}\\ for \\lw-type bignums.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+bignum-location-format+")
           	   0
           	   	       "Index of plob bignum format. This dereferences the symbol \\lisp{:ALLEGRO}\\ for allegro-type bignums and \\lisp{:LISPWORKS}\\ for \\lw-type bignums."))

  (setf  (gethash     1     last-enum-hash-table)
	       "Index of plob bignum size. The size is in bits; it is negative for negative bignums and positive for positive bignums.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+bignum-location-size+")
           	   1
           	   	       "Index of plob bignum size. The size is in bits; it is negative for negative bignums and positive for positive bignums."))

  (setf  (gethash     2     last-enum-hash-table)
	       "Size of plob bignum in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+bignum-size+")
           	   2
           	   	       "Size of plob bignum in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHBIGNUMIDX
           	:int
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbMakeBignum"
           	 #.(read-from-string    "c-sh-make-bignum")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdFormat    #.C2L_SHORTOBJID      :value-in)
             

		   (nSizeInBits    #.:int           :value-in)
             

		   (pnBignum    #.:as-is
			      :vector-in)
             

		   (bDereferencePointer    #.C2L_BOOL      :value-in)
             

		   (nUnmask    #.:int           :value-in)
             

		   (nBignumDataOffset    #.:int           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientObjectReadBignum"
           	 #.(read-from-string    "c-sh-read-bignum")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdFormat    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjId    #.C2L_SHORTOBJID      :value-in)
             

		   (nSizeInBits    #.:int           :value-in)
             

		   (pnBignum    #.:as-is
			      :vector-out)
             

		   (bDereferencePointer    #.C2L_BOOL      :value-in)
             

		   (nUnmask    #.:int           :value-in)
             

		   (nBignumDataOffset    #.:int           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientObjectReadDoubleFloat"
           	 #.(read-from-string    "c-sh-read-double-float")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjId    #.C2L_SHORTOBJID      :value-in)
             

		   (
			      pfDoubleFloat    #.:double-float           :value-out)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientObjectReadFixnum"
           	 #.(read-from-string    "c-sh-read-fixnum")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjId    #.C2L_SHORTOBJID      :value-in)
             

		   (oExpectingClass    #.C2L_SHORTOBJID      :value-in)
             

		   (nExpectingTypeTag    #.C2L_SHTYPETAG      :value-in)
             

		   (nIndex    #.:int           :value-in)
             

		   (pnFixnum    #.:int           :value-out)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientObjectReadSingleFloat"
           	 #.(read-from-string    "c-sh-read-single-float")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjId    #.C2L_SHORTOBJID      :value-in)
             

		   (
			      pfSingleFloat    #.:lisp-single-float           :value-out)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientObjectWriteBignum"
           	 #.(read-from-string    "c-sh-write-bignum")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdFormat    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjId    #.C2L_SHORTOBJID      :value-in)
             

		   (nSizeInBits    #.:int           :value-in)
             

		   (pnBignum    #.:as-is
			      :vector-in)
             

		   (bDereferencePointer    #.C2L_BOOL      :value-in)
             

		   (nUnmask    #.:int           :value-in)
             

		   (nBignumDataOffset    #.:int           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientObjectWriteDoubleFloat"
           	 #.(read-from-string    "c-sh-write-double-float")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjId    #.C2L_SHORTOBJID      :value-in)
             

		   (
			      fDoubleFloat    #.:double-float           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientObjectWriteFixnum"
           	 #.(read-from-string    "c-sh-write-fixnum")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjId    #.C2L_SHORTOBJID      :value-in)
             

		   (oExpectingClass    #.C2L_SHORTOBJID      :value-in)
             

		   (nExpectingTypeTag    #.C2L_SHTYPETAG      :value-in)
             

		   (nIndex    #.:int           :value-in)
             

		   (nFixnumWrite    #.:int           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientObjectWriteSingleFloat"
           	 #.(read-from-string    "c-sh-write-single-float")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjId    #.C2L_SHORTOBJID      :value-in)
             

		   (fSingleFloat    #.:double-float           :value-in)
                   ))    ;

