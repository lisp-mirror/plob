
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "Aug 25 1999" "05:11:46"
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------


  (in-package  :plob)

;;;; --------------------------------------------------------------------------
;;;; For further comments look into file plobbtree.h
;;;; --------------------------------------------------------------------------


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+btree-type-tag+")
           	  (parse-integer  "98" :radix    16)
      
		 "Type tag for plob objects of type BTREE."))     ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+btree-page-tag+")
           	  (parse-integer  "A0" :radix    16)
      
		 "Type tag for plob objects of type BTREEPAGE."))     ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+btree-mapper-tag+")
           	  (parse-integer  "D8" :radix    16)
      
		 "Type tag for plob objects of type BTREEMAPPER."))     ;


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+btree-get-test-mode+")
           	  -2
      
		 "A tag for fnClientBtreeTestMode indicating to return the current mode."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *BTREERESULT*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *BTREERESULT*)
          (let ((last-enum-hash-table   *BTREERESULT*))
  (setf  (gethash     0     last-enum-hash-table)
	       "The key has not been found in the btree.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+btree-not-found+")
           	   0
           	   	       "The key has not been found in the btree."))

  (setf  (gethash     1     last-enum-hash-table)
	       "The key has been found in the btree.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+btree-found+")
           	   1
           	   	       "The key has been found in the btree."))

  (setf  (gethash     2     last-enum-hash-table)
	       "The key and data have been inserted into the btree.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+btree-inserted+")
           	   2
           	   	       "The key and data have been inserted into the btree."))

  (setf  (gethash     3     last-enum-hash-table)
	       "The key was found in the btree; updated data field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+btree-updated+")
           	   3
           	   	       "The key was found in the btree; updated data field."))

  (setf  (gethash     4     last-enum-hash-table)
	       "The key was deleted from the btree.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+btree-deleted+")
           	   4
           	   	       "The key was deleted from the btree."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_BTREERESULT
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHBTREEIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHBTREEIDX*)
          (let ((last-enum-hash-table   *SHBTREEIDX*))
  (setf  (gethash     0     last-enum-hash-table)
	       "Index of plob btree self field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+btree-location-self+")
           	   0
           	   	       "Index of plob btree self field."))

  (setf  (gethash     1     last-enum-hash-table)
	       "Index of plob btree compare field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+btree-location-compare+")
           	   1
           	   	       "Index of plob btree compare field."))

  (setf  (gethash     2     last-enum-hash-table)
	       "Index of plob btree field with number of items.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+btree-location-count+")
           	   2
           	   	       "Index of plob btree field with number of items."))

  (setf  (gethash     3     last-enum-hash-table)
	       "Index of plob btree field with flag if the btree should be cached.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+btree-location-cached+")
           	   3
           	   	       "Index of plob btree field with flag if the btree should be cached."))

  (setf  (gethash     4     last-enum-hash-table)
	       "Index of plob btree field with flag if duplicate keys are allowed.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+btree-location-duplicate-keys-p+")
           	   4
           	   	       "Index of plob btree field with flag if duplicate keys are allowed."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHBTREEIDX
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeClear"
           	 #.(read-from-string    "c-sh-btree-clear")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreeCount"
           	 #.(read-from-string    "c-sh-btree-count")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeDelete"
           	 #.(read-from-string    "c-sh-btree-delete")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (nValueKey    #.:fixnum           :value-in)
             

		   (nTypeTagKey    #.C2L_SHTYPETAG      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeDeleteByFloat"
           	 #.(read-from-string    "c-sh-btree-delete-by-float")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (fKey    #.:double-float           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeDeleteByDouble"
           	 #.(read-from-string    "c-sh-btree-delete-by-double")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (fKey    #.:double-float           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeDeleteByString"
           	 #.(read-from-string    "c-sh-btree-delete-by-string")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (szKey    #.:string           :vector-in)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeInsert"
           	 #.(read-from-string    "c-sh-btree-insert")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (nValueKey    #.:fixnum           :value-in)
             

		   (nTypeTagKey    #.C2L_SHTYPETAG      :value-in)
             

		   (nValueData    #.:fixnum           :value-in)
             

		   (nTypeTagData    #.C2L_SHTYPETAG      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeInsertByFloat"
           	 #.(read-from-string    "c-sh-btree-insert-by-float")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (fKey    #.:double-float           :value-in)
             

		   (nValueData    #.:fixnum           :value-in)
             

		   (nTypeTagData    #.C2L_SHTYPETAG      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeInsertByDouble"
           	 #.(read-from-string    "c-sh-btree-insert-by-double")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (fKey    #.:double-float           :value-in)
             

		   (nValueData    #.:fixnum           :value-in)
             

		   (nTypeTagData    #.C2L_SHTYPETAG      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeInsertByString"
           	 #.(read-from-string    "c-sh-btree-insert-by-string")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (szKey    #.:string           :vector-in)
             

		   (nValueData    #.:fixnum           :value-in)
             

		   (nTypeTagData    #.C2L_SHTYPETAG      :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreemapSearch"
           	 #.(read-from-string    "c-sh-btree-map-search")
      
		 ( (oShortObjIdMapper    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (nValueKeyLower    #.:fixnum           :value-in)
             

		   (nTypeTagKeyLower    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareLower    #.C2L_COMPARETAG      :value-in)
             

		   (nValueKeyUpper    #.:fixnum           :value-in)
             

		   (nTypeTagKeyUpper    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareUpper    #.C2L_COMPARETAG      :value-in)
             

		   (bDescending    #.C2L_BOOL      :value-in)
                   ))    ;


  (define-foreign-function
            #.:void
           	"fnClientBtreemapSearchByFloat"
           	 #.(read-from-string
		 "c-sh-btree-map-search-by-float")
      
		 ( (pnReturnValue    #.:fixnum           :value-out)
             

		   (oShortObjIdMapper    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (fKeyLower    #.:double-float           :value-in)
             

		   (nTypeTagKeyLower    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareLower    #.C2L_COMPARETAG      :value-in)
             

		   (fKeyUpper    #.:double-float           :value-in)
             

		   (nTypeTagKeyUpper    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareUpper    #.C2L_COMPARETAG      :value-in)
             

		   (bDescending    #.C2L_BOOL      :value-in)
                   ))    ;


  (define-foreign-function
            #.:void
           	"fnClientBtreemapSearchByDouble"
           	 #.(read-from-string
		 "c-sh-btree-map-search-by-double")
      
		 ( (pnReturnValue    #.:fixnum           :value-out)
             

		   (oShortObjIdMapper    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (fKeyLower    #.:double-float           :value-in)
             

		   (nTypeTagKeyLower    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareLower    #.C2L_COMPARETAG      :value-in)
             

		   (fKeyUpper    #.:double-float           :value-in)
             

		   (nTypeTagKeyUpper    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareUpper    #.C2L_COMPARETAG      :value-in)
             

		   (bDescending    #.C2L_BOOL      :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreemapSearchByString"
           	 #.(read-from-string
		 "c-sh-btree-map-search-by-string")
      
		 ( (oShortObjIdMapper    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (szKeyLower    #.:string           :vector-in)
             

		   (nTypeTagKeyLower    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareLower    #.C2L_COMPARETAG      :value-in)
             

		   (szKeyUpper    #.:string           :vector-in)
             

		   (nTypeTagKeyUpper    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareUpper    #.C2L_COMPARETAG      :value-in)
             

		   (bDescending    #.C2L_BOOL      :value-in)
                   ))    ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SEEK*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SEEK*)
          (let ((last-enum-hash-table   *SEEK*))
  (setf  (gethash     0     last-enum-hash-table)
	       "Seek position relative to start position.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+seek-set+")
           	   0
           	   	       "Seek position relative to start position."))

  (setf  (gethash     1     last-enum-hash-table)
	       "Seek position relative to current position.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+seek-current+")
           	   1
           	   	       "Seek position relative to current position."))

  (setf  (gethash     2     last-enum-hash-table)
	       "Seek position relative to end position.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+seek-end+")
           	   2
           	   	       "Seek position relative to end position."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SEEK
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreemapSeek"
           	 #.(read-from-string    "c-sh-btree-map-seek")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdMapper    #.C2L_SHORTOBJID      :value-in)
             

		   (nIncrement    #.:fixnum           :value-in)
             

		   (eOrigin    #.C2L_SEEK      :value-in)
             

		   (pnValueKey    #.:fixnum           :value-out)
             

		   (pnTypeTagKey    #.C2L_SHTYPETAG      :value-out)
             

		   (pnValueData    #.:fixnum           :value-out)
             

		   (pnTypeTagData    #.C2L_SHTYPETAG      :value-out)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreemapSeekSet"
           	 #.(read-from-string    "c-sh-btree-map-seek-set")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdMapper    #.C2L_SHORTOBJID      :value-in)
             

		   (nIncrement    #.:fixnum           :value-in)
             

		   (eOrigin    #.C2L_SEEK      :value-in)
             

		   (nValueData    #.:fixnum           :value-in)
             

		   (nTypeTagData    #.C2L_SHTYPETAG      :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreemapFirst"
           	 #.(read-from-string    "c-sh-btree-map-first")
      
		 ( (lpoShortObjIdMapper    #.C2L_SHORTOBJID      :value-out)
             

		   (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (nValueKeyLower    #.:fixnum           :value-in)
             

		   (nTypeTagKeyLower    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareLower    #.C2L_COMPARETAG      :value-in)
             

		   (nValueKeyUpper    #.:fixnum           :value-in)
             

		   (nTypeTagKeyUpper    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareUpper    #.C2L_COMPARETAG      :value-in)
             

		   (bDescending    #.C2L_BOOL      :value-in)
             

		   (nMap    #.:fixnum           :value-in)
             

		   (pnValueKey    #.:fixnum
			      :vector-out)
             

		   (pnTypeTagKey    #.:fixnum
			      :vector-out)
             

		   (pnValueData    #.:fixnum
			      :vector-out)
             

		   (pnTypeTagData    #.:fixnum
			      :vector-out)
                   ))    ;


  (define-foreign-function
            #.:void
           	"fnClientBtreemapFirstByFloat"
           	 #.(read-from-string
		 "c-sh-btree-map-first-by-float")
      
		 ( (pnReturnValue    #.:fixnum           :value-out)
             

		   (lpoShortObjIdMapper    #.C2L_SHORTOBJID      :value-out)
             

		   (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (fKeyLower    #.:double-float           :value-in)
             

		   (nTypeTagKeyLower    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareLower    #.C2L_COMPARETAG      :value-in)
             

		   (fKeyUpper    #.:double-float           :value-in)
             

		   (nTypeTagKeyUpper    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareUpper    #.C2L_COMPARETAG      :value-in)
             

		   (bDescending    #.C2L_BOOL      :value-in)
             

		   (nMap    #.:fixnum           :value-in)
             

		   (pnValueKey    #.:fixnum
			      :vector-out)
             

		   (pnTypeTagKey    #.:fixnum
			      :vector-out)
             

		   (pnValueData    #.:fixnum
			      :vector-out)
             

		   (pnTypeTagData    #.:fixnum
			      :vector-out)
                   ))    ;


  (define-foreign-function
            #.:void
           	"fnClientBtreemapFirstByDouble"
           	 #.(read-from-string
		 "c-sh-btree-map-first-by-double")
      
		 ( (pnReturnValue    #.:fixnum           :value-out)
             

		   (lpoShortObjIdMapper    #.C2L_SHORTOBJID
			      :value-out)
             

		   (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (fKeyLower    #.:double-float           :value-in)
             

		   (nTypeTagKeyLower    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareLower    #.C2L_COMPARETAG      :value-in)
             

		   (fKeyUpper    #.:double-float           :value-in)
             

		   (nTypeTagKeyUpper    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareUpper    #.C2L_COMPARETAG      :value-in)
             

		   (bDescending    #.C2L_BOOL      :value-in)
             

		   (nMap    #.:fixnum           :value-in)
             

		   (pnValueKey    #.:fixnum
			      :vector-out)
             

		   (pnTypeTagKey    #.:fixnum
			      :vector-out)
             

		   (pnValueData    #.:fixnum
			      :vector-out)
             

		   (pnTypeTagData    #.:fixnum
			      :vector-out)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreemapFirstByString"
           	 #.(read-from-string
		 "c-sh-btree-map-first-by-string")
      
		 ( (lpoShortObjIdMapper    #.C2L_SHORTOBJID      :value-out)
             

		   (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (szKeyLower    #.:string           :vector-in)
             

		   (nTypeTagKeyLower    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareLower    #.C2L_COMPARETAG      :value-in)
             

		   (szKeyUpper    #.:string           :vector-in)
             

		   (nTypeTagKeyUpper    #.C2L_SHTYPETAG      :value-in)
             

		   (eCompareUpper    #.C2L_COMPARETAG      :value-in)
             

		   (bDescending    #.C2L_BOOL      :value-in)
             

		   (nMap    #.:fixnum           :value-in)
             

		   (pnValueKey    #.:fixnum
			      :vector-out)
             

		   (pnTypeTagKey    #.:fixnum
			      :vector-out)
             

		   (pnValueData    #.:fixnum
			      :vector-out)
             

		   (pnTypeTagData    #.:fixnum
			      :vector-out)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreemapNext"
           	 #.(read-from-string    "c-sh-btree-map-next")
      
		 ( (oShortObjIdMapper    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (nMap    #.:fixnum           :value-in)
             

		   (pnValueKey    #.:fixnum
			      :vector-out)
             

		   (pnTypeTagKey    #.:fixnum
			      :vector-out)
             

		   (pnValueData    #.:fixnum
			      :vector-out)
             

		   (pnTypeTagData    #.:fixnum
			      :vector-out)
                   ))    ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientBtreemapLast"
           	 #.(read-from-string    "c-sh-btree-map-last")
      
		 ( (oShortObjIdMapper    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreePrint"
           	 #.(read-from-string    "c-sh-btree-print")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (nStdStream    #.C2L_NUMERICSTDSTREAM      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeSearch"
           	 #.(read-from-string    "c-sh-btree-search")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (nValueKey    #.:fixnum           :value-in)
             

		   (nTypeTagKey    #.C2L_SHTYPETAG      :value-in)
             

		   (pnValueKey    #.:fixnum           :value-out)
             

		   (pnTypeTagKey    #.C2L_SHTYPETAG      :value-out)
             

		   (pnValueData    #.:fixnum           :value-out)
             

		   (pnTypeTagData    #.C2L_SHTYPETAG      :value-out)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeSearchByFloat"
           	 #.(read-from-string    "c-sh-btree-search-by-float")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (fKey    #.:double-float           :value-in)
             

		   (nTypeTagKey    #.C2L_SHTYPETAG      :value-in)
             

		   (pnValueKey    #.:fixnum           :value-out)
             

		   (pnTypeTagKey    #.C2L_SHTYPETAG      :value-out)
             

		   (pnValueData    #.:fixnum           :value-out)
             

		   (pnTypeTagData    #.C2L_SHTYPETAG      :value-out)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeSearchByDouble"
           	 #.(read-from-string    "c-sh-btree-search-by-double")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (fKey    #.:double-float           :value-in)
             

		   (nTypeTagKey    #.C2L_SHTYPETAG      :value-in)
             

		   (pnValueKey    #.:fixnum           :value-out)
             

		   (pnTypeTagKey    #.C2L_SHTYPETAG      :value-out)
             

		   (pnValueData    #.:fixnum           :value-out)
             

		   (pnTypeTagData    #.C2L_SHTYPETAG      :value-out)
                   ))    ;


  (define-foreign-function
            #.C2L_BTREERESULT
           	"fnClientBtreeSearchByString"
           	 #.(read-from-string    "c-sh-btree-search-by-string")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (szKey    #.:string           :vector-in)
             

		   (nTypeTagKey    #.C2L_SHTYPETAG      :value-in)
             

		   (pnValueKey    #.:fixnum           :value-out)
             

		   (pnTypeTagKey    #.C2L_SHTYPETAG      :value-out)
             

		   (pnValueData    #.:fixnum           :value-out)
             

		   (pnTypeTagData    #.C2L_SHTYPETAG      :value-out)
                   ))    ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientBtreeRoot"
           	 #.(read-from-string    "c-sh-btree-root")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreeSize"
           	 #.(read-from-string    "c-sh-btree-size")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_COMPARETAG
           	"fnClientBtreeTestMode"
           	 #.(read-from-string    "c-sh-btree-test-mode")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (nNewTestMode    #.C2L_COMPARETAG      :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreePageSize"
           	 #.(read-from-string    "c-sh-btree-set-page-size")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTree    #.C2L_SHORTOBJID      :value-in)
             

		   (nNewPageSize    #.:fixnum           :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_OBJID
           	"fnClientBtreepageParent"
           	 #.(read-from-string    "c-sh-btreepage-parent")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTreePage    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreepageCount"
           	 #.(read-from-string    "c-sh-btreepage-count")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTreePage    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreepageGetSize"
           	 #.(read-from-string    "c-sh-btreepage-size")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTreePage    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.:fixnum
           	"fnClientBtreepageItem"
           	 #.(read-from-string    "c-sh-btreepage-item")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdBTreePage    #.C2L_SHORTOBJID      :value-in)
             

		   (nIndex    #.:fixnum           :value-in)
             

		   (nItems    #.:fixnum           :value-in)
             

		   (
			      pnValueKey    #.:fixnum              :vector-out)
             

		   (
			      pnTypeTagKey    #.:fixnum              :vector-out)
             

		   (
			      pnValueData    #.:fixnum              :vector-out)
             

		   (
			      pnTypeTagData    #.:fixnum              :vector-out)
             

		   (
			      poNext    #.:fixnum              :vector-out)
                   ))    ;

