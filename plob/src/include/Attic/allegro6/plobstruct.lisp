
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "Feb 13 2001" "10:36:53"
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------


(in-package  :plob)

;;;; --------------------------------------------------------------------------
;;;; For further comments look into file plobstruct.h
;;;; --------------------------------------------------------------------------


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+structure-type-tag+")
           	  (parse-integer  "38" :radix    16)
      
		 "Type tag for plob objects of type STRUCTURE."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHSTRUCTIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHSTRUCTIDX*)
          (let ((last-enum-hash-table   *SHSTRUCTIDX*))
  (setf  (gethash     0     last-enum-hash-table)
	       "Index of structure description field. This field references a structure description (which is itself a structure) describing the structure.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+structure-location-description+")
           	   0
           	   	       "Index of structure description field. This field references a structure description (which is itself a structure) describing the structure."))

  (setf  (gethash     0     last-enum-hash-table)
	       "Size of of plob structure cell in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+structure-size+")
           	   0
           	   	       "Size of of plob structure cell in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHSTRUCTIDX
           	:int
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHSTRUCTDESCIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHSTRUCTDESCIDX*)
          (let ((last-enum-hash-table   *SHSTRUCTDESCIDX*))
  (setf  (gethash
	       (+   1     0)        last-enum-hash-table)
	       "Index of structure description name field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-location-name+")
           	   	       (+   1     0)
           	   	       "Index of structure description name field."))

  (setf  (gethash
	       (+   1     1)        last-enum-hash-table)
	       "Index of structure description field with a version number.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-location-version-number+")
           	   	       (+   1     1)
           	   	       "Index of structure description field with a version number."))

  (setf  (gethash
	       (+   1     2)        last-enum-hash-table)
	       "Index of structure description field with a time stamp in Common LISP Universal Time reduced to minutes,i.e. the time stamp contains a Common LISP Universal Time divided by 60.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-location-time-stamp+")
           	   	       (+   1     2)
           	   	       "Index of structure description field with a time stamp in Common LISP Universal Time reduced to minutes,i.e. the time stamp contains a Common LISP Universal Time divided by 60."))

  (setf  (gethash
	       (+   1     3)        last-enum-hash-table)
	       "Index of structure description field which contains the kind of schema evolution.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-location-schema-evolution+")
           	   	       (+   1     3)
           	   	       "Index of structure description field which contains the kind of schema evolution."))

  (setf  (gethash
	       (+   1     4)        last-enum-hash-table)
	       "Index of structure description field with reference to description of the next generation.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-location-next-generation+")
           	   	       (+   1     4)
           	   	       "Index of structure description field with reference to description of the next generation."))

  (setf  (gethash
	       (+   1     5)        last-enum-hash-table)
	       "Index of structure description field with name of constructor function.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-location-constructor+")
           	   	       (+   1     5)
           	   	       "Index of structure description field with name of constructor function."))

  (setf  (gethash
	       (+   1     6)        last-enum-hash-table)
	       "Index of structure description field with the dependent flag.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-location-dependent+")
           	   	       (+   1     6)
           	   	       "Index of structure description field with the dependent flag."))

  (setf  (gethash
	       (+   1     7)        last-enum-hash-table)
	       "Index of structure description field with the number of persistent slots.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-location-persistent-slot-numbers+")
           	   	       (+   1     7)
           	   	       "Index of structure description field with the number of persistent slots."))

  (setf  (gethash
	       (+   1     8)        last-enum-hash-table)
	       "Index of structure description field with the total number of slots.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-location-slot-numbers+")
           	   	       (+   1     8)
           	   	       "Index of structure description field with the total number of slots."))

  (setf  (gethash
	       (+   1     9)        last-enum-hash-table)
	       "Index of structure description slot description list.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-location-slot-descriptions+")
           	   	       (+   1     9)
           	   	       "Index of structure description slot description list."))

  (setf  (gethash
	       (+   1     10)        last-enum-hash-table)
	       "Size of of plob structure slot description in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-description-size+")
           	   	       (+   1     10)
           	   	       "Size of of plob structure slot description in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHSTRUCTDESCIDX
           	:int
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHSTRUCTSLOTDESCIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHSTRUCTSLOTDESCIDX*)
          (let ((last-enum-hash-table   *SHSTRUCTSLOTDESCIDX*))
  (setf  (gethash
	       (+   1     0)        last-enum-hash-table)
	       "Index of structure slot description name field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-slot-description-location-name+")
           	   	       (+   1     0)
           	   	       "Index of structure slot description name field."))

  (setf  (gethash
	       (+   1     1)        last-enum-hash-table)
	       "Index of structure slot description initarg field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-slot-description-location-initarg+")
           	   	       (+   1     1)
           	   	       "Index of structure slot description initarg field."))

  (setf  (gethash
	       (+   1     2)        last-enum-hash-table)
	       "Index of structure slot description reader field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-slot-description-location-reader+")
           	   	       (+   1     2)
           	   	       "Index of structure slot description reader field."))

  (setf  (gethash
	       (+   1     3)        last-enum-hash-table)
	       "Index of structure slot description location field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-slot-description-location-location+")
           	   	       (+   1     3)
           	   	       "Index of structure slot description location field."))

  (setf  (gethash
	       (+   1     4)        last-enum-hash-table)
	       "Index of structure slot description init field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-slot-description-location-init+")
           	   	       (+   1     4)
           	   	       "Index of structure slot description init field."))

  (setf  (gethash
	       (+   1     5)        last-enum-hash-table)
	       "Index of structure slot description type field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-slot-description-location-type+")
           	   	       (+   1     5)
           	   	       "Index of structure slot description type field."))

  (setf  (gethash
	       (+   1     6)        last-enum-hash-table)
	       "Index of structure slot description extent field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-slot-description-location-extent+")
           	   	       (+   1     6)
           	   	       "Index of structure slot description extent field."))

  (setf  (gethash
	       (+   1     7)        last-enum-hash-table)
	       "Index of structure slot description deferred field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-slot-description-location-deferred+")
           	   	       (+   1     7)
           	   	       "Index of structure slot description deferred field."))

  (setf  (gethash
	       (+   1     8)        last-enum-hash-table)
	       "Size of of plob structure slot description in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+structure-slot-description-size+")
           	   	       (+   1     8)
           	   	       "Size of of plob structure slot description in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHSTRUCTSLOTDESCIDX
           	:int
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHPACKAGEIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHPACKAGEIDX*)
          (let ((last-enum-hash-table   *SHPACKAGEIDX*))
  (setf  (gethash
	       (+   1     0)        last-enum-hash-table)
	       "Index of package name field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+package-location-name+")
           	   	       (+   1     0)
           	   	       "Index of package name field."))

  (setf  (gethash
	       (+   1     1)        last-enum-hash-table)
	       "Index of package internal symbol table field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+package-location-internals+")
           	   	       (+   1     1)
           	   	       "Index of package internal symbol table field."))

  (setf  (gethash
	       (+   1     2)        last-enum-hash-table)
	       "Index of package external symbol table field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+package-location-externals+")
           	   	       (+   1     2)
           	   	       "Index of package external symbol table field."))

  (setf  (gethash
	       (+   1     3)        last-enum-hash-table)
	       "Size of of plob package in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+package-size+")
           	   	       (+   1     3)
           	   	       "Size of of plob package in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHPACKAGEIDX
           	:int
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbCreateStructure"
           	 #.(read-from-string    "c-sh-create-structure")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (
			      oShortObjIdStructDescr    #.C2L_SHORTOBJID      :value-in)
                   ))    ;

