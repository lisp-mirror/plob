
;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp -*-----------------------------
;;;; NOTE: This Common LISP file was generated by c2lisp
;;;; ===== on "Feb 13 2001" "10:36:10"
;;;; Changes done directly to this file will be lost!
;;;; ------------------------------------------------------------------------


(in-package  :plob)

;;;; -------------------------------------------------------------------------
;;;; For further comments look into file plobclos.h
;;;; -------------------------------------------------------------------------


  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string    "+instance-type-tag+")
           	  (parse-integer  "48" :radix    16)
      
		 "Type tag for plob objects of type clos instance."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHINSTIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHINSTIDX*)
          (let ((last-enum-hash-table   *SHINSTIDX*))
  (setf  (gethash     0     last-enum-hash-table)
	       "Index of plob CLOS instance class wrapper. This is a reference to an instance of class class-description.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+clos-location-class-wrapper+")
           	   0
           	   	       "Index of plob CLOS instance class wrapper. This is a reference to an instance of class class-description."))

  (setf  (gethash     1     last-enum-hash-table)
	       "Index of plob CLOS instance data vector.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+clos-location-data-vector+")
           	   1
           	   	       "Index of plob CLOS instance data vector."))

  (setf  (gethash     2     last-enum-hash-table)
	       "Size of plob CLOS instance in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string     "+clos-instance-size+")
           	   2
           	   	       "Size of plob CLOS instance in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHINSTIDX
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHPLOBDESCRIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHPLOBDESCRIDX*)
          (let ((last-enum-hash-table   *SHPLOBDESCRIDX*))
  (setf  (gethash     0      last-enum-hash-table)
	       "Size of PLOB description in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+plob-description-size+")
           	   0
           	   	       "Size of PLOB description in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHPLOBDESCRIDX
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHCLASSDESCRIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHCLASSDESCRIDX*)
          (let ((last-enum-hash-table   *SHCLASSDESCRIDX*))
  (setf  (gethash
	       (+   0     0)        last-enum-hash-table)
	       "Index of CLOS class description name field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-name+")
           	   	       (+   0     0)
           	   	       "Index of CLOS class description name field."))

  (setf  (gethash
	       (+   0     1)        last-enum-hash-table)
	       "Index of CLOS class description superclasses field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-superclasses+")
           	   	       (+   0     1)
           	   	       "Index of CLOS class description superclasses field."))

  (setf  (gethash
	       (+   0     2)        last-enum-hash-table)
	       "Index of CLOS class description precedence list field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-precedence-list+")
           	   	       (+   0     2)
           	   	       "Index of CLOS class description precedence list field."))

  (setf  (gethash
	       (+   0     3)        last-enum-hash-table)
	       "Index of CLOS class description metaclass field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-metaclass+")
           	   	       (+   0     3)
           	   	       "Index of CLOS class description metaclass field."))

  (setf  (gethash
	       (+   0     4)        last-enum-hash-table)
	       "Index of CLOS class description version field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-version+")
           	   	       (+   0     4)
           	   	       "Index of CLOS class description version field."))

  (setf  (gethash
	       (+   0     5)        last-enum-hash-table)
	       "Index of CLOS class description time stamp field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-time-stamp+")
           	   	       (+   0     5)
           	   	       "Index of CLOS class description time stamp field."))

  (setf  (gethash
	       (+   0     6)        last-enum-hash-table)
	       "Index of CLOS class description schema evolution field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-schema-evolution+")
           	   	       (+   0     6)
           	   	       "Index of CLOS class description schema evolution field."))

  (setf  (gethash
	       (+   0     7)        last-enum-hash-table)
	       "Index of CLOS class description field with the reference to the next generation.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-next-generation+")
           	   	       (+   0     7)
           	   	       "Index of CLOS class description field with the reference to the next generation."))

  (setf  (gethash
	       (+   0     8)        last-enum-hash-table)
	       "Index of CLOS class description field with the direct method descriptions.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-direct-methods+")
           	   	       (+   0     8)
           	   	       "Index of CLOS class description field with the direct method descriptions."))

  (setf  (gethash
	       (+   0     9)        last-enum-hash-table)
	       "Index of CLOS class description field with the number of persistent slots.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-persistent-slot-numbers+")
           	   	       (+   0     9)
           	   	       "Index of CLOS class description field with the number of persistent slots."))

  (setf  (gethash
	       (+   0     10)        last-enum-hash-table)
	       "Index of CLOS class description field with the total number of slots.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-slot-numbers+")
           	   	       (+   0     10)
           	   	       "Index of CLOS class description field with the total number of slots."))

  (setf  (gethash
	       (+   0     11)        last-enum-hash-table)
	       "Index of CLOS class description field with the descriptions of the direct slots.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-direct-slots+")
           	   	       (+   0     11)
           	   	       "Index of CLOS class description field with the descriptions of the direct slots."))

  (setf  (gethash
	       (+   0     12)        last-enum-hash-table)
	       "Index of CLOS class description field with the descriptions of the effective slots.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-effective-slots+")
           	   	       (+   0     12)
           	   	       "Index of CLOS class description field with the descriptions of the effective slots."))

  (setf  (gethash
	       (+   0     13)        last-enum-hash-table)
	       "Index of CLOS class description field with the instance constructor function.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-constructor+")
           	   	       (+   0     13)
           	   	       "Index of CLOS class description field with the instance constructor function."))

  (setf  (gethash
	       (+   0     14)        last-enum-hash-table)
	       "Index of CLOS class description field with the instance dependent flag.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-dependent+")
           	   	       (+   0     14)
           	   	       "Index of CLOS class description field with the instance dependent flag."))

  (setf  (gethash
	       (+   0     15)        last-enum-hash-table)
	       "Index of CLOS class description field with the class' property list.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-plist+")
           	   	       (+   0     15)
           	   	       "Index of CLOS class description field with the class' property list."))

  (setf  (gethash
	       (+   0     16)        last-enum-hash-table)
	       "Index of CLOS class description field with the prototype instance.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-location-prototype+")
           	   	       (+   0     16)
           	   	       "Index of CLOS class description field with the prototype instance."))

  (setf  (gethash     (+   0     17)        last-enum-hash-table)
	       "Size of PLOB CLOS class description in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+class-description-size+")
           	   (+   0     17)
           	   	       "Size of PLOB CLOS class description in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHCLASSDESCRIDX
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHCLASSLOTIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHCLASSLOTIDX*)
          (let ((last-enum-hash-table   *SHCLASSLOTIDX*))
  (setf  (gethash
	       (+   0     0)        last-enum-hash-table)
	       "Index of CLOS slot description name field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-name+")
           	   	       (+   0     0)
           	   	       "Index of CLOS slot description name field."))

  (setf  (gethash
	       (+   0     1)        last-enum-hash-table)
	       "Index of CLOS slot description initarg field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-init-args+")
           	   	       (+   0     1)
           	   	       "Index of CLOS slot description initarg field."))

  (setf  (gethash
	       (+   0     2)        last-enum-hash-table)
	       "Index of CLOS slot description initform field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-init-form+")
           	   	       (+   0     2)
           	   	       "Index of CLOS slot description initform field."))

  (setf  (gethash
	       (+   0     3)        last-enum-hash-table)
	       "Index of CLOS slot description initform field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-init-function+")
           	   	       (+   0     3)
           	   	       "Index of CLOS slot description initform field."))

  (setf  (gethash
	       (+   0     4)        last-enum-hash-table)
	       "Index of CLOS slot description type field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-type+")
           	   	       (+   0     4)
           	   	       "Index of CLOS slot description type field."))

  (setf  (gethash
	       (+   0     5)        last-enum-hash-table)
	       "Index of CLOS slot description allocation field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-allocation+")
           	   	       (+   0     5)
           	   	       "Index of CLOS slot description allocation field."))

  (setf  (gethash
	       (+   0     6)        last-enum-hash-table)
	       "Index of CLOS slot description extent field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-extent+")
           	   	       (+   0     6)
           	   	       "Index of CLOS slot description extent field."))

  (setf  (gethash
	       (+   0     7)        last-enum-hash-table)
	       "Index of CLOS slot description deferred field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-deferred+")
           	   	       (+   0     7)
           	   	       "Index of CLOS slot description deferred field."))

  (setf  (gethash
	       (+   0     8)        last-enum-hash-table)
	       "Index of CLOS slot description index field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-index+")
           	   	       (+   0     8)
           	   	       "Index of CLOS slot description index field."))

  (setf  (gethash     (+   0     9)         last-enum-hash-table)
	       "Size of PLOB CLOS slot description in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-size+")
           	   (+   0     9)
           	   	       "Size of PLOB CLOS slot description in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHCLASSLOTIDX
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHDIRCLASSLOTIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHDIRCLASSLOTIDX*)
          (let ((last-enum-hash-table   *SHDIRCLASSLOTIDX*))
  (setf  (gethash
	       (+   (+   0     9)        0)        last-enum-hash-table)
	       "Index of CLOS direct slot description readers field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-readers+")
           	   	       (+   (+   0     9)        0)
           	   	       "Index of CLOS direct slot description readers field."))

  (setf  (gethash
	       (+   (+   0     9)        1)        last-enum-hash-table)
	       "Index of CLOS direct slot description writers field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-writers+")
           	   	       (+   (+   0     9)        1)
           	   	       "Index of CLOS direct slot description writers field."))

  (setf  (gethash
	       (+   (+   0     9)        2)        last-enum-hash-table)
	       "Size of PLOB CLOS direct slot description in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+direct-slot-description-size+")
           	   	       (+   (+   0     9)        2)
           	   	       "Size of PLOB CLOS direct slot description in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHDIRCLASSLOTIDX
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHEFFCLASSLOTIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHEFFCLASSLOTIDX*)
          (let ((last-enum-hash-table   *SHEFFCLASSLOTIDX*))
  (setf  (gethash
	       (+   (+   0     9)        0)        last-enum-hash-table)
	       "Index of CLOS effective slot description location field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+slot-description-location-location+")
           	   	       (+   (+   0     9)        0)
           	   	       "Index of CLOS effective slot description location field."))

  (setf  (gethash
	       (+   (+   0     9)        1)        last-enum-hash-table)
	       "Size of PLOB CLOS effective slot description in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+effective-slot-description-size+")
           	   	       (+   (+   0     9)        1)
           	   	       "Size of PLOB CLOS effective slot description in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHEFFCLASSLOTIDX
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (eval-when (:compile-toplevel    :load-toplevel    :execute)
          (progn
          (defvar
            *SHMETHODIDX*
            (make-hash-table  :test    (quote  equal))
           	"Variable defined by C macro BeginEnum.")
          (clrhash   *SHMETHODIDX*)
          (let ((last-enum-hash-table   *SHMETHODIDX*))
  (setf  (gethash
	       (+   0     0)        last-enum-hash-table)
	       "Index of CLOS method description function field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+method-description-location-name+")
           	   	       (+   0     0)
           	   	       "Index of CLOS method description function field."))

  (setf  (gethash
	       (+   0     1)        last-enum-hash-table)
	       "Index of CLOS method description function field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+method-description-location-function+")
           	   	       (+   0     1)
           	   	       "Index of CLOS method description function field."))

  (setf  (gethash
	       (+   0     2)        last-enum-hash-table)
	       "Index of CLOS method description  field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+method-description-location-lambda-list+")
           	   	       (+   0     2)
           	   	       "Index of CLOS method description  field."))

  (setf  (gethash
	       (+   0     3)        last-enum-hash-table)
	       "Index of CLOS method description  field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+method-description-location-specializers+")
           	   	       (+   0     3)
           	   	       "Index of CLOS method description  field."))

  (setf  (gethash
	       (+   0     4)        last-enum-hash-table)
	       "Index of CLOS method description  field.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+method-description-location-qualifiers+")
           	   	       (+   0     4)
           	   	       "Index of CLOS method description  field."))

  (setf  (gethash
	       (+   0     5)        last-enum-hash-table)
	       "Size of PLOB CLOS method description in words.")

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
            #.(read-from-string
	       "+method-description-size+")
           	   	       (+   0     5)
           	   	       "Size of PLOB CLOS method description in words."))
)))

  (eval-when
          (:compile-toplevel    :load-toplevel    :execute)
           	 (defconstant
           C2L_SHMETHODIDX
           	:fixnum
           	"Constant defined by C macro EndEnum."))     ;


  (define-foreign-function
            #.C2L_SHORTOBJID
           	"fnClientDbCreateInstance"
           	 #.(read-from-string    "c-sh-create-instance")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (
			      oShortObjIdClassDescr    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientInstanceWriteWrapper"
           	 #.(read-from-string
		 "c-sh-write-instance-wrapper")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdInstance    #.C2L_SHORTOBJID      :value-in)
             

		   (
			      oShortObjIdClassDescr    #.C2L_SHORTOBJID      :value-in)
                   ))    ;


  (define-foreign-function
            #.C2L_SHLOCK
           	"fnClientInstanceWriteData"
           	 #.(read-from-string
		 "c-sh-write-instance-data")
      
		 ( (oShortObjIdHeap    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdInstance    #.C2L_SHORTOBJID      :value-in)
             

		   (oShortObjIdData    #.C2L_SHORTOBJID      :value-in)
                   ))    ;

