;;;; -*- Package: CL-USER; Mode: LISP; Syntax: ANSI-Common-Lisp -*-------------
;;;; Module	plob-defpackage.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	18.11.93
;;;; Description	PLOB - Persistent Lisp OBjects
;;;;		File with PLOB package definition
;;;;
;;;; Copyright	PLOB! Copyright 1994--1998 Heiko Kirschke.
;;;;		All rights reserved.
;;;;
;;;; Unlimited use, reproduction, modification and distribution of
;;;; this software is permitted.  Any copy or modified version of this
;;;; software must include both the above copyright notice of Heiko
;;;; Kirschke and this paragraph; for each modified version, an
;;;; additional statement must be added telling the year of
;;;; modification and quoting the author of the modification.  Any
;;;; distribution of this software must comply with all applicable
;;;; German export control laws.  This software is made available AS
;;;; IS, and HEIKO KIRSCHKE DISCLAIMS ALL WARRANTIES, EXPRESS OR
;;;; IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, AND
;;;; NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
;;;; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
;;;; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT
;;;; (INCLUDING NEGLIGENCE) OR STRICT LIABILITY, EVEN IF HEIKO
;;;; KIRSCHKE IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.
;;;;
;;;; Please note that these license terms adhere only to the code of
;;;; PLOB!  itself. PLOB! uses POSTORE (Persistent Object Store) as a
;;;; low-level persistent memory; it is provided in binary form within
;;;; PLOB! with the permission of the University of St. Andrews
;;;; (http://www-ppg.dcs.st-andrews.ac.uk/Default.html).  Contact the
;;;; University of St. Andrews for getting their license terms on
;;;; POSTORE.
;;;;
;;;; --------------------------------------------------------------------------

(in-package :cl-user)

(defpackage :plob
  (:use :common-lisp)
  (:export ;; lisp-system:
	   #:class-constructor

           ;; plob-extent:
           #:package-extent #:class-extent #:slot-extent
           #:slot-deferred
           #:class-dependent

           ;; plob-evolution:
           #:schema-evolution

           ;; plob-low:
           #:stableheap-configuration
           #:configuration-flags #:minimum-key #:maximum-key #:key-alignment
           #:sh-close

           #:stableheap-statistics
           #:maximum-space #:allocated-space #:unallocated-space
           #:unused-allocated-space #:allocated-management-space
	   #:number-of-objects

           #:flush-mode

           ;; plob-metaclass:
           #:persistent-metaclass

           ;; plob:
           #:*database-url*
	   #:class-description
	   #:class-description-direct-slots
	   #:class-description-effective-slots
           #:class-description-name
	   #:class-description-next-generation
           #:class-description-persistent-slot-numbers
           #:class-description-schema-evolution
           #:class-description-slot-numbers
           #:class-description-time-stamp
	   #:class-description-version-number

           #:slot-description
           #:slot-description-allocation
           #:slot-description-extent
	   #:slot-description-index
	   #:slot-description-initargs
	   #:slot-description-initform
	   #:slot-description-initfunction
	   #:slot-description-location
           #:slot-description-name
	   #:slot-description-type

           ;; plob-cons:
           #:p-allocate-cons #:p-consp
	   #:p-car #:p-cdr #:p-list #:p-cons
	   #:p-null

           ;; plob-symbol:
	   #:p-allocate-symbol #:p-symbolp #:p-boundp #:p-fboundp
	   #:p-symbol-function
           #:p-symbol-package
           #:p-symbol-plist
           #:p-symbol-name
           #:p-symbol-value
           #:p-find-symbol
           #:p-symbol
           #:p-fmakunbound #:p-makunbound #:p-setq

           ;; plob-function:
           #:p-allocate-function #:p-functionp
           #:p-function-name #:p-function-language #:p-function-code
           #:p-function

           ;; plob-vector:
	   #:p-allocate-vector #:p-vectorp
	   #:p-svref
	   #:p-vector-length
           #:p-vector

	   #:p-allocate-ivector #:p-ivectorp
           #:p-ivector-type #:p-ivector-length #:p-ivector

           ;; plob-array:
           #:p-allocate-array #:p-arrayp
           #:p-array-data-vector
           #:p-array-fill-pointer
           #:p-array-displaced-offset
           #:p-array-adjustable
           #:p-array-rank
           #:p-array-dimensions
           #:p-array-element-type
           #:p-array

           ;; plob-string:
	   #:p-allocate-string #:p-stringp
	   #:p-string-length #:p-string-max-length
           #:p-string

           ;; plob-bit-vector:
	   #:p-allocate-bit-vector #:p-bit-vector-p
	   #:p-bit-vector-length #:p-bit-vector-max-length
           #:p-bit-vector

           ;; plob-numeric:
           #:p-allocate-double-float #:p-make-double-float
	   #:p-double-float-p #:p-double-float
           #:p-allocate-single-float #:p-make-single-float
	   #:p-single-float-p #:p-single-float
	   #:p-allocate-bignum #:p-make-bignum
	   #:p-bignum-p #:p-bignum

           #:p-allocate-ratio #:p-ratiop
           #:p-numerator #:p-denominator #:p-ratio

           #:p-allocate-complex #:p-complexp
           #:p-realpart #:p-imagpart #:p-complex

           ;; plob-misc:
           #:p-apropos #:p-apropos-classes #:p-apropos-packages
           #:p-apropos-btree
           #:p-dependent

           ;; plob-tlatter (only LispWorks 3.x):
	   #+:lispworks3 #:p-tlatter-key
	   #+:lispworks3 #:p-tlatter-data
	   #+:lispworks3 #:p-tlatter-next
           #+:lispworks3 #:p-tlatter

           ;; plob-struct:
           #:p-find-class #:p-delete-class

           #:p-allocate-structure #:p-structurep
           #:p-structure-slot-ref

           #:p-allocate-structure-description
           #:p-structure-description
           #:p-structure-description-p
           #:p-structure-description-name
           #:p-structure-description-slots

           #:p-allocate-structure-slot-description
           #:p-structure-slot-description
           #:p-structure-slot-description-p
           #:p-structure-slot-description-name
           #:p-structure-slot-description-index
           #:p-structure-slot-description-init
           #:p-structure-slot-description-type

           #:p-find-package #:p-delete-package

           #:p-allocate-package #:p-package-p #:p-package
           #:p-package-name
           #:p-package-internals #:p-package-externals
           #:p-intern #:p-unintern

           #:p-hash-table

           ;; plob:
           #:persistent-object
           #:persistent-structure
           #:make-persistent-object #:persistent-object-objid

           ;; plob-session:
           #:open-session #:open-my-session #:close-session #:close-my-session
           #:show-sessions #:with-session
           #:p-configuration
           #:p-exit #:p-reset #:p-restart
           #:p-sessions #:p-stabilise #:p-statistics
	   #:p-admin

	   ;; plob-machine:
	   #:p-lookup-machine
	   #:p-machines #:p-create-machine #:p-machine-loginp #:p-machine-addr
	   #:p-search-machine #:p-delete-machine #:p-insert-machine
	   #:show-machines #:insert-machine #:delete-machine
	   
           ;; plob-heap:
           #:*default-persistent-heap*
           #:persistent-heap
           #:make-persistent-heap
	   #:p-type-tag-of #:p-class-of
	   #:read-lock-store #:write-lock-store
	   
           #:plob-flush #:clear-cache #:close-heap

           #:p-read-only

           #:begin-transaction #:cancel-transaction #:end-transaction
           #:in-transaction-p #:with-transaction

	   #:with-direct-representation
	   #:with-transient-representation

           #:store-object #:load-object

           ;; plob-btree:
           #:btree #:make-btree
           #:clrbtree
           #:getbtree #:getbtree-by-objid
           #:rembtree #:rembtree-by-objid
	   #:btree-cached-p #:btree-count #:btree-size #:btree-test
	   #:btree-pagesize #:btree-minkey #:btree-maxkey

           ;; plob-btree-mapper
           #:mapbtree #:mapbtree-by-objid
	   #:make-btree-mapper
	   #:btree-mapper-search
	   #:btree-mapper-seek
	   
	   ;; plob-database:
           #:get-index-table #:p-select #:pp-select #:p-create-database

           #:open-heap
	   
	   ;; plob-clos-slot-value:
	   #:p-slot-value #:p-slot-boundp #:p-slot-makunbound

	   ;; plob-builtin:
	   #:+plob-unbound-marker+ #:+plob-unstorable-object-marker+
	   #:+plob-min-marker+ #:+plob-max-marker+

	   ;; plob-sexpr:
	   #:persistent-s-expression
	   #:sexpr-source #:sexpr-code))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
