;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-writers.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	10.3.94
;;;; Description	PLOB writer generic functions for class- and slot
;;;;		descriptions. The generic functions defined here are only for
;;;;		internal use by PLOB.
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

(in-package :plob)

;;; ---------------------------------------------------------------------------
;;; Generic functions for writing to class- and slot-descriptions.
;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-constructor)
     (constructor class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{constructor}}
      {a symbol}
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Set the constructor of
 \\funarg{class-description}\\ to
 \\funarg{constructor}.
\\Seealsolabel
 Slot {\\bf p-constructor} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-name)
     (name class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{name}}
      {a symbol}
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Set the name of
 \\funarg{class-description}\\ to
 \\funarg{name}.
\\Seealsolabel
 Slot {\\bf p-name} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-of)
     (class-description class)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
 \\isacls{\\funarg{class}}
\\Purposelabel
 Set the class-description of \\funarg{class}\\ to
 \\funarg{class-description}.
\\Seealsolabel
 \\Fcite{structure-description};
 \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-name->slot-cache)
     (name->slot-cache class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{name->slot-cache}}
      {a cache, i.e.\\ a hash-table}
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Set the name->slot-cache of
 \\funarg{class-description}\\ to
 \\funarg{name->slot-cache}.
\\Seealsolabel
 Slot {\\bf p-name->slot-cache} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-next-generation)
     (next-generation class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{next-generation}\\ resp.\\ \\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Set the next generation of
 \\funarg{class-description}\\ to
 \\funarg{next-generation}.
\\Seealsolabel
 Slot {\\bf p-next-generation} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-persistent-slot-numbers)
     (persistent-slot-numbers class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{persistent-slot-numbers}}
      {a fixnum}
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Set the number of persistent slots of
 \\funarg{class-description}\\ to
 \\funarg{persistent-slot-numbers}.
\\Seealsolabel
 Slot {\\bf p-persistent-slot-numbers} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-schema-evolution)
     (schema-evolution class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{schema-evolution}}
      {a keyword symbol}
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Set the schema evolution of
 \\funarg{class-description}\\ to
 \\funarg{schema-evolution}.
\\Seealsolabel
 Slot {\\bf p-schema-evolution} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-slot-numbers)
     (slot-numbers class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{slot-numbers}}
      {a fixnum}
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Set the number of total slots of
 \\funarg{class-description}\\ to
 \\funarg{slot-numbers}.
\\Seealsolabel
 Slot {\\bf p-slot-numbers} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-effective-slots)
     (slots class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{slots}}
      {a list of instances of
       \\fcite{structure-slot-description}\\ resp.\\ %
       \\fcite{effective-slot-description}}
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Sets the slots of
 \\funarg{class-description}\\ to
 \\funarg{slots}.
\\Seealsolabel
 Slot {\\bf p-slots} of \\fcite{structure-description};
 slot {\\bf p-effective-slots} of \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-time-stamp)
     (time-stamp class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{time-stamp}}
      {an integer}
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Sets the time stamp of
 \\funarg{class-description}\\ to
 \\funarg{time-stamp}.
\\Seealsolabel
 Slot {\\bf p-time-stamp} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf class-description-version-number)
     (version-number class-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{version-number}}
      {a fixnum}
 \\isa{\\funarg{class-description}}
      {a structure-description or a class-description}
\\Purposelabel
 Set the version number of
 \\funarg{class-description}\\ to
 \\funarg{version-number}.
\\Seealsolabel
 Slot {\\bf p-version-number} of
 \\fcite{structure-description}\\ resp.\\ \\fcite{class-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf slot-description-extent)
     (extent slot-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{extent}}
      {a keyword symbol}
 \\isa{\\funarg{slot-description}}
      {a structure-slot-description or a slot-description}
\\Purposelabel
 Set the extent of
 \\funarg{slot-description}\\ to
 \\funarg{extent}.
\\Seealsolabel
 Slot {\\bf p-extent} of \\fcite{structure-slot-description}\\ resp.\\ %
 \\fcite{slot-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf slot-description-deferred)
     (deferred slot-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{deferred}}
      {either \\lispnil\\ or a number}
 \\isa{\\funarg{slot-description}}
      {a structure-slot-description or a slot-description}
\\Purposelabel
 Set the deferred option of
 \\funarg{slot-description}\\ to
 \\funarg{deferred}.
\\Seealsolabel
 Slot {\\bf p-deferred} of \\fcite{structure-slot-description}\\ resp.\\ %
 \\fcite{slot-description}."))

;;; ---------------------------------------------------------------------------
#-:lisp-doc
(defgeneric (setf slot-description-index)
     (index slot-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{index}}
      {a keyword symbol}
 \\isa{\\funarg{slot-description}}
      {a structure-slot-description or a slot-description}
\\Purposelabel
 Set the index of
 \\funarg{slot-description}\\ to
 \\funarg{index}.
\\Seealsolabel
 Slot {\\bf p-index} of \\fcite{slot-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf slot-description-initargs)
     (initargs slot-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{initargs}}
      {a [list of] [keyword] symbol[s]}
 \\isa{\\funarg{slot-description}}
      {a structure-slot-description or a slot-description}
\\Purposelabel
 Set the initarg[s] of
 \\funarg{slot-description}\\ to
 \\funarg{initargs}.
\\Seealsolabel
 Slot {\\bf p-initarg[s]} of \\fcite{structure-slot-description}\\ resp.\\ %
 \\fcite{slot-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf slot-description-location)
     (location slot-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isanobject{\\funarg{location}}
 \\isa{\\funarg{slot-description}}
      {a structure-slot-description or a slot-description}
\\Purposelabel
 Set the location of
 \\funarg{slot-description}\\ to
 \\funarg{location}.
\\Seealsolabel
 Slot {\\bf p-location} of
 \\fcite{structure-slot-description}\\ resp.\\ %
 \\fcite{effective-slot-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf slot-description-name)
     (name slot-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{name}}
      {a symbol}
 \\isa{\\funarg{slot-description}}
      {a structure-slot-description or a slot-description}
\\Purposelabel
 Set the name of
 \\funarg{slot-description}\\ to
 \\funarg{name}.
\\Seealsolabel
 Slot {\\bf p-name} of
 \\fcite{structure-slot-description}\\ resp.\\ %
 \\fcite{slot-description}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf slot-description-type)
     (type slot-description)
  #+:lisp-doc (:documentation
   "
\\Argumentslabel
 \\isa{\\funarg{type}}
      {a type expression}
 \\isa{\\funarg{slot-description}}
      {a structure-slot-description or a slot-description}
\\Purposelabel
 Set the type of
 \\funarg{slot-description}\\ to
 \\funarg{type}.
\\Seealsolabel
 Slot {\\bf p-type} of \\fcite{structure-slot-description}\\ resp.\\ %
 \\fcite{slot-description}."))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
