;;;; -*- Package: CL-USER; Mode: LISP; Syntax: ANSI-Common-Lisp; Readtable: plob-readtable -*-
;;;; Module	plob-quick-tour.lisp
;;;; Author	Heiko Kirschke
;;;; Date	1998/01/26
;;;; Description
;;;;
;;;;	Example code for chapter 2 of PLOB's User's Guide.
;;;;
;;;; Copyright	PLOB! Copyright 1994--2001 Heiko Kirschke.
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
;;;; $Header$
;;;;
;;;; --------------------------------------------------------------------------

;;;; ----------------------------------------------------------------
;;;; Starting LISP and loading PLOB
;;;; ----------------------------------------------------------------

(load "defsystem-plob")
(load-plob)

;;; If you have loaded PLOB by evaluating the previous two statements,
;;; you should now do a:
;;;	M-x revert-buffer
;;; in order to get the right readtable; otherwise, the LISP reader
;;; will signal an error on each `#!' encountered.

;;; In Allegro, evaluate the following expression to display the
;;; results of evaluating the example statements in an emacs buffer
;;; for the rest of this file:
#+:Allegro
(lep::eval-in-emacs
 "(progn
    (make-local-variable 'fi:lisp-evals-always-compile)
    (setq fi:lisp-evals-always-compile nil))")

;;; In LispWorks 4.0.1 on Windows/NT, for looking at the output
;;; switch to the `Output' buffer by pressing the `Output' tab in the
;;; window's caption bar after each evaluation.

;;;; ----------------------------------------------------------------
;;;; Opening the database
;;;; ----------------------------------------------------------------

(open-my-session)

;;;; ----------------------------------------------------------------
;;;; Showing other sessions active on the database
;;;; ----------------------------------------------------------------

(show-sessions)

;;;; ----------------------------------------------------------------
;;;; Defining a persistent class
;;;; ----------------------------------------------------------------

(defclass person ()
  ((name :initarg :name :initform nil :accessor person-name)
   (soc-sec-# :initarg :soc-sec-#
	      :accessor person-soc-sec-#
	      :index (btree :test equal)))
  (:metaclass persistent-metaclass))

;;;; ----------------------------------------------------------------
;;;; Binding to a persistent symbol
;;;; ----------------------------------------------------------------

(setf #!*cleese* (make-instance 'person :name "Cleese" :soc-sec-# 2000))

;;;; ----------------------------------------------------------------
;;;; Making persistent instances
;;;; ----------------------------------------------------------------

(make-instance 'person :name "Palin" :soc-sec-# 3000)
(make-instance 'person :name "Gilliam" :soc-sec-# 4000)

;;;; ----------------------------------------------------------------
;;;; Doing simple queries
;;;; ----------------------------------------------------------------

(p-select 'person :where 'soc-sec-# := 2000)
(person-name (p-select 'person :where 'soc-sec-# := 2000))

;;;; ----------------------------------------------------------------
;;;; Defining a print method
;;;; ----------------------------------------------------------------

(defmethod print-object ((obj person) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~A (short-objid ~A)"
	    (if (slot-boundp obj 'name) (person-name obj))
	    (persistent-object-objid obj))))
(p-select 'person :where 'soc-sec-# := 2000)

;;;; ----------------------------------------------------------------
;;;; Leaving LISP
;;;; ----------------------------------------------------------------

;; [type `:exit' in a listener]

;;;; ----------------------------------------------------------------
;;;; Restarting the system; reload of classes
;;;; ----------------------------------------------------------------

(load "defsystem-plob")
(load-plob)

(values #!*cleese*)

;;;; ----------------------------------------------------------------
;;;; Doing a schema evolution
;;;; ----------------------------------------------------------------

(defclass person ()
  ((name :initarg :name :initform nil :accessor person-name)
   (soc-sec-# :initarg :soc-sec-#
	      :accessor person-soc-sec-#
                          :index (btree :test equal))
   ;; Adding slot age:
   (age :initarg :age :initform nil :accessor person-age))
  (:metaclass persistent-metaclass))

(values #!*cleese*)

;;;; ----------------------------------------------------------------
;;;; Using transactions
;;;; ----------------------------------------------------------------

;; Initialize the person's age to 52:
(setf (person-age #!*cleese*) 52)

;; Committing transaction:

(with-transaction ()
  (setf (person-age #!*cleese*) 53)
  ;; some other code in between
  (setf (person-soc-sec-# #!*cleese*) 2001))

(format t "Age of ~A is ~A, soc-sec-# ~A~%"
	(person-name #!*cleese*)
	(person-age #!*cleese*)
	(person-soc-sec-# #!*cleese*))

;; Aborting transaction:

(with-transaction ()
  (setf (person-age #!*cleese*) 54)
  (setf (person-soc-sec-# #!*cleese*) 2002)
  (error "Something went wrong."))

;; Should be 53 and 2001:
(format t "Age of ~A is ~A, soc-sec-# ~A~%"
	(person-name #!*cleese*)
	(person-age #!*cleese*)
	(person-soc-sec-# #!*cleese*))

;;;; ----------------------------------------------------------------
;;;; Defining and using btrees
;;;; ----------------------------------------------------------------

(setf #!*btree* (make-btree :test 'equal :pagesize 16))
(setf (getbtree "Cleese" #!*btree*) #!*cleese*)
(setf (getbtree "myself" #!*btree*) "Heiko Kirschke")
(p-apropos-btree #!*btree*)

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
