;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-defaults.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@informatik.uni-hamburg.de
;;;; Date	1996/10/02
;;;; Description	Defaults for PLOB!
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
;;; RPC server: transport protocol to use, host and remote directory
;;; ---------------------------------------------------------------------------
(defparameter *default-database-url* "tcp://localhost/database"
  #+:lisp-doc "
\\Purposelabel
 The URL naming the default database. The actual database is found
 in \\fcite{*database-url*}.
\\Seealsolabel
  \\Fcite{*database-url*}.")

;;; ---------------------------------------------------------------------------
(defparameter *database-url* ""
  #+:lisp-doc "
\\Purposelabel
 The URL naming the actual database.
 At opening, the values of \\fcite{*database-url*}\\ and
 \\fcite{*default-database-url*}\\ are merged by a call to
 \\fcite{merge-urls}\\ to form the effective database URL.
 So, if you want to open another database instead of the default one,
 it is a good idea to set \\fcite{*database-url*}\\ to the URL
 of this database.
\\Seealsolabel
  \\Fcite{*default-database-url*}.")

;;; ---------------------------------------------------------------------------
(defvar *verbose* 4
  #+:lisp-doc "
\\Purposelabel
 A variable indicating the amount of messaging to provide by \\plob.
 Following values are defined:
 \\begin{description}
 \\item[0 \\protect\\textrm{or} \\protect\\lispnil]
  Do not show any messages at all and suppress all warnings. All
  cerrors are continued.
 \\item[1] Show all warnings; cerrors will be raised.
 \\item[2] Show the storing and updating of class metaobjects.
  Those messages will have a `;;;;;' prefix.
 \\item[3] Show the bootstrap messages.
  Those messages will have a `;;;;' prefix.
 \\item[4] Show all messages returned from the server.
  Those messages will have a `;;;' prefix.
 \\item[5] Show the storing and loading of structure and
  \\clos\\ instances. Those messages will have a `;;' prefix.
 \\item[6] Show the storing and loading of non-structure and
  non-\\clos\\ instances. Those messages will have a `;' prefix.
 \\item[7] Show the storing and loading of \\clos\\ slots.
  Those messages will have a `;' prefix, too.
 \\end{description}")

;;; ---------------------------------------------------------------------------
;;; Schema evolution support
;;; ---------------------------------------------------------------------------
(defparameter *default-structure-schema-evolution*
  :write-back-deny-identity-change
  #+:lisp-doc "
\\Purposelabel
 The default schema evolution to use for
 \\cl\\ \\lisp{defstruct}\\ objects.
\\Seealsolabel
 \\Fcite{(setf schema-evolution)};
 \\fcite{schema-evolution}.")

;;; ---------------------------------------------------------------------------
(defparameter *default-clos-schema-evolution* :write-back
  #+:lisp-doc "
\\Purposelabel
 The default schema evolution to use for
 \\clos\\ objects.
\\Seealsolabel
 \\Fcite{(setf schema-evolution)};
 \\fcite{schema-evolution}.")

;;; ---------------------------------------------------------------------------
;;; Slot extents
;;; ---------------------------------------------------------------------------
(defparameter *default-structure-slot-extent* :cached-demand-load
  #+:lisp-doc "
\\Purposelabel
 The default extent for slots of \\cl\\ \\lisp{defstruct}\\ objects.
\\Seealsolabel
 \\Fcite{(setf slot-extent)};
 \\fcite{slot-extent}.")

;;; ---------------------------------------------------------------------------
(defparameter *default-clos-slot-extent* :cached
  #+:lisp-doc "
\\Purposelabel
 The default extent
 for slots of \\clos\\ objects whose class object was created by a
 \\lisp{defclass}-statement without a
 \\lisp{(:metaclass persistent-metaclass)}\\ class option,
 i.e.\\ for \\clos\\ objects without direct \\plob\\ support.
 These are e.g.\\ always all classes created before \\plob\\ was started,
 like all system-defined classes etc.
\\Seealsolabel
 \\Fcite{(setf slot-extent)};
 \\fcite{slot-extent}.")

;;; ---------------------------------------------------------------------------
(defparameter *default-plob-slot-extent* :cached-write-through
  #+:lisp-doc "
\\Purposelabel
 The default extent
 for slots of \\clos\\ objects whose class object was created by a
 \\lisp{defclass}-statement with a
 \\lisp{(:metaclass persistent-metaclass)} class option,
 i.e.\\ for \\clos\\ objects with direct \\plob\\ support.
\\Seealsolabel
 \\Fcite{(setf slot-extent)};
 \\fcite{slot-extent}.")

;;; ---------------------------------------------------------------------------
;;; Support for `true' short floats
;;; ---------------------------------------------------------------------------
(defconstant +has-short-float-p+
  (eq (type-of 1.0s0) 'short-float)
    "
\\Purposelabel
 A flag if the used LISP system supports short float numbers,
 i.e.\\ if the type \\textbf{short-float} is a true subtype
 of type \\textbf{single-float}.")

(when +has-short-float-p+
  (pushnew :short-float *features*))

;;; ---------------------------------------------------------------------------
;;; LISP system name
;;; ---------------------------------------------------------------------------
(defconstant +lisp-symbolic-system-name+
    #+:lispworks3 :lispworks3
    #+:lispworks4 :lispworks4
    #+(and :allegro (version>= 5))       :allegro5
    #+(and :allegro (not (version>= 5))) :allegro4
    "
\\Purposelabel
 The name of the current LISP system as a keyword symbol.")

;;; ---------------------------------------------------------------------------
;;; Storing/loading of objects
;;; ---------------------------------------------------------------------------
(defparameter *default-depth* :flat
  #+:lisp-doc "
\\Purposelabel
 Default depth used for storing and loading of objects.
\\Seealsolabel
 \\Fcite{store-object}, \\fcite{load-object}.")

;;; ---------------------------------------------------------------------------
;;; Caching of numbers
;;; ---------------------------------------------------------------------------
(defparameter *cache-numbers* nil
  #+:lisp-doc "
\\Purposelabel
  A flag if numbers (i.e.\\ single and double floats, bignums)
  should be cached. Not caching numbers will result
  in identical (in the sense of \\lisp{eq}) numbers stored
  more than once in the persistent heap.")

;;; ---------------------------------------------------------------------------
;;; Caching of strings
;;; ---------------------------------------------------------------------------
(defparameter *cache-strings* t
  #+:lisp-doc "
\\Purposelabel
  A flag if strings should be cached. Not caching strings will result
  in identical (in the sense of \\lisp{eq}) strings stored
  more than once in the persistent heap.")

;;; ---------------------------------------------------------------------------
;;; Lock conflict timeouts
;;; ---------------------------------------------------------------------------
(defparameter *suspend-timeout* 2
  #+:lisp-doc "
\\Purposelabel
 Maximum time to wait in seconds when the \\cl\\ session receives
 a suspend request.
%% \\Seealsolabel
%%  \\Fcite{sh-suspend-callback}.
")

;;; ---------------------------------------------------------------------------
(defparameter *in-bootstrap-wait-timeout* 60
  #+:lisp-doc "
\\Purposelabel
 The max.\\ time in seconds a LISP process will wait until the process
 doing the \\plob\\ bootstrap will have finished the bootstrap.
\\Seealsolabel
 \\Fcite{*in-bootstrap-p*};
 \\fcite{open-heap}.")


;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
