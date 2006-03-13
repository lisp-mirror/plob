;;;; -*- Package: CL-USER; Mode: LISP; Syntax: ANSI-Common-Lisp -*-------------
;;;; Module	plob-toplevel.lisp
;;;; Author	Heiko Kirschke
;;;; Copyright	(C) 1998 Heiko Kirschke
;;;; Date	1998/01/26
;;;; Description
;;;;
;;;;	Allegro top level aliases for PLOB. Adapt following code and put
;;;;	it into your clinit.cl LISP startup file.
;;;;
;;;; Copyright	PLOB! Copyright 1994--2006 Heiko Kirschke.
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
;;;; $Id$
;;;;
;;;; --------------------------------------------------------------------------

(setf *print-readably* nil)
(load "~/plob-2.09/src/allegro/defsystem-plob.lisp")

;; Comment out following line if you do not need to compile PLOB with
;; every startup:
(compile-plob)

(load-plob)

#+Allegro
(top-level:alias "o" (&optional (url plob::*database-url*))
  (close-heap)
  (when (symbolp url)
    (setf url (string-downcase (symbol-name url))))
  (open-my-session url))

#+Allegro
(top-level:alias "c" ()
    (close-heap))

#+Allegro
(top-level:alias "s" ()
    (show-sessions))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
