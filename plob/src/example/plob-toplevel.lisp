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
