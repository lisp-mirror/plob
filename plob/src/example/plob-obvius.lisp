;;;; -*-Lisp-*-----------------------------------------------------------------
;;;; Module	plob-obvius.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	14.4.94
;;;; Description	Interface between PLOB and OBVIUS.
;;;; --------------------------------------------------------------------------

(in-package :plob)

;;; ---------------------------------------------------------------------------
;;; OBVIUS stuff
;;; ---------------------------------------------------------------------------

(unless (find-package :lispview)
  (make-package :lispview))

(setf (package-extent (find-package :lispview)) :transient)

(unless (find-package :obvius)
  (make-package :obvius))

(when (find-class 'obvius::viewable nil)
  (finalize-inheritance (find-class 'obvius::viewable))
  (setf (slot-extent 'obvius::pictures-of (find-class 'obvius::viewable))
        :transient))
(when (find-class 'obvius::image nil)
  (finalize-inheritance (find-class 'obvius::image))
  (setf (class-constructor (find-class 'obvius::image))
        'make-obvius-image))

(defun make-obvius-image (objid &optional depth p-heap)
  (declare (ignore depth p-heap))
  (let ((image (make-instance 'obvius::image
			      :data (slot-value objid 'obvius::data))))
    (register-to-cache objid image)
    (setf (slot-value image 'obvius::name)
	  (slot-value objid 'obvius::name))
    (setf (slot-value image 'obvius::display-type)
	  (slot-value objid 'obvius::display-type))
    (setf (slot-value image 'obvius::history)
	  (slot-value objid 'obvius::history))
    (setf (slot-value image 'obvius::info-list)
	  (slot-value objid 'obvius::info-list))
    (setf (slot-value image 'obvius::current)
	  (slot-value objid 'obvius::current))
    (setf (slot-value image 'obvius::superiors-of)
	  (slot-value objid 'obvius::superiors-of))
    image))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
