;;;; --------------------------------------------------------------------------
;;;; Module	plob-movis.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		kirschke@informatik.uni-hamburg.de
;;;; Date	1996/11/27
;;;; Description	Storing of MOVIS objects with PLOB
;;;; --------------------------------------------------------------------------

(in-package :cl-user)

(use-package :plob)

;;; ---------------------------------------------------------------------------
;;; Write-lock to whole persistent heap:
#+never
(plob::write-lock *default-persistent-heap*
                  plob::+null-objid+
                  nil :nocache plob::+null-type-tag+
                  plob::+lock-store-write+)

;;; ---------------------------------------------------------------------------
(defun setup-movis-classes ()
  (p-delete-class 'movis::movis-line)
  (p-delete-class 'movis::neighborhood-description)
  (p-delete-class 'movis::deposit-region)
  (p-delete-class 'movis::convex-polygon)
  (p-delete-class 'coordinates::point)
  (p-delete-class 'movis::2d-point)
  (clear-cache)
  (setf (class-dependent (find-class 'movis::movis-line)) nil)
  (setf (class-dependent (find-class 'movis::neighborhood-description))
	:read)
  (setf (class-dependent (find-class 'movis::deposit-region))
	:read)
  (setf (class-dependent (find-class 'movis::convex-polygon))
	:read)
  (setf (class-dependent (find-class 'coordinates::point)) :read)
  (setf (class-dependent (find-class 'movis::2d-point)) :read)
  (setf (class-extent (find-class 'movis::2d-point)) :cached)
  (setf (slot-extent 'movis::x (find-class 'movis::2d-point)) :cached)
  (setf (slot-extent 'movis::y (find-class 'movis::2d-point)) :cached)
  (store-object (find-class 'movis::movis-line)) 
  (store-object (find-class 'movis::neighborhood-description))
  (store-object (find-class 'movis::deposit-region))
  (store-object (find-class 'movis::convex-polygon))
  (store-object (find-class 'coordinates::point))
  (store-object (find-class 'movis::2d-point)))

;;; ---------------------------------------------------------------------------
(defun deposits-to-vectors (lines)
  (flet ((deposit-to-vector
          (d)
	  (let* ((polygon (slot-value d 'movis::polygon))
                 (points (slot-value polygon 'movis::points)))
            (unless (vectorp points)
              (setf (slot-value polygon 'movis::points)
                    (coerce points 'vector)))
            (map nil
                 #'(lambda (point)
                     (let ((x (movis::point-x point)))
                       (unless (or (short-float-p x)
                                   (not (floatp x)))
                         (setf (movis::point-x point)
                               (coerce x 'short-float))))
                     (let ((y (movis::point-y point)))
                       (unless (or (short-float-p y)
                                   (not (floatp y)))
                         (setf (movis::point-y point)
                               (coerce y 'short-float)))))
                 points))))

  (loop for l in lines
	do
        (let ((initial-deposit (slot-value l 'movis::initial-deposit)))
          (when initial-deposit
            (deposit-to-vector initial-deposit)))
        (let ((pair-deposits (slot-value l 'movis::pair-deposits)))
          (when pair-deposits
            (loop for d being each hash-value in pair-deposits
                  do
                  (when d
                    (deposit-to-vector d)))))))
  (values))

;;; ---------------------------------------------------------------------------
(defun store-1-object (o)
  (format t "About to store ~A~%" o)
  (store-object o))

;;; ---------------------------------------------------------------------------
(defun movis-store-p-1 ()
  (plob::assert-open-session-p)
  (let ((plob::*default-depth* :cached))
    (declare (special plob::*default-depth*))
    (setup-movis-classes)
    (end-transaction t)
    (time
     (progn
       (begin-transaction)
       (let ((pair-deposits (loop for l in *v*
                                  collect
                                  (slot-value l 'movis::pair-deposits))))
         (loop for l in *v*
	       do
               (setf (slot-value l 'movis::pair-deposits) nil))
	 (unwind-protect
             (progn
               (plob::write-lock *default-persistent-heap*
                                 plob::+null-objid+
                                 nil :nocache plob::+null-type-tag+
                                 plob::+lock-store-write+)
               (set-up-profiler :package '(:plob)
                                :kind :profile
                                :interval 10)
               (profile (setf #lmovis::movis64d10l *v*))
               )
           (loop for l in *v*
                 for d in pair-deposits
	         do
                 (setf (slot-value l 'movis::pair-deposits) d)))
         (unwind-protect
             (progn
               (set-up-profiler :package '(:plob)
                                :kind :profile
                                :interval 10)
               (profile
                 (loop for d in pair-deposits
                       for i from 0 below 10
                       do
                       (format t "Storing ~A ~A~%" i d)
                       (store-object d))))
	   (end-transaction))))))
  nil)

;;; ---------------------------------------------------------------------------
(defun movis-store-1 ()
  (plob::assert-open-session-p)
  (let ((plob::*default-depth* :cached))
    (declare (special plob::*default-depth*))
    (setup-movis-classes)
    (end-transaction t)
    (time
     (progn
       (begin-transaction)
       (unwind-protect
	   (progn
	     (plob::write-lock *default-persistent-heap*
			       plob::+null-objid+
			       nil :nocache plob::+null-type-tag+
			       plob::+lock-store-write+)
	     (setf #lmovis::movis64d10l *v*)
	     )
	 (progn
	   (end-transaction))))))
  nil)

;;; ---------------------------------------------------------------------------
(defun movis-store-p-2 ()
  (plob::assert-open-session-p)
  (let ((plob::*default-depth* :nocache))
    (declare (special plob::*default-depth*))
    (setup-movis-classes)
    (end-transaction t)
    (time
     (with-transaction ()
       (plob::write-lock *default-persistent-heap*
			 plob::+null-objid+
			 nil :nocache plob::+null-type-tag+
			 plob::+lock-store-write+)
       (let ((objids (copy-seq *v*))
             (pair-deposits (loop for l in *v*
                                  collect
                                  (slot-value l 'movis::pair-deposits))))
         (loop for l in *v*
	       do
               (setf (slot-value l 'movis::pair-deposits) nil))
	 (unwind-protect
             (progn
               (set-up-profiler :package '(:plob)
                                :kind :profile
                                :interval 10)
               (profile (loop for l in *v*
                              for i from 0
                              do
                              (setf (elt objids i)
                                    (store-object l))))
               (loop for o in objids
		     for l in *v*
                     do
                     (plob::register-to-cache o l))
               )
           (loop for l in *v*
                 for d in pair-deposits
	         do
                 (setf (slot-value l 'movis::pair-deposits) d)))
         (setf plob::*default-depth* :cached)
	 (set-up-profiler :package '(:plob)
			  :kind :profile
			  :interval 10)
         #+never
	 (profile (store-1-object (elt pair-deposits 0))
		  (store-1-object (elt pair-deposits 1))
		  (store-1-object (elt pair-deposits 2))
		  (store-1-object (elt pair-deposits 3))
		  ;;(store-1-object (elt pair-deposits 4))
		  ;;(store-1-object (elt pair-deposits 5))
		  ;;(store-1-object (elt pair-deposits 6))
		  ;;(store-1-object (elt pair-deposits 7))
		  ;;(store-1-object (elt pair-deposits 8))
		  )
         ;; #+never
         (profile
	  (loop for d in pair-deposits
                for i from 0 below 10
                do
	        (format t "Storing ~A ~A~%" i d)
	        (store-object d)))))))
  nil)

;;; ---------------------------------------------------------------------------
(defun movis-store-2 ()
  (plob::assert-open-session-p)
  (let ((plob::*default-depth* :nocache))
    (declare (special plob::*default-depth*))
    (setup-movis-classes)
    (time
     (progn
       (setf #lmovis::movis64d10l *v*)
       nil))))

;;; ---------------------------------------------------------------------------
(defun movis-read-1 ()
  (plob::assert-open-session-p)
  (setf *w* nil)
  (clear-cache)
  (p-find-class 'movis::movis-line)
  (p-find-class 'movis::neighborhood-description)
  (p-find-class 'coordinates::point)
  (time
   (progn
     (begin-transaction)
     (unwind-protect
	 (progn
	   (plob::read-lock *default-persistent-heap*
			    plob::+null-objid+
			    :nocache
                            plob::+null-type-tag+
			    plob::+lock-store-read+)
	   (setf *w* #lmovis::movis64d10l))
       (end-transaction))
     nil)))

;;; ---------------------------------------------------------------------------
(defun movis-read-2 ()
  (plob::assert-open-session-p)
  (setf *w* nil)
  (clear-cache)
  (p-find-class 'movis::movis-line)
  (p-find-class 'movis::neighborhood-description)
  (p-find-class 'coordinates::point)
  (time
   (progn
     (setf *w* #lmovis::movis64d10l)
     nil)))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
