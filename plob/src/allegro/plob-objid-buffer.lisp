;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-objid-buffer.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	1996/11/25
;;;; Description	Buffering of objids and type tags
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

(in-package :plob)

;;; ---------------------------------------------------------------------------
;;; Constants
;;; ---------------------------------------------------------------------------
(defparameter *objid-buffer-size* 128
  #+:lisp-doc "The max.\\ number of \\objid{}s and \\typetag{}s
 to hold in a single objid buffer.")

;;; ---------------------------------------------------------------------------
;;; Types
;;; ---------------------------------------------------------------------------
(defstruct (objid-buffer
            (:constructor
             make-objid-buffer (objid heap objids type-tags)))
  #+:lisp-doc "
\\Purposelabel
 A structure holding an \\objid\\ buffer.
 An \\objid\\ buffer is used for a more efficient communication between
 LISP and the \\plob\\ C client code. Since the foreign function calls
 to C are rather expensive, an \\objid\\ buffer holds \\emph{all}
 \\objid{}s and \\typetag{}s contained in a persistent object (so LISP
 doesn't read/write each of it with an extra call)."
  (objid nil
	 #+:lisp-doc :documentation #+:lisp-doc "
 The buffered persistent object.")
  (heap nil
	#+:lisp-doc :documentation #+:lisp-doc "
 The heap to use for \\objid\\ input/output.")
  (dirty nil
	 #+:lisp-doc :documentation #+:lisp-doc "
 A flag if the \\objid\\ buffer has been written to.")
  (start nil
	 #+:lisp-doc :documentation #+:lisp-doc "
 The slot index into the referenced persistent object.")
  (current-size 0
		#+:lisp-doc :documentation #+:lisp-doc "
 The current size of the \\objid\\ and \\typetag\\ buffer.")
  (objids nil
	  #+:lisp-doc :documentation #+:lisp-doc "
 The buffer holding the \\objid{}s")
  (type-tags nil
	     #+:lisp-doc :documentation #+:lisp-doc "
 The buffer holding the \\typetag{}s"))

;;; ---------------------------------------------------------------------------
;;; Functions: Flush an objid buffer
;;; ---------------------------------------------------------------------------
(defun flush-objid-buffer (objid-buffer)
  #+:lisp-doc "Flush an \\objid\\ buffer if it was modified."
  (let ((dirty (objid-buffer-dirty objid-buffer)))
    (when dirty
      (let* ((current-size (objid-buffer-current-size objid-buffer))
             (written-objids (sh-write-indices
			      (objid-buffer-heap objid-buffer)
			      (objid-buffer-objid objid-buffer)
			      (objid-buffer-start objid-buffer)
			      current-size
			      (objid-buffer-objids objid-buffer)
			      (objid-buffer-type-tags objid-buffer)
			      +null-objid+ +null-type-tag+)))
	#+:never
	(format t "Flushed objid ~A, start ~A, ~
		   number of objids = ~A, wrote ~A objids~%"
		(make-persistent-object (objid-buffer-objid objid-buffer))
		(objid-buffer-start objid-buffer)
		current-size written-objids)
        (setf (objid-buffer-dirty objid-buffer) nil)
	;; 1998/05/04 HK: Very strange, in Allegro 4.3.1 on Linux,
	;; sh-write-indices returns not the correct number of words
	;; written when called directly after a garbage collection
	;; has been done.
	#-:allegro
        (unless (= written-objids current-size)
          (warn "Expected to write ~A objids, wrote ~A objids to ~A."
		current-size written-objids
		(make-persistent-object
		 (objid-buffer-objid objid-buffer))))))
    dirty))

;;; ---------------------------------------------------------------------------
;;; Functions: Open/close an objid buffer
;;; ---------------------------------------------------------------------------
(defun compute-objid-buffer-size (suggested-size)
  #+:lisp-doc "Compute an optimal size for an \\objid\\ buffer.
\\Seealsolabel
  \\Fcite{objid-buffer}"
  (cond
   ((null suggested-size)
    *objid-buffer-size*)
   ((<= suggested-size *objid-buffer-size*)
    suggested-size)
   (t
    (ceiling suggested-size (ceiling suggested-size *objid-buffer-size*)))))

;;; ---------------------------------------------------------------------------
(defun open-objid-buffer
     (p-objid &optional suggested-size (p-heap *default-persistent-heap*))
  #+:lisp-doc "Open an \\objid-buffer.
\\Seealsolabel
  \\Fcite{objid-buffer}"
  (let ((size (compute-objid-buffer-size suggested-size)))
    (make-objid-buffer (persistent-object-objid p-objid)
		       (persistent-object-objid p-heap)
                       (make-int-vector size nil)
                       (make-int-vector size nil))))

;;; ---------------------------------------------------------------------------
(defun close-objid-buffer (objid-buffer)
  #+:lisp-doc "Close an \\objid\\ buffer.
\\Seealsolabel
  \\Fcite{objid-buffer}"
  (unwind-protect
      (flush-objid-buffer objid-buffer)
    (let ((objid (objid-buffer-objid objid-buffer))
	  (objids (objid-buffer-objids objid-buffer))
	  (type-tags (objid-buffer-type-tags objid-buffer)))
      (when type-tags
        (free-int-vector type-tags)
        (setf (objid-buffer-type-tags objid-buffer) nil))
      (when objids
        (free-int-vector objids)
        (setf (objid-buffer-objids objid-buffer) nil))
      (when objid
	(flush-object objid t (objid-buffer-heap objid-buffer))
	(setf (objid-buffer-objid objid-buffer) nil)
	(setf (objid-buffer-heap objid-buffer) nil)))))

;;; ---------------------------------------------------------------------------
;;; Functions: Read/write an objid buffer
;;; ---------------------------------------------------------------------------
(defun read-objid-buffer (objid-buffer at-index)
  #+:lisp-doc "
 Read an \\objid\\ and a \\typetag\\ from \\funarg{objid-buffer}.
\\Seealsolabel
  \\Fcite{objid-buffer}"
  (let* ((objids (objid-buffer-objids objid-buffer))
	 (type-tags (objid-buffer-type-tags objid-buffer))
         (start (objid-buffer-start objid-buffer))
         (offsetted-index (if start (- at-index start) 0)))
    (unless (and start
                 (<= 0 offsetted-index)
		 (< offsetted-index (objid-buffer-current-size objid-buffer)))
      (flush-objid-buffer objid-buffer)
      ;; The index doesn't point into the current buffer; reload
      ;; the buffer starting at index at-index:
      (let ((read-objids 0))
        (if (or (minusp offsetted-index)
                (and (null start) (plusp at-index)))
            ;; The index seems to run backwards, so do a backward read:
	  (let* ((size (int-vector-length objids))
		 (start (max 0 (- at-index size -1))))
	    (setf read-objids (sh-read-indices-into
			       (objid-buffer-heap objid-buffer)
			       (objid-buffer-objid objid-buffer)
			       start size objids type-tags
			       +null-objid+ +null-type-tag+))
	    (setf (objid-buffer-start objid-buffer) start)
	    (setf (objid-buffer-current-size objid-buffer) read-objids)
	    (setf offsetted-index (- at-index start)))
	  ;; The index seems to run forward, so do a forward read:
	  (progn
	    (setf read-objids (sh-read-indices-into
			       (objid-buffer-heap objid-buffer)
			       (objid-buffer-objid objid-buffer)
			       at-index (int-vector-length objids)
			       objids type-tags +null-objid+ +null-type-tag+))
	    (setf (objid-buffer-start objid-buffer) at-index)
	    (setf (objid-buffer-current-size objid-buffer) read-objids)
	    (setf offsetted-index 0)))
	(when (<= read-objids 0)
	  (error "Can't index ~A with index ~A."
		 (make-persistent-object (objid-buffer-objid objid-buffer))
		 at-index))))
    ;; Now, the index points into the current buffer:
    (values (int-vector objids offsetted-index)
	    (int-vector type-tags offsetted-index))))

;;; ---------------------------------------------------------------------------
(defun write-objid-buffer (objid-buffer at-index objid type-tag)
  #+:lisp-doc "Write an \\objid\\ and a \\typetag\\ to \\funarg{objid-buffer}.
\\Seealsolabel
  \\Fcite{objid-buffer}"
  (let* ((objids (objid-buffer-objids objid-buffer))
	 (type-tags (objid-buffer-type-tags objid-buffer))
         (start (objid-buffer-start objid-buffer))
         (size (int-vector-length objids))
         (offsetted-index (if start (- at-index start) 0)))
    (unless (and start (<= 0 offsetted-index (1- size)))
      (flush-objid-buffer objid-buffer)
      ;; The index doesn't point into the current buffer; reinitialize
      ;; the buffer:
      (int-vector-fill objids +ignore-slot-tag+)
      (int-vector-fill type-tags +ignore-slot-tag+)
      (setf (objid-buffer-current-size objid-buffer) 0)
      (if (or (minusp offsetted-index)
	      (and (null start) (plusp at-index)))
	  ;; The index seems to run backwards, so do a backward write:
	  (let ((start (max 0 (- at-index size -1))))
	    (setf (objid-buffer-start objid-buffer) start)
	    (setf offsetted-index (- at-index start)))
	  ;; The index seems to run forward, so do a forward write:
          (progn
            (setf (objid-buffer-start objid-buffer) at-index)
            (setf offsetted-index 0))))
    (setf (objid-buffer-dirty objid-buffer) t)
    (setf (int-vector objids offsetted-index) objid)
    (setf (int-vector type-tags offsetted-index) type-tag)
    (setf (objid-buffer-current-size objid-buffer)
          (max (objid-buffer-current-size objid-buffer)
	       (1+ offsetted-index)))))

;;; ---------------------------------------------------------------------------
;;; Macros
;;; ---------------------------------------------------------------------------
(defmacro with-objid-buffer
     ((objid-buffer p-objid suggested-size p-heap)
      &body body)
  #+:lisp-doc "Make an environment for \\funarg{body}\\ where
 \\funarg{objid-buffer}\\ references an open \\objid\\ buffer.
\\Seealsolabel
  \\Fcite{objid-buffer}"
  (let ((done (gensym "DONE-"))
        (size (gensym "SIZE-"))
        (objids (gensym "OBJIDS-"))
        (type-tags (gensym "TYPE-TAGS-"))
        (result (gensym "RESULT-")))
    `(let* ((,done      nil)
            (,size      (compute-objid-buffer-size ,suggested-size))
            (,objids    (make-int-vector ,size nil))
            (,type-tags (make-int-vector ,size nil))
            (,objid-buffer
             (make-objid-buffer (persistent-object-objid ,p-objid)
                                (persistent-object-objid ,p-heap)
                                ,objids ,type-tags))
            (,result nil))
       #-:lispworks4 ;; and hopefully not later
       (declare (dynamic-extent ,done ,size
				,objids ,type-tags ,objid-buffer))
       (unwind-protect
           (progn
             (setf ,result (multiple-value-list (progn ,@body)))
             (setf ,done t))
         (progn
           (if ,done
               (close-objid-buffer ,objid-buffer)
	     (progn
	       (free-int-vector ,type-tags)
	       (free-int-vector ,objids)))))
       (values-list ,result))))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
