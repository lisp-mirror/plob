;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-error.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	1997/03/11
;;;; Description	PLOB error handling functions
;;;;
;;;; Copyright	PLOB! Copyright 1994--2002 Heiko Kirschke.
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
;;; Error handling
;;; ---------------------------------------------------------------------------
(define-condition simple-postore-error (error)
  ((error-message :initarg :error-message :accessor error-message))
  #+:lisp-doc (:documentation "Error conditions for POSTORE."))

;;; ---------------------------------------------------------------------------
(defmethod print-object ((c simple-postore-error) s)
  (if *print-escape*
      (format s "#<~A ~A>" (class-name (class-of c)) (error-message c))
    (format s "~A" (error-message c))))

;;; ---------------------------------------------------------------------------
(define-condition postore-cerror (simple-postore-error)
  ()
  #+:lisp-doc (:documentation "Continuable error conditions for POSTORE."))

;;; ---------------------------------------------------------------------------
(define-condition postore-error (simple-postore-error)
  ()
  #+:lisp-doc (:documentation "Error conditions for POSTORE."))

;;; ---------------------------------------------------------------------------
(define-condition postore-fatal (simple-postore-error)
  ()
  #+:lisp-doc (:documentation "Fatal error conditions for POSTORE."))

;;; ---------------------------------------------------------------------------
;;; Variables
;;; ---------------------------------------------------------------------------
(defvar *ignore-info-messages-semaphore* 0
  #+:lisp-doc "
\\Purposelabel
 A semaphore used to suppress info messages during the active wait for
 a lock.")

;;; ---------------------------------------------------------------------------
;;; Functions
;;; ---------------------------------------------------------------------------
(defconstant +substitute-newline-default+ ";;; "
  "The default to be used in \\fcite{substitute-newline}.")

;;; ---------------------------------------------------------------------------
(defun substitute-newline (message
			   &optional (replace-by +substitute-newline-default+))
  #+:lisp-doc "
 Substitute sequences of newlines followed by spaces into a newline
 followed by two semicolons."
  (let ((substituted-message (copy-seq message))
        (length-left-part 0) (length-replace-by (length replace-by)))
    (loop for position-newline =
          (position #\Newline substituted-message
		    :start length-left-part)
          while position-newline
          finally (return substituted-message)
          as left-part = (subseq substituted-message 0 position-newline)
	  as right-part = (string-left-trim
			   '(#\Space #\Tab #\Newline)
			   (subseq substituted-message position-newline))
          do
	  (setf length-left-part (+ (length left-part) length-replace-by))
	  (setf substituted-message
	    (concatenate 'string
	      left-part (string #\Newline)
	      replace-by right-part)))))

;;; ---------------------------------------------------------------------------
(defun sh-signal-error (error-level continue-message error-message)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{error-level}}
      {a fixnum}
 \\isa{\\funarg{continue-message}}
      {a string}
 \\isa{\\funarg{error-message}}
      {a string}
\\Purposelabel
 The \\cl\\ callback called when an error occurres in
 the C level of \\plob.
\\Seealsolabel
 \\Fcite{sh-signal-cerror}, \\fcite{sh-signal-serror}."

  (cond
   ((= error-level +error-level-0+) nil)
   ((= error-level +error-level-info+)
    (when (and *verbose* (>= *verbose* 4)
               (= *ignore-info-messages-semaphore* 0))
       (format t ";;; ~A~%" (substitute-newline error-message))))
   ((= error-level +error-level-warn+)
    (when (and *verbose* (>= *verbose* 1))
      (write-string +substitute-newline-default+ *error-output*)
      (warn (substitute-newline error-message))))
   ((= error-level +error-level-cerror+)
    (if (and *verbose* (>= *verbose* 1))
      (restart-case
       (error 'postore-cerror :error-message error-message)
       (continue
	()
	:report
	(lambda (stream)
	  (write-string continue-message stream))
	(values))
       (make-error
	()
	:report
	"Resignal this as a non-continuable error to LISP to enable debugging."
	(error 'postore-error
	       :error-message (concatenate 'string
				"Resignaled: " error-message))))
      ;; 1998/12/02 HK: For Plob being silent, signal CERRORs from
      ;; lower layer as ERRORs, since they normally indicate some
      ;; communication problems with the server:
      (error 'postore-error :error-message error-message)))
   ((= error-level +error-level-error+)
    (error 'postore-error :error-message error-message))
   ((>= error-level +error-level-fatal+)
    (error 'postore-fatal :error-message error-message))))

;;; ---------------------------------------------------------------------------
;;; System-specific handling of error callbacks:
;;; ---------------------------------------------------------------------------
#+:lispworks
(defun sh-error-callback (error-level continue-message error-message)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{continue-message}\\ resp.\\ \\funarg{error-message}}
      {a string}
\\Purposelabel
 The \\lwcl\\ callback called when a continuable error occurres in
 the C level of \\plob.
\\Seealsolabel
 \\Fcite{sh-signal-cerror}."

  (sh-signal-error error-level continue-message error-message))

;;; ---------------------------------------------------------------------------
#+:allegro
(defconstant +error-message-buffer-size+ 2048
  #+:lisp-doc "
\\Purposelabel
 Size of the error message string buffers used for getting strings from
 the \\plob\\ low-level functions.")

;;; ---------------------------------------------------------------------------
#+:allegro
(defun sh-error-callback ()
  #+:lisp-doc "
\\Purposelabel
 The \\allegrocl\\ callback called when a server error occurres in
 the C level of \\plob."

  (let ((error-level (sh-get-error-level))
	(continue-message (make-string +error-message-buffer-size+
				       :initial-element #\Space))
	(error-message (make-string +error-message-buffer-size+
				    :initial-element #\Space)))
      (sh-get-error-continue continue-message +error-message-buffer-size+)
      (sh-get-error-message error-message +error-message-buffer-size+)
      (sh-signal-error error-level continue-message error-message)))

;;; ---------------------------------------------------------------------------
(defmacro catch-errors (&body forms)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{forms}}
      {an implicit \\lisp{progn}\\ expression
       with calls to \\plob\\ C level functions}
\\Valueslabel
 Returns the result of evaluating \\funarg{forms}.
\\Purposelabel
 Evaluates \\funarg{forms}\\ in an environment which catches all errors
 which might occure in the \\plob\\ C level functions; when an error is
 caught, control can be transferred out of the C functions back to
 the \\cl\\ system debugger.
\\Remarkslabel
 The errors caught are raised from the C level using the C macros
 \\lisp{CERROR}\\ and \\lisp{ERROR}. A situation which leads to raising
 an error from the C level is shown in figure~\\ref{tab:backtrace}.
 \\begin{figure}[htbp]\\centering%
 \\begin{tabular}{|c|l|p{10cm}|}%
 \\hline
 \\tabularheader{Level}
	&\\tabularheader{Environment}
		&\\tabularheader{Description}\\\\
 \\hline\\hline
 1 
  & LISP
   & This is in the LISP toplevel processing of {\\bf catch-errors};
     here the \\funarg{forms}\\ are evaluated which call the C level
     functions.\\\\
 \\hline
 2
  & C
   & During the processing of the C function called, an error is raised
     by the C macro \\lisp{ERROR}.\\\\
 \\hline
 3
 & LISP
  & The C macro \\lisp{ERROR}\\ transfers control to the error handler
    {\\bf sh-error-callback}, which  raises immediately an error condition
    of class \\class{postore-error}. This error condition is caught
    by {\\bf catch-errors} on level~1, i.e.\\ the process control is
g    thrown back to level~1.\\\\
 \\hline
 \\end{tabular}%
 \\caption{\\protect\\clogo\\ code error handling}%
 \\label{tab:backtrace}%
 \\end{figure}%
 The handling done by the C macro \\lisp{CERROR}\\ is very similar;
 the difference is that the process control is transferred from
 level~3 to level~1 only when the user requests so, because the
 transfer of control to level~1 makes a continuation of the C
 function impossible.

 The above error handling is done especially for \\lw\\ to enable
 a useful stack backtrace: When the \\cl\\ debugger is called
 already in level~3, the stack backtrace looks very poor, since it
 shows only the call to {\\bf sh-error-callback} but not the calls
 leading to and done in level~1, i.e.\\ the debugger cannot `see'
 the calls which happened before level~3.

 This error handling is only done for `soft' errors raised from the
 C level by using the C macros \\lisp{CERROR}\\ and
 \\lisp{ERROR}. `Hard' errors like de-refencing an invalid pointer
 in the C level often crash \\lw, sometimes without any further
 announcements. If \\lw\\ did not crash but shows something like
 \\begin{quote}\\tt
 Signal 10: Bus error
 \\end{quote}
 in the debugger, try to close \\plob\\ by calling
 \\fcite{close-heap}, leave and restart \\lw.
 An error handling for `hard' errors could only be done by integrating
 a complete C debugger into the C code, since \\lw\\ cannot debug C code.

 \\note\\ Uncontrolled writing into the binary representation of
 \\cl\\ objects (not done by \\plob) normally crashes the system
 at the next garbage collection.
\\Seealsolabel
 \\Fcite{sh-cerror-callback};
 \\fcite{sh-error-callback}."

  `(handler-case (progn ,@forms)
     (postore-error (condition)
       (error condition))
     (postore-fatal (condition)
       (cerror "Force a restart of the PLOB! daemon."
	       condition)
       (p-restart t))))

;;; ---------------------------------------------------------------------------
(defun handle-lock-error (p-heap-objid p-objid lock-error)
  #+:lisp-doc "
\\Purposelabel
 Signal an error for a failed lock request.
"
  (let* ((explanation (gethash lock-error *SHLOCK*))
	 (p-objid-text
	  (if (eql p-objid +null-objid+)
	      ""
	    (format nil " ~A" p-objid))))
    (if explanation
	(error "Locking~A by ~A returned error code ~A:~%       ~A"
               p-objid-text p-heap-objid lock-error explanation)
      (error "Locking ~A by ~A returned error code ~A."
             p-objid p-heap-objid lock-error))))

;;; ---------------------------------------------------------------------------
(defun handle-lock-conflict (lock-expression p-heap-objid p-objid
                                             lock-conflict)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{lock-expression}}
      {a function taking one argument, the \\funarg{p-objid}}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 If the lock operation is retried, the value returned by
 calling \\funarg{lock-expression}\\ with argument \\funarg{p-objid}.
\\Purposelabel
 Show a \\lisp{cerror}\\ which indicates that locking the persistent
 object referenced by \\funarg{p-objid}\\ failed. The
 \\funarg{lock-expression} argument is a function which contains
 the call to the C level which indicated the failed lock.
 The user is asked what to do next; this is one of:
 \\begin{itemize}

 \\item Retry the lock by calling \\funarg{lock-expression}\\ again
  with the \\funarg{p-objid}\\ argument in the hope that the
  conflicting lock has been relased in the meantime.

 \\item Remove all locks from \\funarg{p-objid}\\ brute-force
  and call \\funarg{lock-expression}\\ again with the
  \\funarg{p-objid}\\ argument to obtain the lock.

 \\end{itemize}
\\Seealsolabel
 \\Fcite{with-handle-lock-conflict}."

  (flet (
         (lock-conflict-vector-restart
          (p-heap-objid p-objid lock-conflict)
          (declare (ignore lock-conflict))
          ;; 1996/10/28: Activate the following two lines if the Stable Heap
          ;; daemon terminated ungracefully and sends you an error for almost
          ;; any object loaded in the bootstrap:
	  ;; (sh-unlock-all-all p-heap-objid p-objid)
	  ;; (return-from lock-conflict-vector-restart (values))
          (if (sh-objid-valid-p p-objid)
	      (if (and *verbose* (>= *verbose* 1))
		  (restart-case
		      (error "Locking ~Aobject ~A by ~A failed."
			     (if (p-read-only p-objid p-heap-objid)
				 "read-only "
			       "")
			     (print-persistent-immediate-object
                              p-objid (p-type-tag-of p-objid) nil
                              p-heap-objid)
			     (print-persistent-immediate-object
                              p-heap-objid (p-type-tag-of p-heap-objid) nil
                              p-heap-objid))
		    (continue
			()
			:report "Remove all other locks brute-force from object & retry locking."
		      (sh-unlock-all-all p-heap-objid p-objid)
		      (values))
		    (retry
			()
			:report "Retry to lock the object."
		      (values)))
		(sh-unlock-all-all p-heap-objid p-objid))
	    (when (and *verbose* (>= *verbose* 1))
	      (cerror "Retry to lock the object."
		      "Locking object ~A by ~A failed."
                      (print-persistent-immediate-object
                       p-objid (p-type-tag-of p-objid) nil
                       p-heap-objid)
                      (print-persistent-immediate-object
                       p-heap-objid (p-type-tag-of p-heap-objid) nil
                       p-heap-objid)))))

         (lock-conflict-store-restart
          (p-heap-objid lock-conflict)
          (declare (ignore lock-conflict))
          ;; 1996/10/28: Activate the following two lines if the Stable Heap
          ;; daemon terminated ungracefully and sends you an error for almost
          ;; any object loaded in the bootstrap:
	  ;; (sh-unlock-all-all p-heap-objid +null-objid+)
	  ;; (return-from lock-conflict-store-restart (values))
	  (if (and *verbose* (>= *verbose* 1))
	      (restart-case
                  (error "Locking the whole store by ~A failed."
                         (print-persistent-immediate-object
                          p-heap-objid (p-type-tag-of p-heap-objid) nil
                          p-heap-objid))
		(continue
		    ()
		    :report "Remove the root lock brute-force from store & retry locking."
		  (sh-unlock-all-all p-heap-objid +null-objid+)
		  (values))
		(retry
		    ()
		    :report "Retry to lock the store."
		  (values)))
	    (sh-unlock-all-all p-heap-objid +null-objid+))))

    (incf *ignore-info-messages-semaphore*)
    (unwind-protect
	(loop as old-lock-mode = (list lock-conflict)
              as p-numeric-heap-objid = (persistent-object-objid p-heap-objid)
              as p-objid-valid-p = (sh-objid-valid-p p-objid)
              as p-numeric-objid = (if p-objid-valid-p
                                       (persistent-object-objid p-objid)
                                     p-objid)
	      finally (return (values-list old-lock-mode))
	      do
	      (process-wait-with-timeout
	       (format nil "Waiting for lock on ~A"
                       (if p-objid-valid-p
                           (print-persistent-immediate-object
                            p-objid (p-type-tag-of p-objid) nil
                            p-heap-objid)
                         p-objid))
	       *suspend-timeout*
	       #'(lambda (p-heap-objid p-objid)
		   (setf old-lock-mode
			 (multiple-value-list
			  (funcall lock-expression p-heap-objid p-objid)))
		   (not (<= +lock-conflict-first+
                            (car old-lock-mode)
                            +lock-conflict-last+)))
	       p-heap-objid p-objid)
	      while (<= +lock-conflict-first+
                        (car old-lock-mode)
                        +lock-conflict-last+)
	      do
              (if (<= (car old-lock-mode) +lock-conflict-vector+)
	          (lock-conflict-vector-restart p-numeric-heap-objid
                                                p-numeric-objid
                                                (car old-lock-mode))
		(lock-conflict-store-restart p-numeric-heap-objid
					     (car old-lock-mode))))
      (decf *ignore-info-messages-semaphore*))))

;;; ---------------------------------------------------------------------------
(defmacro with-handle-lock-conflict (lock-expression p-heap-objid p-objid)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{lock-expression}}
      {a function taking one argument}
 \\isanobjid{\\funarg{p-objid}}
\\Valueslabel
 The value returned by evaluating \\funarg{lock-expression}.
\\Purposelabel
 Evaluates \\funarg{lock-expression}\\ and checks if a lock failure
 occurred; if yes, \\fcite{handle-lock-conflict}\\ is called.
\\Seealsolabel
 \\Fcite{handle-lock-conflict}."

  (let ((saved-lock-expression (gensym "SAVED-LOCK-EXPRESSION-"))
        (saved-heap-objid (gensym "SAVED-HEAP-OBJID-"))
        (saved-objid (gensym "SAVED-OBJID-"))
        (old-lock-mode (gensym "OLD-LOCK-MODE-"))
        (return-value (gensym "RETURN-VALUE-")))
    `(let* ((,saved-lock-expression ,lock-expression)
            (,saved-heap-objid ,p-heap-objid)
            (,saved-objid ,p-objid)
            (,old-lock-mode (multiple-value-list
                             (funcall ,saved-lock-expression
                                      ,saved-heap-objid
                                      ,saved-objid)))
	    (,return-value (car ,old-lock-mode)))
       (declare (type ;;; 2005-04-06 hkirschk: Corrected for LispWorks 4.4
		      #-:small-fixnum fixnum
		      #+:small-fixnum integer
		      ,saved-heap-objid ,saved-objid)
                #-:lispworks4 ;; and hopefully not later
		(dynamic-extent ,saved-lock-expression
				,saved-heap-objid ,saved-objid
				,old-lock-mode ,return-value))
       (cond
        ((or (< ,return-value +lock-error-first+)
	     (> ,return-value +lock-error-last+)
	     (>= ,return-value 0))
         (values-list ,old-lock-mode))
        ((<= +lock-conflict-first+ ,return-value +lock-conflict-last+)
	 (handle-lock-conflict ,saved-lock-expression
			       ,saved-heap-objid ,saved-objid
			       ,return-value))
        (t (handle-lock-error ,saved-heap-objid ,saved-objid
			      ,return-value))))))

;;;; Local variables:
;;;; buffer-file-coding-system: raw-text-unix
;;;; End:
