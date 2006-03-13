;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-session.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	20.4.94
;;;; Description	Multiprocessing support for PLOB
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

(in-package :plob)

;;; ---------------------------------------------------------------------------
(defvar *sessions* (make-hash-table :test 'eql)
  #+:lisp-doc "A hash table containing all open sessions.")

;;; ---------------------------------------------------------------------------
(defgeneric open-session (url p-heap)
  #+:lisp-doc (:documentation "
\\Valueslabel
 Returns a session-representing instance of
 \\fcite{persistent-heap}.
\\Purposelabel
 Open a session for \\funarg{p-heap}.
 A session is viewed as a calling
 lisp-internal process by the C level of \\plob\\ and as a
 handle for a `logical' heap by the \\cl\\ level (see
 \\fcite{persistent-heap}\\ for details).
 A session is opened by one of the other \\plob\\ functions when necessary.
\\Seealsolabel
 \\Fcite{persistent-heap};
 \\fcite{*default-persistent-heap*}."))

;;; ---------------------------------------------------------------------------
(defgeneric close-session (p-heap &optional (with-garbage-collection t))
  #+:lisp-doc (:documentation "
\\Purposelabel
 Close the session of \\funarg{p-heap}.
\\Remarkslabel
 The session will be opened again if necessary.
\\Seealsolabel
 \\Fcite{open-session};
 \\fcite{*default-persistent-heap*}."))

;;; ---------------------------------------------------------------------------
(defun local-url-p (&optional (url (effective-url)))
  #+:lisp-doc "
 Check if the passed URL is refererring a local database."
  (equal (url-transport url) "local"))

;;; ---------------------------------------------------------------------------
(defun effective-url (&optional (url *database-url*)
				(default-url *default-database-url*))
  #+:lisp-doc "
 Compute the effective URL to open."
  (let ((result (merge-urls url default-url)))
    (if (local-url-p result)
	(make-url :transport "local" :directory (url-directory result))
	result)))

;;; ---------------------------------------------------------------------------
(defun close-my-session (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 A more comfortable user front end function for \\fcite{close-session}."
  (close-session p-heap))

;;; ---------------------------------------------------------------------------
(defmethod open-session (url p-heap)
  (setf *database-url* url)
  (unless *in-bootstrap-p*
    (assert-sh-open-p))
  (unless p-heap
    (setf p-heap (make-persistent-heap)))
  (let ((p-heap-objid (persistent-object-objid p-heap)))
    (unless p-heap-objid
      (setf p-heap-objid (sh-open))
      (setf (persistent-object-objid p-heap) p-heap-objid))
    (when p-heap-objid
      (let ((p-heap-pid (persistent-heap-pid p-heap)))
        (unless p-heap-pid
          (setf (persistent-heap-pid p-heap) (process-pid))
	  ;; 1996/11/01 HK: Looks as if this crashes
	  ;; the sytem at closedown:
          ;; (ensure-process-cleanup
          ;;  `(close-session ,*default-persistent-heap*))
          ))
      (setf (gethash p-heap-objid *sessions*) p-heap)))
  p-heap)

;;; ---------------------------------------------------------------------------
(defun open-my-session
    (&optional (url *database-url*)
	       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
  A more comfortable user front end function for \\fcite{open-session}."
  (open-session url p-heap))

;;; ---------------------------------------------------------------------------
(defmethod close-session (p-heap &optional (with-garbage-collection t))
  (let ((p-heap-objid (persistent-object-objid p-heap)))
    (when (and (sh-open-p)
               (persistent-heap-p p-heap)
               (sh-objid-valid-p p-heap-objid))
      (end-transaction t p-heap)
      (sh-close p-heap-objid with-garbage-collection))
    (setf (persistent-heap-objid p-heap) nil)
    (setf (persistent-heap-pid p-heap) nil)
    (remhash p-heap-objid *sessions*))
  p-heap)

;;; ---------------------------------------------------------------------------
(defmacro with-session (&rest forms)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{forms}}
      {an implicit \\lisp{progn}\\ expression}
\\Valueslabel
 Returns the value of evaluating \\funarg{forms}.
\\Purposelabel
 Embed the evaluation of \\funarg{forms}\\ into an own session. Each session
 has isolated access to the persistent heap.
\\Seealsolabel
 \\Fcite{open-my-session};
 \\fcite{close-session}."
  (let ((result (gensym "RESULT-")))
    `(let* ((*default-persistent-heap* (make-persistent-heap))
	    (,result (multiple-value-list (progn ,@forms))))
       (close-session *default-persistent-heap*)
       (values-list ,result))))
  
;;; ---------------------------------------------------------------------------
(defmethod close-heap (&optional (with-garbage-collection t))
  (labels ((close-all-client-sessions
            (&optional (with-garbage-collection t))
	    (maphash #'(lambda (p-heap-objid p-heap)
                         (declare (ignore p-heap-objid))
			 (close-session p-heap with-garbage-collection))
		     *sessions*)
	    (clrhash *sessions*)
	    (values)))
                                      
    (let ((root-persistent-heap-objid *root-persistent-heap-objid*))
      (if (sh-open-p)
          (unwind-protect
              (close-all-client-sessions with-garbage-collection)
            (progn
              (invalidate-all-globals)
              (clear-cache)
              (when root-persistent-heap-objid
                (sh-close root-persistent-heap-objid
                          with-garbage-collection))))
        (unwind-protect
	    (close-all-client-sessions with-garbage-collection)
          (progn
	    (invalidate-all-globals)
	    (clear-cache))))))
  (values))

;;; ---------------------------------------------------------------------------
(defun p-sessions (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Return the object representing all active sessions."
  (when (sh-open-p)
    (assert-open-session-p p-heap)
    (let ((p-objid-sessions
           (sh-sessions (persistent-object-objid p-heap))))
      (when p-objid-sessions
        (make-persistent-object p-objid-sessions)))))

;;; ---------------------------------------------------------------------------
(defun show-sessions (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Show all active sessions. In the very left column a single indicator
 character is shown:
 \\begin{description}
 \\item[`*'] for the session which evaluated \\lisp{(show-sessions)}
 \\item[`.'] for sessions within the current LISP process
 \\item[` ' (blank)] for sessions of other LISP processes
 \\end{description}"

  (if (sh-open-p)
      (progn
        (assert-open-session-p p-heap)
        (with-transaction (p-heap)
	  (let ((p-objid-heap (persistent-object-objid p-heap))
		(sessions (p-sessions p-heap)))
	    (format t "  Sessions on ~A:~%" (effective-url))
	    (when sessions
	      (mapbtree #'(lambda (key data)
			    (declare (ignore data))
			    (format t "~A ~A~%"
				    (let ((p-objid-key
					   (persistent-object-objid key)))
				      (cond
				       ((= p-objid-heap p-objid-key) "*")
				       ((gethash p-objid-key *sessions*) ".")
				       (t " ")))
				    key)
			    t)
			sessions)))))
    (format t "Not connected to any server.~%"))
  (values))

;;; ---------------------------------------------------------------------------
(defun p-configuration (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Get the Stable Heap configuration settings."
  (when (sh-open-p)
    (assert-open-session-p p-heap)
    (sh-configuration (persistent-object-objid p-heap))))

;;; ---------------------------------------------------------------------------
(defun p-exit (&optional force (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabool{force}
\\Purposelabel
 Try to exit the \\plob\\ server daemon. When \\funarg{force}\\ is
 \\lispnil, the exit request is rejected when there are other active
 client sessions. Passing \\nonnil\\ for \\funarg{force}\\ will force
 a server exit which in turn will have unpredictable consequences for
 the other clients, since all clients states will be lost.

 After the daemon has exited, it can only be started again on the
 server host by the \\plob\\ administrator, e.g.\\ by using the
 \\lisp{plobdadmin}\\ script with the \\lisp{connect}\\ command on
 the server host."

  (sh-exit (persistent-object-objid p-heap) force))

;;; ---------------------------------------------------------------------------
(defun p-reset (&optional force	(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabool{force}
\\Purposelabel
 Try to reset the \\plob\\ server daemon. When \\funarg{force}\\ is
 \\lispnil, the reset request is rejected when there are other active
 client sessions.
\\Remarkslabel
 The reset will clear the admin user; the next user which logs in
 is made the new admin user."
  (sh-reset (persistent-object-objid p-heap) force))

;;; ---------------------------------------------------------------------------
(defun p-restart (&optional force (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabool{force}
\\Purposelabel
 Try to restart the \\plob\\ server daemon. When \\funarg{force}\\ is
 \\lispnil, the restart request is rejected when there are other active
 client sessions. Passing \\nonnil\\ for \\funarg{force}\\ will force a
 server restart which in turn will have unpredictable consequences for
 the other clients, since the client's state will be lost.
\\Remarkslabel
 Restarting the server daemon is a good way to terminate all active
 sessions; after the restart, all sessions will be closed."
  (sh-restart (persistent-object-objid p-heap) force))

;;; ---------------------------------------------------------------------------
(defun p-suspend (&optional description (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{description}}
       {a string telling the reason why the server was suspended}
 \\isabool{force}
\\Purposelabel
 Suspend the \\plob\\ server daemon. Before being suspended, all cached
 data is flushed to disk; during being suspended, a backup can be done on
 the database files used by the server. Pleas note that during being
 suspended, all client calls to the server except a resume call will
 block the client's thread.
\\Seealsolabel
 \\Fcite{p-resume}."
  (sh-suspend (persistent-object-objid p-heap) description))

;;; ---------------------------------------------------------------------------
(defun p-resume ()
  #+:lisp-doc "
\\Purposelabel
 Resume the \\plob\\ server daemon.
\\Seealsolabel
 \\Fcite{p-suspend}."
  (sh-resume))

;;; ---------------------------------------------------------------------------
(defun p-stabilise (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Stabilise (i.e. flush) the Stable Heap."
  (when (sh-open-p)
    (assert-open-session-p p-heap)
    (sh-stabilise (persistent-object-objid p-heap))))

;;; ---------------------------------------------------------------------------
(defun p-statistics (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Get the Stable Heap statistics."
  (when (sh-open-p)
    (assert-open-session-p p-heap)
    (sh-statistics (persistent-object-objid p-heap))))

;;; ---------------------------------------------------------------------------
(defconstant +version-layers+
    '(:database :server :client :api)
  #+:lisp-doc "The symbolic names of all layers of which a version number
 can be retrieved.")

;;; ---------------------------------------------------------------------------
(defun p-get-version (&optional (layer :api)
				(p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{layer}}
       {a keyword symbol naming the version to retrieve}
\\Purposelabel
  Get a \\plob\\ version number; which kind of version number to retrieve
  is decided by \\funarg{layer}:
  \\begin{description}
  \\item[\\lisp{:database}] Get the version number the database was
    formatted with.
  \\item[\\lisp{:server}] Get the version number of the server's C code.
  \\item[\\lisp{:client}] Get the version number of the client's C code.
  \\item[\\lisp{:api}] Get the version number of the LISP API code.
  \\end{description}"
  (let* ((p-heap-objid (if (sh-open-p)
			   (persistent-object-objid p-heap)
			 +null-objid+))
	 (version-number
	  (ecase layer
	    (:database
	     (c-sh-get-version p-heap-objid +get-database-version+))
	    (:server
	     (c-sh-get-version p-heap-objid +get-server-code-version+))
	    (:client
	     (c-sh-get-version p-heap-objid +get-client-c-code-version+))
	    (:api +plob-version+))))
    (when (> version-number 0)
      version-number)))

;;; ---------------------------------------------------------------------------
(defun check-version-numbers (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Check if there is a version number mismatch between
 \\plobwoexcl's layers.
\\Valueslabel
  Returns the number of version number mismatches between \\plobwoexcl's
  layers."
  (let ((mismatches 0)
	(first-version-number nil) (first-version-layer nil))
    (dolist (layer +version-layers+)
      (let ((version-number (p-get-version layer p-heap)))
	(unless first-version-number
	  (setf first-version-number version-number)
	  (setf first-version-layer layer))
	(when (and version-number
		   (not (eql first-version-number version-number)))
	  (incf mismatches)
	  (when (and *verbose* (>= *verbose* 1))
	    (warn "PLOB version number mismatch between ~
                   ~A layer with version ~D.~2,'0D and ~
                   ~A layer with version ~D.~2,'0D."
		  first-version-layer
		  (floor first-version-number 100)
		  (mod first-version-number 100)
		  layer
		  (floor version-number 100)
		  (mod version-number 100))))))
    (when (> mismatches 0)
      mismatches)))

;;; ---------------------------------------------------------------------------
(defun p-admin (command &optional (url *database-url*) user-name)
  #+:lisp-doc "
\\Argumentslabel
  \\isa{\\funarg{command}}
       {a symbol or a string naming a subcommand of script
        \\lisp{plobdadmin}}
\\Purposelabel
 Send \\funarg{command}\\ to script plobdadmin.
 In order for this command to work, the patched script
 \\lisp{plobdadmin}\\ found in the database root directory must be
 locatable in the current user's \\lisp{\\$\\{HOME\\}/bin}\\ directory,
 for example by establishing a symbolic link.
 Furthermore, the \\lisp{rsh}\\ command must execute properly;
 here, that means, without reading a password. Please
 consult the \\lisp{man rsh}\\ pages on how to achieve this and how
 security constraints are affected by this."
  (when (symbolp command)
    (setf command (string-downcase (symbol-name command))))
  (unless user-name
    (setf user-name "${USER}"))
  (let* ((server-host (url-host (effective-url url)))
	 (command-string
         (format
	  nil
	  "rsh ~A -l ~A /bin/sh ${HOME}/bin/plobdadmin -d ${DISPLAY} -~A"
	  server-host user-name command)))
    #+:allegro
    (excl:shell command-string)
    #+:lispworks
    (system::call-system-showing-output command-string
                                        :prefix nil
                                        :show-cmd nil)))

;;; ---------------------------------------------------------------------------
;;; 1996/10/23 HK: suspend-callback is obsolete for the RPC version:
#+:now-obsolete
(defun sh-suspend-callback (objid-lock-by objid-to-lock reason)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{objid-lock-by}\\ resp.\\ \\funarg{objid-to-lock}}
 \\isa{\\funarg{reason}}
      {a string}
\\Purposelabel
 The \\cl\\ callback called when the current \\cl\\ session should be
 suspended. This function is called from the C level of \\plob\\ when an access
 conflict arises.

 This function waits for a maximum of {\\bf *suspend-timeput*} seconds;
 if the session did not receive a {\\bf sh-wakeup-callback} in this time,
 the lock request is cancelled and an error is signalled.
\\Remarkslabel
 If the current \\cl\\ session cannot be suspended (e.g.\\ for
 \\cl\\ systems with a missing multi-processing interface),
 return immediately from \\fcite{sh-suspend-callback};
 the \\plob\\ C level detects that the \\cl\\ session did not wait
 for the conflicting lock and signals an error.

\\Exampleslabel
 Session~A locked a persistent object for write access and
 session~B wants to lock the same persistent object
 for read access; since read and write locks conflict for different
 sessions, session~B is suspended until session~A releases the
 write lock.
\\Seealsolabel
 Section \\fcite{locking ...};
 \\fcite{sh-wakeup-callback};
 \\fcite{sh-set-lock};
 \\fcite{*suspend-timeout*}."

  (declare (ignore objid-to-lock))
  (let ((p-heap (is-registered-objid objid-lock-by))
	(result +c-false+))
    (if p-heap
        (let ((timed-out t))
	  (when (and *verbose* (>= *verbose* 5))
	    (format t ";; Suspend: ~A~%" reason))
	  (setf (persistent-heap-suspended-p p-heap) t)
	  (process-wait-with-timeout
	   reason
	   *suspend-timeout*
	   #'(lambda (p-heap)
	       (when (not (persistent-heap-suspended-p p-heap))
	         (setf timed-out nil)
	         t))
	   p-heap)
	  (if timed-out
	      (warn "Suspend timed out: ~A" reason)
            (setf result +c-true+)))
      (warn "Suspend request for unknown object ~A."
	    (make-persistent-object objid-lock-by)))
    result))

;;; ---------------------------------------------------------------------------
;;; 1996/10/23 HK: wakeup-callback is obsolete for the RPC version:
#+:now-obsolete
(defun sh-wakeup-callback (objid-lock-by objid-to-lock reason)
  #+:lisp-doc "
\\Argumentslabel

 \\isanobjid{\\funarg{objid-lock-by}\\ resp.\\ \\funarg{objid-to-lock}}
 \\isa{\\funarg{reason}}
      {a string}

\\Purposelabel

 The \\cl\\ callback called when the current \\cl\\ session should be
 waked up after a suspend.

 This function is called from the C level of \\plob\\ after a call
 to \\fcite{sh-suspend-callback}\\ when an access
 conflict for the current \\cl\\ session no longer exists.

\\Seealsolabel

 Section \\fcite{locking ...};
 \\fcite{sh-suspend-callback};
 \\fcite{sh-set-lock};
 \\fcite{*suspend-timeout*}."

  (declare (ignore objid-to-lock))
  (let ((p-heap (is-registered-objid objid-lock-by))
        (result +c-false+))
    (if p-heap
        (progn
          (when (and *verbose* (>= *verbose* 5))
            (format t ";; Wakeup: ~A~%" reason))
          (setf (persistent-heap-suspended-p p-heap) nil)
          (setf result +c-true+))
      (warn "Wakeup request for unknown object ~A."
	    (make-persistent-object objid-lock-by)))
  result))

;;;; Local variables:
;;;; buffer-file-coding-system: raw-text-unix
;;;; End:
