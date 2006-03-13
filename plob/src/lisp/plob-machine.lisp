;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-machine.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	1997/04/04
;;;; Description	Machine login administration
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
(defun p-lookup-machine (mach-name)
  #+:lisp-doc
  "Look up \\funarg{mach-name}\\ and return a numeric Internet address for it."
  (sh-get-host-addr mach-name))

;;; ---------------------------------------------------------------------------
(defun p-create-machine (mach-name
			 &optional loginp
			 (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Return an object representing the machine named \\funarg{mach-name}."

  (when (sh-open-p)
    (assert-open-session-p p-heap)
    (let ((p-objid-machine
           (sh-create-machine (persistent-object-objid p-heap)
			      (p-lookup-machine mach-name)
			      (ecase loginp
				((nil) +login-ignore+)
				(:allow +login-allow+)
				(:deny +login-deny+)))))
      (when p-objid-machine
        (make-persistent-object p-objid-machine)))))

;;; ---------------------------------------------------------------------------
(defun (setf p-machine-loginp) (new-value machine
				&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Set the login flag for a machine.
\\Exampleslabel
 To deny a login from clients running on any machine and
 allow a login only from clients running on your localhost,
 evaluate:
\\begin{lispcode}
(setf (p-machine-login \"*\") :deny)
(setf (p-machine-login \"localhost\") :allow)
\\end{lispcode}
 To allow all clients whose Internet address begins with
 \\lisp{134.100.12} \\ a login to the stable heap, evaluate:
\\begin{lispcode}
(setf (p-machine-login \"134.100.12.*\") :deny)
\\end{lispcode}
\\Remarkslabel
 The checking is done on basis of the Internet address which was used
 by the client for addressing the server, i.e.\\ the value of the
 \\fcite{*database-url*}.
 This function may only be called by the \\plob\\ administrator.
\\Seealsolabel
 \\Fcite{show-machines}"

  (let ((machine-objid (typecase machine
			 (string (let ((mach (p-search-machine machine)))
				   (unless mach
				     (setf mach
				       (p-insert-machine machine)))
				   (persistent-object-objid mach)))
		       (t (persistent-object-objid machine)))))
    (when machine-objid
      (sh-machine-loginp (persistent-object-objid p-heap)
			 machine-objid
			 (cond
			  ((eq new-value nil) +login-ignore+)
			  ((eq new-value :allow) +login-allow+)
			  ((eq new-value :deny) +login-deny+)
			  (t new-value)))))
  new-value)

;;; ---------------------------------------------------------------------------
(defun map-numeric-loginp (numeric-loginp)
  #+:lisp-doc "
\\Purposelabel
 Map the \\funarg{numeric-loginp}\\ to a keyword symbol."
    (cond
     ((= numeric-loginp +login-allow+) :allow)
     ((= numeric-loginp +login-deny+) :deny)
     (t nil)))
  
;;; ---------------------------------------------------------------------------
(defun p-machine-loginp (machine
			 &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Get the login allow/deny-flag for a machine."
  (let ((machine-objid (typecase machine
			 (string (let ((mach (p-search-machine machine)))
				   (when mach
				     (persistent-object-objid mach))))
		       (t (persistent-object-objid machine)))))
    (when machine-objid
      (map-numeric-loginp
       (sh-machine-loginp (persistent-object-objid p-heap)
			  machine-objid
			  +login-get-flag+)))))

;;; ---------------------------------------------------------------------------
(defun p-machine-addr (machine
		       &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Get the Internet address of \\funarg{machine}"
  (typecase machine
    (string (p-lookup-machine machine))
    (t (sh-machine-addr (persistent-object-objid p-heap)
			(persistent-object-objid machine)))))

;;; ---------------------------------------------------------------------------
(defun p-search-machine (machine
			 &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Search \\funarg{machine}\\ in the internal list of known machines.
\\Remarkslabel
 This function may only be called by the \\plob\\ administrator."
  (let* ((machine-addr (typecase machine
			 (string (p-lookup-machine machine))
			 (t (p-machine-addr machine))))
	 (found (sh-search-machine (persistent-object-objid p-heap)
				   machine-addr)))
    (when found
      (make-persistent-object found))))

;;; ---------------------------------------------------------------------------
(defun p-delete-machine (machine
			 &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Delete \\funarg{machine}\\ from the internal list of known machines.
\\Remarkslabel
 This function may only be called by the \\plob\\ administrator."

  (let* ((machine-addr (typecase machine
			 (string (p-lookup-machine machine))
			 (t (p-machine-addr machine))))
	 (deleted (sh-delete-machine (persistent-object-objid p-heap)
				     machine-addr)))
    (when deleted
      (make-persistent-object deleted))))

;;; ---------------------------------------------------------------------------
(defun p-insert-machine (machine
			 &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Insert \\funarg{machine}\\ into the internal list of known machines.
\\Remarkslabel
 This function may only be called by the \\plob\\ administrator."

  (let* ((machine-objid (typecase machine
			  (string
			   (persistent-object-objid
			    (p-create-machine machine)))
			  (t
			   (persistent-object-objid machine))))
	 (inserted
	  (sh-insert-machine (persistent-object-objid p-heap)
			     machine-objid)))
    (when inserted
      (make-persistent-object inserted))))

;;; ---------------------------------------------------------------------------
(defun p-machines (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Return the object representing all machines seen so far.
\\Remarkslabel
 This function may only be called by the \\plob\\ administrator."

  (when (sh-open-p)
    (assert-open-session-p p-heap)
    (let ((p-objid-machines
           (sh-machines (persistent-object-objid p-heap))))
      (when p-objid-machines
        (make-persistent-object p-objid-machines)))))

;;; ---------------------------------------------------------------------------
(defun show-machines (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Show all machines which may login to the stable heap.
\\Remarkslabel
 This function may only be called by the \\plob\\ administrator."

  (if (sh-open-p)
      (progn
        (assert-open-session-p p-heap)
        (with-transaction (p-heap)
	  (let ((machines (p-machines p-heap)))
	    (if machines
		(progn
		  (format t "Machines known to ~A:~%" (effective-url))
		  (mapbtree #'(lambda (key data)
				(format t "  ~S ~A~%" key data)
				t)
			    machines))
	      (format t "You have no permissions to access~%~
			the list of allowed machines~%")))))
    (format t "Not connected to any server.~%"))
  (values))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
