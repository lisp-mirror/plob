;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-url.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	1998/02/06
;;;; Description	URLs for naming databases
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
;;; Classes
;;; ---------------------------------------------------------------------------
(defstruct (url
	    (:print-function
	     (lambda (object stream depth)
	       (declare (ignore depth))
	       (write-url object stream))))
  #+:lisp-doc "
\\Purposelabel
 A class representing URLs naming databases. An URL has the form
\\begin{IndentedCompactCode}
  [<transport>:][//<host>][/directory]
\\end{IndentedCompactCode}

\\Seealsolabel
  \\Fcite{make-url-from-string}, \\fcite{merge-urls},
  \\fcite{split-url}, \\fcite{write-url}."

  (transport nil
	     #+:lisp-doc :documentation #+:lisp-doc "
 The transport interface to use for accessing the database. Most
 \\unix\\ system implement only \\lisp{tcp}\\ as the one and only
 transport interface. Do not try to use \\lisp{udp}.")

  (host nil
	#+:lisp-doc :documentation #+:lisp-doc "
 The host where the database resides.")

  (directory nil
	     #+:lisp-doc :documentation #+:lisp-doc "
 The directory on host which contains the database."))

;;; ---------------------------------------------------------------------------
;;; Interface: Generic functions
;;; ---------------------------------------------------------------------------

;;; ---------------------------------------------------------------------------
;;; Implementation: Methods
;;; ---------------------------------------------------------------------------
(defun write-url (object &optional stream)
  #+:lisp-doc "
\\Purposelabel
  Write an instance of \\fcite{url}\\ to \\funarg{stream}.
  If \\funarg{stream}\\ is \\lispnil, a string with the print
  representation is returned."

  (let* ((transport (url-transport object))
	 (host (url-host object))
	 (directory (url-directory object))
	 (object-string
	  (concatenate 'string
		       (if transport
			   (format nil "~A:" transport)
			 "")
		       (if host
			   (format nil "//~A" host)
			 "")
		       (if directory
			   (format nil "~A~A"
				   (if host "/" "")
				   directory)
			 ""))))
    (if stream
	(write-string object-string stream)
      object-string)))

;;; ---------------------------------------------------------------------------
(defun split-url (url)
  #+:lisp-doc "
\\Purposelabel
  Split an URL into its components transport, host and directory.
\\Valueslabel
  Three values are returned:
  \\begin{enumerate}
  \\item The transport protocol specified in \\funarg{url}.
  \\item The host name specified in \\funarg{url}.
  \\item The directory name specified in \\funarg{url}.
  \\end{enumerate}
  If any of these fields is missing in \\funarg{url}, \\lispnil\\ is
  returned.
\\Seealsolabel
  \\Fcite{url}."

  (flet ((split-url-from-string
          (source)
          (let ((transport nil) (host nil) (directory nil)
                (from 0) (i 0) (state :host-or-directory-1)
                (s source))
            ;; Get transport part by searching for the first #\:
            (let ((colon (position #\: source)))
              (when colon
                (if (> colon 0)
                    (setf transport (subseq source 0 colon))
                  (cerror "Return with NIL for transport name"
                          "Missing transport name in URL `~a'"
                          s))
                (setf source (subseq source (1+ colon)))))
            (map nil
                 #'(lambda (c)
                     ;;(format t "Working on ~a, state ~a, from ~a, i ~a~%"
                     ;;        c state from i)
                     (ecase state
                       (:host-or-directory-1
                        (setf from i)
                        (case c
                          (#\/ (setf state :host-or-directory-2))
                          (t (setf state :directory))))
                       (:host-or-directory-2
                        (case c
                          (#\/ (setf from (1+ i))
                               (setf state :host))
                          (t (setf state :directory))))
                       (:host
                        (case c
                          (#\/ (if (> i from)
                                   (setf host (subseq source from i))
                                 (cerror "Return with NIL for host name"
                                         "Missing host name in URL `~a'"
                                         s))
                               (setf from (1+ i))
                               (setf state :directory))))
                       (:directory
                        nil))
                     (incf i))
                 source)
            (ecase state
              (:host
               ;; Retrieve pending host string:
               (if (> i from)
                   (setf host (subseq source from))
                 (cerror "Return with NIL for host name"
                         "Missing host name in URL `~a'"
                         s)))
              ((:host-or-directory-1 :host-or-directory-2 :directory)
               ;; Retrieve pending directory string:
               (when (> i from)
                 (setf directory (subseq source from)))))
            (values transport host directory))))
    (etypecase url
      (url
       (values (url-transport url) (url-host url) (url-directory url)))
      (string
       (split-url-from-string url)))))

;;; ---------------------------------------------------------------------------
(defun merge-urls (first-url &rest more-urls)
  #+:lisp-doc "
\\Purposelabel
 Merge URLs.
\\Seealsolabel
 \\Fcite{url}, \\fcite{merge-pathnames}."

  (let ((url-transport nil) (url-host nil) (url-directory nil))
    (loop for url in (nconc (list first-url) more-urls)
      until (and url-transport url-host url-directory)
      finally (return (make-url :transport url-transport
				:host url-host
				:directory url-directory))
      do
	(multiple-value-bind (transport host directory)
	    (split-url url)
	  (when (and (null url-transport) transport)
	    (setf url-transport transport))
	  (when (and (null url-host) host)
	    (setf url-host host))
	  (when (and (null url-directory) directory)
	    (setf url-directory directory))))))
    
;;; ---------------------------------------------------------------------------
(defun make-url-from-string (url-string)
  #+:lisp-doc "
\\Purposelabel
  Make an instance of \\fcite{url}\\ from \\funarg{url-string}.
\\Seealsolabel
  \\Fcite{split-url}."

  (multiple-value-bind (url-transport url-host url-directory)
      (split-url url-string)
    (make-url :transport url-transport
	      :host url-host
	      :directory url-directory)))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
