;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-misc.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	10.2.94
;;;; Description	PLOB miscellaneous functions
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
(defun find-direct-slot (slot-name the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{slot-name}}
      {a symbol naming a direct slot of \\funarg{the-class}}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Returns the direct slot definition metaobject of the slot
 named \\funarg{slot-name}\\ in \\funarg{the-class}.
\\Seealsolabel
 \\Fcite{find-effective-slot}."

  (find slot-name
        (class-direct-slots the-class)
        :key #'slot-definition-name
        :test #'eq))

;;; ---------------------------------------------------------------------------
(defun find-effective-slot (slot-name the-class)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{slot-name}}
      {a symbol naming an effective slot of \\funarg{the-class}}
 \\isacls{\\funarg{the-class}}
\\Purposelabel
 Returns the effective slot definition metaobject of the slot
 named \\funarg{slot-name}\\ in \\funarg{the-class}.
\\Remarkslabel
 The functions {\\bf find-effective-slot},
 {\\bf find-effective-slot-description} and
 {\\bf find-slot-description} do almost the same; conclusion into
 a single function would be better.
\\Seealsolabel
 \\Fcite{find-direct-slot};
 \\fcite{find-effective-slot-description};
 \\fcite{find-effective-slot-description-by-objid};
 \\fcite{find-slot-description}."

  (find slot-name
        (class-slots the-class)
        :key #'slot-definition-name
        :test #'eq))

;;; ---------------------------------------------------------------------------
;;; Get/set dependent flag directly
;;; ---------------------------------------------------------------------------
(defun p-dependent (p-objid &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Get the dependent flag of \\funarg{p-objid}."
  (assert-open-session-p p-heap)
  (let ((dependent-mode (c-sh-dependent (persistent-object-objid p-heap)
                                        (persistent-object-objid p-objid)
                                        +flag-dependent-get+)))
    (cond
     ((= dependent-mode +flag-dependent-none+) nil)
     ((= dependent-mode +flag-dependent-read+) :read)
     ((= dependent-mode +flag-dependent-write+) :write)
     ((= dependent-mode +flag-dependent-read-write+) :read-write)
     (t dependent-mode))))

;;; ---------------------------------------------------------------------------
(defun (setf p-dependent)
     (symbolic-dependent-mode p-objid
                              &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Set the dependent flag of \\funarg{p-objid}."
  (assert-open-session-p p-heap)
  (let ((dependent-mode 
         (cond
          ((eq symbolic-dependent-mode nil) +flag-dependent-none+)
          ((eq symbolic-dependent-mode :read) +flag-dependent-read+)
          ((eq symbolic-dependent-mode :write) +flag-dependent-write+)
          ((eq symbolic-dependent-mode :read-write)
           +flag-dependent-read-write+)
          (t symbolic-dependent-mode))))
    (c-sh-dependent (persistent-object-objid p-heap)
		    (persistent-object-objid p-objid)
		    dependent-mode))
  symbolic-dependent-mode)

;;; ---------------------------------------------------------------------------
;;; Flushing the stable heap
;;; ---------------------------------------------------------------------------
(defun flush-if-idle ()
  #+:lisp-doc "
\\Purposelabel
 A hook called when \\lw\\ goes idle:
 The \\sh\\ is flushed to disk.
\\Seealsolabel
 \\Fcite{flush-if-idle-mode}."

  (let ((p-heap-objid (persistent-object-objid *default-persistent-heap*)))
    (when (sh-objid-valid-p p-heap-objid)
      (catch-errors (c-sh-stabilise p-heap-objid))
      (catch-errors (c-sh-flush-transaction p-heap-objid)))))

;;; ---------------------------------------------------------------------------
(defun flush-if-idle-mode (&optional (set-it nil set-it-p))
  #+:lisp-doc "
\\Argumentslabel
 \\isabool{\\funarg{set-it}}
\\Purposelabel
 For argument \\funarg{set-it}\\ passed with a \\nonnil\\ value,
 \\fcite{flush-if-idle}\\ is put on the idle sleep hook.
 For argument \\funarg{set-it}\\ passed with \\lispnil,
 \\fcite{flush-if-idle}\\ is removed from the idle sleep hook.
 For argument \\funarg{set-it}\\ not passed,
 it is checked if \\fcite{flush-if-idle}\\ is on the idle sleep hook.
\\Seealsolabel
 \\Fcite{flush-mode};
 \\fcite{is-on-idle-sleep-hook};
 \\fcite{push-to-idle-sleep-hook};
 \\fcite{remove-from-idle-sleep-hook};
 \\fcite{flush-if-idle}."

  (if set-it-p
      (progn
        (if set-it
            (push-to-idle-sleep-hook 'flush-if-idle)
	  (remove-from-idle-sleep-hook 'flush-if-idle))
        set-it)
    (is-on-idle-sleep-hook 'flush-if-idle)))

;;; ---------------------------------------------------------------------------
(defconstant +flush-mode->keyword+
  `((,+flush-never+	. :never)
    (,+flush-seldom+	. :seldom)
    (,+flush-sometimes+	. :sometimes)
    (,+flush-often+	. :often)
    (,+flush-always+	. :always))
  #+:lisp-doc "
\\Purposelabel
 Internal constant.
 Mapping between keyword symbols \\lisp{:never}, \\lisp{:seldom},
 \\lisp{:sometimes}, \\lisp{:often}\\ and \\lisp{:always}\\ and their
 \\plob\\ C level numeric flush-mode companions {\\bf +flush-never+},
 {\\bf +flush-seldom+}, {\\bf +flush-sometimes+}, {\\bf +flush-often+} and
 {\\bf +flush-always+}.
\\Seealsolabel
 \\Fcite{flush-mode}.")

;;; ---------------------------------------------------------------------------
(defun flush-mode (&optional (mode nil modep)
		   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 \\Argumentslabel
  \\isa{\\keyarg{mode}}
       {one of the keyword symbols
        \\lisp{:never}, \\lisp{:seldom},
        \\lisp{:sometimes}, \\lisp{:often}\\ or \\lisp{:always}}
  \\isabool{\\keyarg{idle}}
 \\Valueslabel
  Returns two values:
  \\begin{enumerate}

  \\item The current flush mode.

  \\item A second value of \\nonnil\\ indicates that idle flushing
   is switched on, otherwise idle flushing is switched off.

  \\end{enumerate}
 \\Purposelabel
  This function serves two purposes:
  \\begin{enumerate}

  \\item Set resp.\\ request the implicit flushing done in
   the \\plob\\ C level (argument \\keyarg{mode}).
   If \\keyarg{mode}\\ is passed, the flush mode is set to
   \\keyarg{mode}. A mode of \\lisp{:never}\\ causes the
   \\plob\\ C level never to flush implicitly whereas a mode of
   \\lisp{:always}\\ will cause the C level to flush always
   after any changes done to a persistent object.
   If \\keyarg{mode}\\ is not passed, the current flush mode keeps
   unchanged.

  \\item Set resp.\\ request if the \\sh\\ is flushed when the
   system goes idle (argument \\keyarg{idle}).
   If \\keyarg{idle}\\ is passed and \\nonnil, each time
   \\lw\\ goes idle a flush of the \\sh\\ is performed;
   the flushing is triggered from the \\cl\\ code, so the
   \\plob\\ C level flush mode passed in \\keyarg{mode}\\ should
   be set to \\lisp{:never}\\ when \\keyarg{idle} is \\nonnil.
   If \\keyarg{idle}\\ is passed and \\lispnil, flushing
   is never triggered from the \\cl\\ code.
   If \\keyarg{idle}\\ is not passed, the idle flushing keeps
   unchanged.

  \\end{enumerate}
 \\Remarkslabel
  The mode \\lisp{:always}\\ will slow down the \\cl\\ system awfully,
  i.e.\\ it will no longer be usable. If at least some automatic
  flushing is wanted, try it with
  \\keyarg{mode}\\ \\lisp{:never}\\ and \\keyarg{idle}\\ \\lispt.

  For maximum speed, no implicit and no idle flushing should be done;
  in this case, the user must call the \\fcite{plob-flush}\\ from time
  to time. When implicit and idle flushing is switched off, the
  \\fcite{close-heap}\\ or the \\fcite{plob-flush}\\ {\\sl must}
  be called by the user before exiting \\lw; otherwise the
  \\sh\\ might be in an inconsistent state next time it is opened.

  In the current \\plob\\ RPC-based version, flushing initiated by the
  client doesn't make much sense, so no client-initiated flushing will
  be done.
 \\Exampleslabel
  Do no implicit flushing in the \\plob\\ C level and also never when
  the system goes idle (this is the recommened default case).
  In this case, before exiting \\lw\\ a call to \\fcite{close-heap}\\ or
  to \\fcite{plob-flush}\\ must be done:
  \\begin{lispcode}
 (flush-mode :mode :never :idle nil)
  \\end{lispcode}

  Do no implicit flushing in the \\plob\\ C level but when the system
  goes idle (the other reasonable setting).
  In this case, before exiting \\lw\\ a call to \\fcite{close-heap}\\ or
  to \\fcite{plob-flush}\\ should be done:
  \\begin{lispcode}
 (flush-mode :mode :never :idle t)
  \\end{lispcode}
 \\Seealsolabel
  \\Fcite{flush-if-idle-mode};
  \\fcite{+flush-mode->keyword+}."

  (let ((p-heap-objid (persistent-heap-objid p-heap)))
    (if modep
	(let ((numeric-flush-mode (car (rassoc mode +flush-mode->keyword+))))
	  (unless numeric-flush-mode
	    (setf numeric-flush-mode mode))
	  (catch-errors (c-sh-flush-mode p-heap-objid
					 numeric-flush-mode))
	  mode)
      (cdr (assoc (catch-errors (c-sh-flush-mode p-heap-objid
						 +flush-get-mode+))
		  +flush-mode->keyword+)))))
      
;;; ---------------------------------------------------------------------------
;;; Handling for sub-objects (objects contained in an enclosing object)
;;; ---------------------------------------------------------------------------

(defun with-direct-representation-p (depth)
  #+:lisp-doc "
 Check if \\funarg{depth}\\ has a value indicating working directly on
 a persistent object."

  (or (eq depth :objid) (eq depth :object)))

;;; ---------------------------------------------------------------------------
(defun t-slot-to-p-objid-in-transaction
    (t-slot-value depth p-heap p-objid at-location t-set-function
     &optional (expecting-type-tag +null-type-tag+)
	       (expecting-class +null-objid+)
	       (p-set-function #'(setf p-index)))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{t-slot-value}}
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}}
\\Valueslabel
 Returns three values:
 \\begin{enumerate}

 \\item The value of \\funarg{t-slot-value}\\ is returned as
  the first value.

 \\item The \\objid\\ of storing
  \\funarg{t-slot-value}\\ to the \\sh\\ is returned as
  the second value.

 \\item The \\typetag\\ of \\funarg{t-slot-value}\\ is returned as
  the third value.

 \\end{enumerate}
\\Purposelabel
 Write to a persistent object's component; the
 slot value of the persistent object referenced by
 \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
 set to the persistent object created by storing
 \\funarg{t-slot-value}\\ to the \\sh.
\\Seealsolabel
 \\Fcite{(setf p-index)}."

  (multiple-value-bind (p-slot-objid p-slot-type-tag)
      (t-object-to-p-objid t-slot-value depth p-heap)
    (when (and t-set-function (not (with-direct-representation-p depth)))
      ;; Write t-slot-value destructively into transient object
      ;; registered under p-objid:
      (multiple-value-bind (t-object t-object-p)
	  (is-registered-objid p-objid)
	(when t-object-p
	  (funcall t-set-function t-slot-value t-object))))
    (funcall p-set-function
             p-slot-objid 
             p-heap p-objid at-location
	     expecting-type-tag expecting-class p-slot-type-tag)
    (values t-slot-value p-slot-objid p-slot-type-tag)))

;;; ---------------------------------------------------------------------------
(defun t-slot-to-p-objid
     (t-slot-value depth p-heap p-objid at-location t-set-function
		     &optional (expecting-type-tag +null-type-tag+)
		     (expecting-class +null-objid+)
                     (p-set-function #'(setf p-index)))
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{t-slot-to-p-objid-in-transaction}.
\\Valueslabel
 See \\fcite{t-slot-to-p-objid-in-transaction}.
\\Purposelabel
 Calls the \\fcite{t-slot-to-p-objid-in-transaction}\\ ensuring
 that the call will be embedded into a transaction on
 \\funarg{p-heap}.
\\Seealsolabel
 \\Fcite{t-slot-to-p-objid-in-transaction};
 \\fcite{p-objid-to-t-slot}."

  (with-transaction (p-heap)
    (t-slot-to-p-objid-in-transaction
     t-slot-value depth p-heap p-objid at-location t-set-function
     expecting-type-tag expecting-class p-set-function)))

;;; ---------------------------------------------------------------------------
(defun p-objid-to-t-slot-in-transaction
    (p-objid at-location depth p-heap t-set-function
     &optional (expecting-type-tag +null-type-tag+)
	       (expecting-class +null-objid+)
	       (p-get-function #'p-index))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{p-objid}}
 \\isa{\\funarg{at-location}}
      {a fixnum}
 \\isatypetag{\\funarg{expecting-type-tag}}
\\Valueslabel
 Returns three values:
 \\begin{enumerate}

 \\item The transient representation of the persistent object
  located at position \\funarg{at-location}\\ in \\funarg{p-objid}.

 \\item The \\objid\\ of the persistent object located at position
  \\funarg{at-location}\\ in \\funarg{p-objid}.

 \\item The \\typetag\\ of the persistent object located at position
  \\funarg{at-location}\\ in \\funarg{p-objid}.

 \\end{enumerate}

\\Purposelabel
 Read a persistent object's component; the
 slot value of the persistent object referenced by
 \\funarg{p-objid}\\ at position \\funarg{at-location}\\ is
 returned in its transient representation.
\\Seealsolabel
 \\Fcite{p-index}."

  (multiple-value-bind (p-slot-objid p-slot-type-tag)
      (funcall p-get-function p-heap p-objid at-location expecting-type-tag)
    (if (and t-set-function (not (with-direct-representation-p depth)))
      ;; Write t-slot-value destructively into transient object
      ;; registered under p-objid:
      (multiple-value-bind (t-object t-object-p)
	  (is-registered-objid p-objid)
	(multiple-value-bind
	    (t-slot-value p-slot-objid-2 p-slot-type-tag-2)
	    (p-objid-to-t-object p-slot-objid p-slot-type-tag depth p-heap)
	  (when t-object-p
	    (funcall t-set-function t-slot-value t-object))
	  (values t-slot-value p-slot-objid-2 p-slot-type-tag-2)))
      (values
       (p-objid-to-t-object p-slot-objid p-slot-type-tag depth p-heap)
       p-slot-objid p-slot-type-tag))))

;;; ---------------------------------------------------------------------------
(defun p-objid-to-t-slot (p-objid at-location depth p-heap t-set-function
				  &optional
				  (expecting-type-tag +null-type-tag+)
				  (expecting-class +null-objid+)
				  (p-get-function #'p-index))
  #+:lisp-doc "
\\Argumentslabel
 See \\fcite{p-objid-to-t-slot-in-transaction}.
\\Valueslabel
 See \\fcite{p-objid-to-t-slot-in-transaction}.
\\Purposelabel
 Calls the \\fcite{p-objid-to-t-slot-in-transaction}\\ ensuring
 that the call will be embedded into a transaction on
 \\funarg{p-heap}.
\\Seealsolabel
 \\Fcite{p-objid-to-t-slot-in-transaction};
 \\fcite{t-slot-to-p-objid}."

  (with-transaction (p-heap)
    (p-objid-to-t-slot-in-transaction p-objid at-location depth p-heap
				      t-set-function
				      expecting-type-tag expecting-class
				      p-get-function)))

;;; ---------------------------------------------------------------------------
;;; Persistent apropos
;;; ---------------------------------------------------------------------------

(defun p-apropos-btree (btree &optional search-string)
  #+:lisp-doc "
\\Argumentslabel
 \\isabtree{\\funarg{btree}}
 \\isa{\\funarg{search-string}}
      {a string}
\\Purposelabel
 Print the contents of \\funarg{btree}\\ in a human-readable form.
 If \\funarg{search-string}\\ is passed, only BTree entries with
 \\funarg{search-string}\\ in their key are printed.
\\Seealsolabel
 \\Fcite{make-btree};
 \\fcite{p-apropos}."

  (let ((downcase-search-string nil)
	(p-heap *default-persistent-heap*))
    (flet ((show-apropos
            (k data)
	    (let* ((class-of-key (class-of k))
                   (key (if (eq class-of-key
			        +persistent-object-class+)
                            (load-object k :cached p-heap)
                          k))
                   (downcase-key (cond
				  ((stringp key)
				   (string-downcase key))
				  ((symbolp key)
				   (string-downcase (symbol-name key)))))
                   (key-objid (if (eq class-of-key
				      +persistent-object-class+)
                                   (persistent-object-objid k)
                                 nil))
                   (data-objid (if (eq (class-of data)
                                       +persistent-object-class+)
                                   (persistent-object-objid data)
                                 nil)))
	      (when (or (null downcase-search-string)
		        (and downcase-key
			     (search downcase-search-string downcase-key)))
		(if (and data-objid
                         (p-symbolp data-objid))
                    (let ((key-string (if (and (stringp key)
                                               (= key-objid
                                                  (p-symbol-name data-objid
                                                                 :objid)))
					  (sh-pprint-symbol
					   (persistent-object-objid p-heap)
					   data-objid)
			                (format nil "~A" key)))
                          (defined (if (p-fboundp data-objid)
                                       " (defined)"
                                     "")))
                      (if (p-boundp data-objid)
			  (format t "~A, value: ~A~A~%"
                                  key-string
                                  (p-symbol-value data-objid :object)
                                  defined)
		        (format t "~A~A~%" key-string defined)))
		  (format t "~S, data: ~A~%" key data))))
            t))
      (cond
       ((stringp search-string)
        (setf downcase-search-string (string-downcase search-string))
        (mapbtree #'show-apropos btree
                  :key-depth :object
                  :data-depth :object))
       ((and search-string (symbolp search-string))
        (setf downcase-search-string
	      (string-downcase (symbol-name search-string)))
        (mapbtree #'show-apropos btree
                  :key-depth :object
                  :data-depth :object))
       (t
        (mapbtree #'show-apropos btree
                  :key-depth :object
                  :data-depth :object)))))
  (values))

;;; ---------------------------------------------------------------------------
(defun p-apropos-classes (&optional class-name)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{class-name}}
      {a string}
\\Purposelabel
 Print all defined persistent classes in a human-readable form.
 If \\funarg{class-name}\\ is passed, only persistent classes with
 \\funarg{class-name}\\ in their name are printed.
\\Seealsolabel
 \\Fcite{p-find-class};
 \\fcite{p-apropos};
 \\fcite{p-apropos-packages};
 \\fcite{apropos}."

  (p-apropos-btree *symbol->class-table* class-name))

;;; ---------------------------------------------------------------------------
(defun p-apropos-packages (&optional package-name)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{package-name}}
      {a string}
\\Purposelabel
 Print all defined persistent packages in a human-readable form.
 If \\funarg{package-name}\\ is passed, only persistent packages with
 \\funarg{package-name}\\ in their name are printed.
\\Seealsolabel
 \\Fcite{p-apropos};
 \\fcite{p-apropos-classes};
 \\fcite{apropos}."

  (p-apropos-btree *name->package-table* package-name))

;;; ---------------------------------------------------------------------------
(defun p-apropos (&optional search-string &rest packages)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{search-string}}
      {a string}
 \\isa{\\funarg{packages}}
      {a list of names of persistent packages}
\\Purposelabel
 At first see \\fcite{apropos}.

 The difference to {\\bf apropos} is that the symbols printed
 are persistent symbols, i.e.\\ symbols stored in the \\sh.
\\Seealsolabel
 Section \\fcite{symbol ...};
 \\fcite{p-apropos-classes};
 \\fcite{p-apropos-packages};
 \\fcite{apropos}."

  (if packages
      (dolist (p packages)
        (let ((p-package (p-find-package p :objid)))
          (if p-package
              (p-apropos-btree (p-package-internals p-package)
                               search-string))))
    (mapbtree #'(lambda (key data)
                  (declare (ignore key))
                  (p-apropos-btree (p-package-internals data) search-string)
                  t)
              *name->package-table*
              :key-depth :object
              :data-depth :object))
  (values))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
