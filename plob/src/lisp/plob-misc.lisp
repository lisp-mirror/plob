;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-misc.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	10.2.94
;;;; Description	PLOB miscellaneous functions
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
      {a regular expression}
\\Purposelabel
 Print the contents of \\funarg{btree}\\ in a human-readable form.
 If \\funarg{search-string}\\ is passed, only BTree entries with
 \\funarg{search-string}\\ in their key are printed.
\\Seealsolabel
 \\Fcite{make-btree};
 \\fcite{p-apropos};
 \\fcite{p-make-regex}."

  (let ((p-heap *default-persistent-heap*))
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
              (if (and data-objid (p-symbolp data-objid))
                  (let ((key-string
                         (if (and (stringp key)
                                  (= key-objid (p-symbol-name data-objid :objid)))
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
                (format t "~S, data: ~A~%" key data)))
            t))
      (cond
       ((stringp search-string)
        (let ((filter (p-make-regex search-string :icase t)))
          (mapbtree #'show-apropos btree
                    :key-depth :object
                    :data-depth :object
                    :filter filter)
          (p-destroy filter)))
       ((and search-string (symbolp search-string))
        (let ((filter (p-make-regex (symbol-name search-string)
                                     :icase t)))
          (mapbtree #'show-apropos btree
                    :key-depth :object
                    :data-depth :object
                    :filter filter)
          (p-destroy filter)))
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
      {a regular expression}
\\Purposelabel
 Print all defined persistent classes in a human-readable form.
 If \\funarg{class-name}\\ is passed, only persistent classes with
 \\funarg{class-name}\\ in their name are printed.
\\Seealsolabel
 \\Fcite{p-find-class};
 \\fcite{p-apropos};
 \\fcite{p-apropos-packages};
 \\fcite{apropos};
 \\fcite{p-make-regex}."

  (p-apropos-btree *symbol->class-table* class-name))

;;; ---------------------------------------------------------------------------
(defun p-apropos-packages (&optional package-name)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{package-name}}
      {a regular expression}
\\Purposelabel
 Print all defined persistent packages in a human-readable form.
 If \\funarg{package-name}\\ is passed, only persistent packages with
 \\funarg{package-name}\\ in their name are printed.
\\Seealsolabel
 \\Fcite{p-apropos};
 \\fcite{p-apropos-classes};
 \\fcite{apropos};
 \\fcite{p-make-regex}."

  (p-apropos-btree *name->package-table* package-name))

;;; ---------------------------------------------------------------------------
(defun p-apropos (&optional search-string &rest packages)
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{search-string}}
      {a regular expression}
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
 \\fcite{apropos};
 \\fcite{p-make-regex}."

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

;;; ---------------------------------------------------------------------------
(defun p-compare (first second &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{first}}
      {a persistent object}
 \\isa{\\funarg{second}}
      {a persistent object}
\\Valueslabel
 The returned value indicates if the passed objects are equal:
 \\begin{description}
 \\item[\\lisp{:equal}] Both objects are equal.
 \\item[\\lisp{:eql}] Both objects are equal or identical.
 \\item[\\lisp{:eq}] Both objects are identical,
   i.e.\\ one and the same object.
 \\item[\\lisp{:not-equal}] Both objects are not equal.
 \\item[\\lisp{:not-eql}] Both objects are neither equal nor identical.
 \\item[\\lisp{:not-eq}] Both objects are not identical. This return
   value indicates that the objects passed to p-compare are not
   comparable at all.
 \\item[\\lisp{:less-equal}] The first object is less or equal
   compared to the second object.
 \\item[\\lisp{:less}] The first object is less
   compared to the second object.
 \\item[\\lisp{:greater}]The first object is greater
   compared to the second object.
 \\item[\\lisp{:greater-equal}]The first object is greater or equal
   compared to the second object.
 \\end{description}
\\Purposelabel
  Compare two persistent objects. p-compare works only on
  persistent objects, not on transient objects. Both objects
  may be a composition of lists and vectors, as long as the
  objects structure is equal.
\\Remarkslabel
  Passing transient (i.e., not stored) objects to p-compare
  raises errors. When one of the objects to be compared should be
  a regex, the regex object has to be given as first argument.
\\Seealsolabel
 \\Fcite{p-make-regex}."
  (assert-open-session-p p-heap)
  (sh-compare (persistent-object-objid p-heap)
	      (persistent-object-objid first)
	      (p-type-tag-of first p-heap)
	      (persistent-object-objid second)
	      (p-type-tag-of second p-heap)))

;;; ---------------------------------------------------------------------------
(defun p-make-regex (pattern
		     &key
		     ;; regcomp() flags
		     (basic nil) (extended t) (icase nil) (nosub t) (newline nil)
		     (nospec nil) (notmatching nil)
		     ;; regexec() flags
		     (notbol nil) (noteol nil)
		     (trace nil) (large nil) (backref nil)
		     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{pattern}}
      {a string with a regular expression, see the user's
       guide, section `regex(7)' for explanations}
%% regcomp
 \\isa{\\keyarg{basic}} %% REG_BASIC
  {a synonym for 0, provided as a counterpart to \\keyarg{extended}
   to improve readability. This is an extension, compatible with but
   not specified by POSIX 1003.2, and should be used with caution in
   software intended to be portable to other systems}
 \\isa{\\keyarg{extended}} %% REG_EXTENDED
  {a flag to compile modern (``extended'') REs, rather than the obsolete
   (``basic'') REs that are the default}
 \\isa{\\keyarg{icase}} %% REG_ICASE
  {a flag to compile for matching that ignores upper/lower case distinctions}
 \\isa{\\keyarg{nosub}} %% REG_NOSUB
  {a flag to compile for matching that need only report success or failure, not
   what was matched}
 \\isa{\\keyarg{newline}} %% REG_NEWLINE
  {a flag to compile for newline-sensitive matching. By default, newline is a
   completely ordinary character with no special meaning in either REs
   or strings. With this flag, `[\\^{\\ }' bracket expressions and `.'
   never match newline, a `\\^{\\ }' anchor matches the null string
   after any newline in the string in addition to its normal function,
   and the `\\$' anchor matches the null string before any newline
   in the string in addition to its normal function}
 \\isa{\\keyarg{nospec}} %% REG_NOSPEC
  {a flag to compile with recognition of all special characters turned off.
   All characters are thus considered ordinary, so the ``RE'' is a literal
   string. This is an extension, compatible with but not specified by
   POSIX 1003.2, and should be used with caution in software intended
   to be portable to other systems. \\keyarg{extended}\\ and
   \\keyarg{nospec} may not be used in the same call to regcomp}
 \\isa{\\keyarg{notmatching}}
  {a flag to signal a match for strings not matching the pattern}
%% regexec
 \\isa{\\keyarg{notbol}} %% REG_NOTBOL
  {a flag for the first character of the string is not the beginning of
   a line, so the `\\^{\\ }' anchor should not match before it. This does not
   affect the behavior of newlines under \\keyarg{newline}.}
 \\isa{\\keyarg{noteol}} %% REG_NOTEOL
  {a flag for the NUL terminating the string does not end a line, so the
   `\\$' anchor should not match before it. This does not affect
   the behavior of newlines under \\keyarg{newline}}
 \\isa{\\keyarg{trace}}{a flag for racing of execution}
 \\isa{\\keyarg{large}}{a flag for Force large representation}
 \\isa{\\keyarg{backref}}{a flag for force use of backref code}
\\Valueslabel
  A persistent object of class regex is returned.
\\Purposelabel
  Create a persistent regular expression.
\\Remarkslabel
  The expression is compiled on its first usage (i.e., match).
  For checking the syntax of a regular expression, use
  \\fcite{p-compile-regex}. After creation, the pattern cannot
  be changed. Destroy the object (see \\fcite{p-destroy}) and
  create a new one.
\\Seealsolabel
  \\Fcite{p-compile-regex}\\ and the user's guide for the syntax of
  regular expressions. \\Fcite{p-compare}."
  (declare (type string pattern))
  (assert-open-session-p p-heap)
  (let ((regcomp-flags 0) (regexec-flags 0))
    (declare (type fixnum regcomp-flags regexec-flags))
    ;; regcomp() flags
    (when basic
      (setf regcomp-flags (logior regcomp-flags +regex-basic+)))
    (when extended
      (setf regcomp-flags (logior regcomp-flags +regex-extended+)))
    (when icase
      (setf regcomp-flags (logior regcomp-flags +regex-icase+)))
    (when nosub
      (setf regcomp-flags (logior regcomp-flags +regex-nosub+)))
    (when newline
      (setf regcomp-flags (logior regcomp-flags +regex-newline+)))
    (when nospec
      (setf regcomp-flags (logior regcomp-flags +regex-nospec+)))
    (when notmatching
      (setf regcomp-flags (logior regcomp-flags +regex-not-matching+)))
    ;; regexec() flags
    (when notbol
      (setf regexec-flags (logior regexec-flags +regex-notbol+)))
    (when noteol
      (setf regexec-flags (logior regexec-flags +regex-noteol+)))
    (when trace
      (setf regexec-flags (logior regexec-flags +regex-trace+)))
    (when large
      (setf regexec-flags (logior regexec-flags +regex-large+)))
    (when backref
      (setf regexec-flags (logior regexec-flags +regex-backref+)))
    (let ((objid (sh-make-regex (persistent-object-objid p-heap)
				pattern regcomp-flags regexec-flags)))
      (when objid (make-persistent-object objid +regex-type-tag+)))))

;;; ---------------------------------------------------------------------------
(defun p-compile-regex (regex &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isa{\\funarg{regex}}
      {a persistent object of class regex}
\\Valueslabel
  If the compilation was successfull, \\lisp{nil}\\ is returned.
  If the compilation was not successfull, two values are returned:
 \\begin{itemize}
 \\item An error number
 \\item A string containing an error message
 \\end{itemize}
\\Purposelabel
  Compile a regular expression.
\\Remarkslabel
  Since the expression is compiled always on its first usage
  (i.e., match), it is not necessary to call p-compile-regex
  explicite. Use this function to check if the syntax of a
  regular expression is correct.
\\Seealsolabel
  \\Fcite{p-make-regex}."
  (assert-open-session-p p-heap)
  (sh-compile-regex (persistent-object-objid p-heap)
                    (persistent-object-objid regex)))

;;;; Local variables:
;;;; buffer-file-coding-system: raw-text-unix
;;;; End:
