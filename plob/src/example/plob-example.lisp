;;;; -*- Package: CL-USER; Mode: LISP; Syntax: ANSI-Common-Lisp; Readtable: plob-readtable -*-------------
;;;; Module	plob-example.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Copyright	(C) 1993, 1994 Heiko Kirschke
;;;; Date	11.4.94
;;;; Description	PLOB example
;;;;	PLOB is an acronym for Persistent Lisp OBjects
;;;;			       =          =    ==
;;;;	This example is derived from the WOOD file example.lisp
;;;;    Before working on this example after having worked through
;;;;    the code located in plob-quick-tour.lisp, you should delete
;;;;    class PERSON by evaluating (p-delete-class 'person).
;;;; --------------------------------------------------------------------------

(in-package :cl-user)
(use-package :plob)

;;; ---------------------------------------------------------------------------
(defclass count-instances ()

  ((instances
    :accessor instance-count
    :allocation :class
    :initform 0
    :type fixnum
    :documentation "
 The value in this slot is persistent. It is incremented for each
 transient instance created."))

  (:metaclass persistent-metaclass)

  (:documentation "
\\Purposelabel
 This is a persistent class for counting its generated instances.

 The class is made persistent by the class option \\lisp{(:metaclass
 persistent-metaclass)}; this tells \\clos\\ to select the
 \\plob\\ \\clsmc\\ \\class{persistent-metaclass}\\ as \\cls\\ for the
 \\clsmo\\ which represents the \\cls\\ \\class{count-instances}.
\\Seealsolabel
 \\Fcite{persistent-metaclass};
 \\fcite{(setf slot-extent)}."))

;;; ---------------------------------------------------------------------------
#+lisp-doc
(defgeneric clos::initialize-instance (clos::instance &rest clos::initargs)
  (:documentation "
 See \\fcite{initialize-instance}."))

;;; ---------------------------------------------------------------------------
(defmethod initialize-instance :after ((object count-instances)
                                       &rest initargs
                                       &key objid)
  "
 Increment the number of instances stored in \\funarg{object}.

 When the \\keyarg{objid}\\ argument is given within the object's
 initialization, the object is not really new created but loaded from
 persistent memory."
  (declare (ignore initargs))
  (unless objid
    (incf (instance-count object))))

;;; ---------------------------------------------------------------------------
;; define the PERSON class

(defclass person (count-instances)

  ((first-name
    :initarg :first-name
    :accessor person-first-name
    :extent :cached
    :documentation "
 A \\slt\\ with \\lisp{:extent :cached}.
 This means that the \\slt\\ is represented in both transient and
 persistent memory but its transient state is only saved to persistent
 memory
 \\begin{enumerate}
 \\item At object initialization (i.e.\\ when the object is created and
   \\slt[s]\\ are initialized with \\keyarg{initarg}\\ arguments).
 \\item By calls to the \\fcite{store-object}\\ with the object
   (a \\class{person}\\ instance in this case) to store as argument.
 \\end{enumerate}")

   (last-name
    :initarg :last-name
    :accessor person-last-name
    :extent :cached)

   (age
    :initarg :age
    :accessor person-age
    :extent :cached
    :documentation "
 A \\slt\\ which once had an \\lisp{:extent :persistent}.
 This means that the \\slt\\ would only be represented in persistent
 memory; each read or write access would be promoted immediately to
 persistent memory (`once' means the
 earlier-non-client/server-version of \\plob).

 Since such a representation implies a heavy overhead for each slot
 access, its usage is strongly discouraged now (`now' means compared
 to the earlier-non-client/server-version of \\plob).")

   (sex
    :initarg :sex
    :accessor person-sex)

   (occupation
    :initarg :occupation
    :accessor person-occupation
    :extent :cached)

   (soc-sec-#
    :initarg :soc-sec-#
    :accessor person-soc-sec-#
    :extent :cached-write-through
    :index (btree :test equal :pagesize 32)
    :documentation "
 This slot holds the social security number of a person.

 This is an \\lisp{:extent :cached-write-through}\\ \\slt\\ with a BTree
 index defined on it; the \\lisp{:test equal}\\ option tells the
 BTree to use \\lisp{equal}\\ testing for key compares. Each write
 access on this \\slt\\ will insert the new value into a BTree;
 instances can be selected by the \\fcite{p-select}.

 The \\lisp{:cached-write-through}\\ extent is the default
 \\slt\\ extent for slots of \\clos\\ objects with
 \\lisp{:metaclass persistent-metaclass}\\ if no
 \\lisp{:extent}\\ \\slt\\ option and no \\lisp{:extent}\\ class
 option is specified."))

  (:extent :cached)

  (:metaclass persistent-metaclass)

  (:documentation "
\\Purposelabel
 A class for persistent person data.
\\Seealsolabel
 \\Fcite{count-instances}."))

;;; ---------------------------------------------------------------------------
#+lisp-doc
(defgeneric clos::print-object (clos::instance stream)
  (:documentation "
 See \\fcite{print-object}."))

;;; ---------------------------------------------------------------------------
(defmethod print-object ((object person) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~A, ~A, ~A (objid ~A)"
	    (if (slot-boundp object 'last-name)
		(person-last-name object)
	      "#<unbound last-name>")
	    (if (slot-boundp object 'first-name)
		(person-first-name object)
	      "#<unbound first-name>")
	    (if (slot-boundp object 'occupation)
		(person-occupation object)
	      "#<unbound occupation>")
	    ;; 1996/10/31 HK: Omitting the soc-sec-# from printing won't force
            ;; an active transaction for each printing of the object;
            ;; this speeds up a lot the loading of the object
            ;; representations into the LispWorks inspector:
	    ;; (if (slot-boundp object 'soc-sec-#)
	    ;;     (person-soc-sec-# object)
	    ;;   "#<unbound soc-sec-#>")
	    (persistent-object-objid object))))

;;; ---------------------------------------------------------------------------

(defvar *last-name->person-list* nil
  "
\\Purposelabel
 A transient \\cl\\ variable holding a BTree mapping a last name to a
 list of persons with the same last name. The BTree itself is stored
 in the value cell of the persistent symbol
 \\lisp{*last-name->person-list*}; its value can be `loaded'
 by the \\cl\\ macro reader \\lisp{\\#!*last-name->person-list*}.")

;;; --------------------------------------------------------------------------
(defun setup-last-name->person-list (&optional force)
  "
\\Purposelabel
 Set up a proper persistent object mapping last names to persons."
  (when (or (null *last-name->person-list*)
	    force)
    (unless (p-boundp '*last-name->person-list*)
      ;; #!*last-name->person-list* evaluates to the value of the
      ;; persistent symbol *last-name->person-list*:
      (setf #!*last-name->person-list* (make-btree :test 'equal :pagesize 16)))
    (setf *last-name->person-list* #!*last-name->person-list*)
    (unless (p-select 'person :where 'soc-sec-# := 1 :depth :objid)
      (make-instance 'person
		     :first-name "Heiko"
		     :last-name "Kirschke"
		     :sex 'm
		     :age (multiple-value-bind (second minute hour
						date month year)
			      (get-decoded-time)
			    (declare (ignore second minute hour))
			    (let ((age (- year 1964)))
			      (if (or (< month 6)
				      (and (= month 6) (<= date 19)))
				  (- age 1)
				age)))
		     :occupation "Computer Scientist"
		     :soc-sec-# 1)))
  *last-name->person-list*)

;;; --------------------------------------------------------------------------
(defmethod open-heap :after (&optional url)
  "
\\Purposelabel
 Initialization: if the persistent symbol
 \\lisp{*last-name->person-list*}\\ is not bound,
 it is set to a new persistent btree."
  (setf *last-name->person-list* nil))

;;; --------------------------------------------------------------------------
(defmethod close-heap :before (&optional (with-garbage-collection t))
  "
\\Purposelabel
 De-initialization: Invalidate the cached value of
 \\fcite{*last-name->person-list*}."
  (setf *last-name->person-list* nil))

;;; --------------------------------------------------------------------------

(defmethod initialize-instance :after ((object person) &rest all-keys)
  "Method to maintain the value of \\fcite{*last-name->person-list*}."
  (declare (ignore all-keys))
  (let ((name (when (slot-boundp object 'last-name)
		(person-last-name object))))
    (when name
      (let ((last-name (string-upcase (person-last-name object))))
        (with-transaction ()
	  ;; This (setf (getbtree ...) ...) stores the (cons ...) under key
	  ;; last-name in the btree *last-name->person-list*:
	  (let ((last-name->person-list (setup-last-name->person-list)))
	    (setf (getbtree last-name last-name->person-list :objid)
	      (p-cons object
		      (getbtree last-name
				last-name->person-list
				:objid)
		      :objid)))))))
  object)

;;; ---------------------------------------------------------------------------

(defun find-person-with-soc-sec-# (soc-sec-#)
  "
\\Argumentslabel
 \\isa{\\funarg{soc-sec-\\#}}
      {an instance of class \\class{fixnum}}
\\Purposelabel
 Search a person by its social security number.
\\Seealsolabel
 \\Fcite{p-select}."

  (p-select 'person :where 'soc-sec-# := soc-sec-#))

;;; ---------------------------------------------------------------------------

(defun find-people-from-soc-sec-#
    (from-soc-sec-# &optional (to-soc-sec-# nil to-soc-sec-#-p))
  "
\\Argumentslabel
 \\isa{\\funarg{from-soc-sec-\\#}\\ resp.\\ \\funarg{to-soc-sec-\\#}}
      {an instance of class \\class{fixnum}}
\\Purposelabel
 Search all persons with their social security numbers being in the range
 \\funarg{from-soc-sec-#}\\ to (exclusive) \\funarg{to-soc-sec-#}.
\\Seealsolabel
 \\Fcite{p-select}."

  (let ((selected-people
         (if to-soc-sec-#-p
	     (p-select 'person :where 'soc-sec-#
		       :>= from-soc-sec-# :<= to-soc-sec-#)
	 (p-select 'person :where 'soc-sec-# :>= from-soc-sec-#))))
    (dolist (person selected-people)
      (format t "~A ~A~%" (person-soc-sec-# person) person))))

;;; ---------------------------------------------------------------------------

(defun find-people-with-last-name (last-name)
  "
\\Argumentslabel
 \\isa{\\funarg{last-name}}
      {a string}
\\Purposelabel
 Search all persons with its last name being \\funarg{last-name}."

  (let ((last-name->person-list (setup-last-name->person-list)))
    (getbtree (string-upcase last-name) last-name->person-list)))

;;; ---------------------------------------------------------------------------

(defun print-people-by-soc-sec-# ()
  "Print all people stored so far ordered by their social security number."
  (let ((persons 0))
    (with-transaction
     ()
     ;; The (p-select ...) without any :=, :from or :below argument behind the
     ;; attribute name ('soc-sec-# here) selects all instances
     ;; of class 'person:
     (setup-last-name->person-list)
     (dolist (person (p-select 'person :where 'soc-sec-#))
       (incf persons)
       (format t "~4@A ~S~%"
	       (person-soc-sec-# person)
	       person)))
    (format t "*** Total of ~A persons.~%" persons))
  (values))

;;; ---------------------------------------------------------------------------

(defun print-people-by-last-name ()
  "Print all people stored so far ordered by their last name."
  (let ((persons 0)
        (last-name->person-list (setup-last-name->person-list)))
    (with-transaction ()
      (mapbtree #'(lambda (last-name person-list)
		    (declare (ignore last-name))
		    (dolist (person person-list)
                      (incf persons)
		      (format t "~s~%" person))
		    t)
		last-name->person-list))
    (format t "*** Total of ~A persons.~%" persons))
  (values))

;;; ---------------------------------------------------------------------------

;; Code for creating random PERSON instances.
(defparameter *first-names*
  '(("Alan" . m)
    ("Abraham" . m)
    ("Andrew" . m)
    ("Alice" . f)
    ("Susan" . f)
    ("Bob" . m)
    ("Hillary" . f)
    ("Joe" . m)
    ("Bill" . m)
    ("Matthew" . m)
    ("Gail" . f)
    ("Gary" . m)
    ("Doug" . m)
    ("Christie" . f)
    ("Steve" . m)
    ("Elizabeth" . f)
    ("Melissa" . f)
    ("Karla" . f)
    ("Dan" . m)
    ("Irving" . m))
  "A list with first names used to create random persons.")

(defparameter *last-names*
  '("Smith" "Jones" "Peterson" "Williams" "Kennedy" "Johnson"
    "Riley" "Sylversteen" "Wilson" "Cranshaw" "Ryan" "O'Neil"
    "McAllister")
  "A list with last names used to create random persons.")

(defparameter *occupations*
  '("Student" "Baker" "Candlestick Maker"
    "Engineer" "Hacker" "Tailor" "Cop" "Lawyer" "Doctor"
    "Dentist" "Politician" "Cashier" "Insurance Sales"
    "Advertising")
  "A list with occupations used to create random persons.")

;;; ---------------------------------------------------------------------------

(defun random-person ()
  "Generate a random persistent person."
  (multiple-value-bind (first-name last-name sex)
      (random-name)
    (let* ((occupation (random-element *occupations*))
	   (soc-sec-# (random-soc-sec-#))
	   (person (make-instance 'person
		     :first-name first-name
		     :last-name last-name
		     :sex sex
		     :age (random 100)
		     :occupation occupation
		     :soc-sec-# soc-sec-#)))
      (values person (instance-count person)))))

;;; ---------------------------------------------------------------------------

(defun random-element (sequence)
  "
\\Argumentslabel
 \\isa{\\funarg{sequence}}
      {a sequence}
\\Purposelabel
 Return a random element of \\funarg{sequence}."

  (elt sequence (random (length sequence))))
    
;;; ---------------------------------------------------------------------------

(defun random-name ()
  "
\\Valueslabel
 Three values are returned: A first name, a last name and the sex of
 the first name.
\\Purposelabel
 Generate a random name."

  (let ((first.sex (random-element *first-names*))
        (last (random-element *last-names*)))
    (values
     (car first.sex) 
     last
     (cdr first.sex))))

;;; ---------------------------------------------------------------------------

(defun random-soc-sec-# ()
  "Generate a random social security number."
  (loop
   (let ((soc-sec-# (+ (random 9000) 1000)))
     ;; Make sure that the soc-sec-# is unique:
     (unless (p-select 'person :where 'soc-sec-# := soc-sec-# :depth :objid)
       (return soc-sec-#)))))

;;; ---------------------------------------------------------------------------

(defun store-n-random-people (n)
  "
\\Argumentslabel
 \\isa{\\funarg{n}}
      {an instance of class \\class{fixnum}}
\\Purposelabel
 Store \\funarg{n}\\ random-generated people."

  (with-transaction ()
    (dotimes (i n)
      (random-person))))

;;; ---------------------------------------------------------------------------

(defun clear-people ()
  "Deletes all person data."
  (when (p-find-class 'person)
    (setf (instance-count (clos::class-prototype (p-find-class 'person))) 0))
  (p-delete-class 'count-instances)
  (p-delete-class 'person)
  (p-makunbound '*last-name->person-list*)
  (setf *last-name->person-list* nil))

;;; ---------------------------------------------------------------------------
;;; Demonstration of transactions
;;; ---------------------------------------------------------------------------
(defun show-transaction ()
  "
 Show the usage of transactions."

  (let ((person (random-person)))
    (format t "~%Created ~A~%aged ~A.~%"
            person (person-age person))
    (unwind-protect
	(with-transaction ()
	  (setf (person-age person) "A non-numeric age.")
	  ;; Since class 'person has an :extent :cached, a store of
	  ;; the person instance created above must be forced here:
	  (store-object person)
	  (format t "~%~A~%now aged ~S in the scope of~%~A.~%~%"
		  person (person-age person) *default-persistent-heap*)
	  (unless (integerp (person-age person))
	    (cerror "Confirm the non-numeric person age."
		    "~A~%has non-numeric age ~S.~%~%~
                    If you abort this cerror, ~
		    a transaction rollback is initiated ~
                    which sets the person age back ~
		    to its starting value; ~
                    if you continue from this cerror, ~
		    the transaction is committed; ~
                    the person's age will stay non-numeric.~%"
		    person (person-age person))))
      ;; Since class 'person has an :extent :cached, a reload
      ;; the person instance created above must be forced here,
      ;; since a cancel-transaction is not promoted to transient
      ;; instances:
      (setf person (load-object person))
      (format t "The transaction scope ends here; the age of~%~
                 ~A is~%now ~S.~%~%"
              person (person-age person)))
    person))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
