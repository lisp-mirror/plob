;;;; --------------------------------------------------------------------------
;;;; Module	plob-test.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Date	19.11.93
;;;; Description	PLOB test procedures
;;;; --------------------------------------------------------------------------

(in-package :cl-user)

(use-package :plob)

;;; ---------------------------------------------------------------------------

(defun tst-time (&optional with-store-profiler with-load-profiler)
  (declare (optimize speed))
  (let ((number-of-list-elements (if with-store-profiler 1000 1000)))
    (format t "(make-list ~A)~%" number-of-list-elements)
    (time (setf l (make-list number-of-list-elements)))
    ;; (format t "(dolist (e) (make-list ~A))~%" number-of-list-elements)
    ;; (time (dolist (e l) ()))
    (clear-cache plob::*default-persistent-heap*)
    (if with-store-profiler
        (progn
          (set-up-profiler :package '(:plob)
                           :kind :profile
                           :interval 10)
          (profile (setf p (store-object l))))
      (progn
	(format t "(store-object (make-list ~A))~%" number-of-list-elements)
	(time (setf p (store-object l)))))
    (clear-cache plob::*default-persistent-heap*)
    (if with-load-profiler
        (progn
          (set-up-profiler :package '(:plob)
                           :kind :profile
                           :interval 10)
          (profile (setf m (load-object p))))
      (progn
        (format t "(load-object ~A)~%" p)
        (time (setf m (load-object p)))))
    nil))

(defmacro tst-prof (expr)
  `(progn
     (set-up-profiler :package '(:plob)
		      :kind :profile
		      :interval 10)
     (profile ,expr)))



; Example for circular list from Steele, Common LISP, p. 537:
(progn
  (setf *print-circle* t)
  (setq x (list 'p-test 'q-test))
  (setq y (list (list 'a-test 'b-test) x 'foo-test x))
  (rplacd (last y) (cdr y)))

; displaced-to arrays:
(setf a (make-array 12 :adjustable t))
(setf b (make-array 10 :displaced-to a))
(setf c (make-array 10 :displaced-to b))

(defun tst (sample-object)
  (declare (optimize (speed 3) (debug 1) (safety 1)))
  (clear-cache plob::*default-persistent-heap*)
  (setf p (store-object sample-object))
  (setf q (persistent-object-objid p))
  (clear-cache plob::*default-persistent-heap*)
  (setf m (load-object p))
  (clear-cache plob::*default-persistent-heap*)
  (format t "(load-object ~A)=~S~%" p m)
  (values))

(defun atst ()
  (setf a (make-array '(3 4) :adjustable t))
  (setf b (make-array (array-total-size a) :displaced-to a)))

(defun rtst ()
  ;; (plob::sh-load)
  (setf l (store-object '(1 2 3) plob::*default-persistent-heap*))
  (begin-transaction plob::*default-persistent-heap*)
  (setf (plob::p-car l) "ha")
  (cancel-transaction plob::*default-persistent-heap*)
  (clear-cache)
  (let ((result (load-object l)))
    result))

(defun stst ()
  ;; (plob::sh-load)
  (setf l (store-object '(1 2 3) plob::*default-persistent-heap*))
  (begin-transaction plob::*default-persistent-heap*)
  (begin-transaction plob::*default-persistent-heap*))

(defstruct (struct-1)
  (slot-1 "Slot-1")
  (slot-2 "Slot-2" :type fixnum))
(setf (class-extent (find-class 'struct-1)) :cached-demand-load)

(defstruct (struct-2)
  (slot-0 "Slot-0")
  (slot-1 "Slot-1")
  ;(slot-1a "Slot-1a")
  (slot-1b "Slot-1b")
  (slot-2 "Slot-2" :type fixnum)
  )
(setf (class-extent (find-class 'struct-2)) :cached-demand-load)

;; (setf (slot-extent 'slot-0 (find-class 'struct-2)) :cached)
;; (setf (slot-extent 'slot-1 (find-class 'struct-2)) :demand-load)

(defun test-defstruct ()
  (setf *print-circle* t)
  (let ((struct-1 (make-struct-1 :slot-1 "Slot-1 of struct-1"))
	(struct-2 (make-struct-2 :slot-0 "Slot-0 of struct-2")))
    (setf (struct-1-slot-2 struct-1) struct-2)
    (setf (struct-2-slot-1 struct-2) struct-1)
    (let ((plob::*default-depth* :cached))
      (clear-cache)
      (with-transaction
       ()
       (write-lock-store)
       (setf #!*defstruct* struct-1)))))

(setf *cache* (plob::persistent-heap-objid->object-cache
	       plob::*default-persistent-heap*))

(defstruct (s-2)
  (slot-0 "Slot-0")
  (slot-1 "Slot-1")
  ;(slot-1a "Slot-1a")
  ;(slot-1b "Slot-1b")
  ;(slot-1c "Slot-1c")
  (slot-2 "Slot-2" :type fixnum))
;;(setf (slot-extent 'slot-1 (find-class 's-2)) :transient)

(setf (slot-deferred 'slot-1 (find-class 's-2)) 100)

;; (open-heap)

(defclass foobar ()
  ((slot-1 :accessor slot-1 :initform "Slot-1")
   (slot-2 :accessor slot-2 :initform "Slot-2")
   (slot-2b :accessor slot-2b)
   (slot-4 :accessor slot-4 :allocation :class :initform "jaja")
   (slot-5 :accessor slot-5 :allocation :class)
   (slot-3 :accessor slot-3)
   ))

(setf (class-constructor (find-class 'foo)) 'make-foo)
(setf (class-dependent (find-class 'foobar)) :read)

(defun make-foo (objid &optional p-heap depth)
  (format t "Hier ist make-foo
slot-1 ~A
slot-2 ~A
slot-4 ~A~%"
          (slot-value objid 'slot-1)
          (slot-value objid 'slot-2)
          (slot-value objid 'slot-4))
  (let ((object (make-instance 'foo)))
    (setf (slot-value object 'slot-1) (slot-value objid 'slot-1))
    (setf (slot-value object 'slot-2) (slot-value objid 'slot-2))
    object))

(defclass bar (foo)
  ((bar-slot :accessor bar-slot :initform t)))

(defclass p-foo ()
  (
   (slot-soso
    :accessor slot-soso
    :initarg :slot-soso
    :initform "Schlott soso.")
   (shared-slot
    :accessor shared-slot
    :allocation :class
    :initform "jetzt ein alter shared-slot")
   (shared-slot-2
    :accessor shared-slot-2
    :allocation :class
    :initform "shared-slot-2")
   (slot-1 :accessor slot-1
           :initarg :slot-1ab
           :index (btree :test equal))
   (slot-4b :accessor slot-4b
           :initarg :slot-4b
           :initform "Schlott numero vier bee."
           :deferred 10
           :extent :transient)
   (slot-1y :accessor slot-1y :initarg :slot-1y :initform "Slot 1y")
   ;(slot-1z :accessor slot-1z :initarg :slot-1z :initform "Slot 1z")
   ;(slot-2 :accessor slot-2 :initarg :slot-2 :initform "2")
   ;(slot-2b :accessor slot-2b :initarg :slot-2b)
   ;(slot-2c :accessor slot-2c :initarg :slot-2c)
   (slot-2d :accessor slot-2d
            :extent :persistent
            :initarg :slot-2d
            :initform "2d")
   (slot-2e :accessor slot-2e :initarg :slot-2e :initform "Slot 2e")
   (slot-3 :accessor slot-3 :initarg :slot-3 :initform "3")
   (slot-4 :accessor slot-4
           :initarg :slot-4
           :initform "Schlott numero vier."
           :extent :transient)
   )
  ;(:constructor make-pfoo)
  (:metaclass persistent-metaclass))

(defclass p-bar (p-foo)
  ((bar-slot :accessor bar-slot :index btree))
  (:metaclass persistent-metaclass))

(defgeneric slot-1 (clos::object))

(defvar h (make-hash-table))
(progn
  (setf (gethash 1 h) "Eins")
  (setf (gethash 2 h) "Zwei")
  (setf (gethash 3 h) "Drei"))

;;; ---------------------------------------------------------------------------

(defvar *picture* nil)
(defvar *p* nil)

;;; ---------------------------------------------------------------------------
#+huhu
(progn
  (defvar *b* nil)
  (setf *b* (make-btree))
  (setf (getbtree 1 *b*) "Eins.")
  (setf (getbtree 2 *b*) "Zwei.")
  (setf (getbtree 20.0d0 *b*) "Zwanzig.")
  (setf (getbtree 3.0s0 *b*) "Short drei.")
  (setf (getbtree 4.0e0 *b*) "Single vier.")
  (mapbtree #'(lambda (k d) (format t "k ~A, d ~A~%" k d) t) *b* :>= 1.0e0))

#+haha
(progn
  (defvar *c* (make-btree))
  (setf (getbtree "aaa" *c*) "Eins.")
  (setf (getbtree "bbb" *c*) "Zwei.")
  (setf (getbtree "ccc" *c*) "Short drei.")
  (setf (getbtree "ddd" *c*) "Single vier."))

;;; ---------------------------------------------------------------------------
(defun st-pic ()
  (setf *picture* (make-array '(256 256 3)
			      ;; :element-type 'single-float
			      :element-type '(unsigned-byte 8)
                              :initial-element 64))
  (time (setf *p* (store-object *picture*)))
  (setf *picture* nil)
  *p*)
(defun ld-pic ()
  (time (load-object *p*))
  (values))

;;; ---------------------------------------------------------------------------
#+LispWorks
(foreign:define-foreign-function (test-close "_SH_short_close")
                                 ())

(setf a #!non-existing-symbol)

;;; ---------------------------------------------------------------------------
(progn
  (setf c (plob::make-persistent-object (plob::p-allocate-cons)))
  (setf h1 (plob::make-persistent-heap
            (plob::p-allocate nil plob::+heap-type-tag+)))
  (setf h2 (plob::make-persistent-heap
            (plob::p-allocate nil plob::+heap-type-tag+))))
(progn
  (begin-transaction nil h1)
  (begin-transaction nil h2))
(progn
  (plob::p-set-lock h1 c plob::+cons-type-tag+ plob::+lock-vector-write+))

;;; ---------------------------------------------------------------------------
(defclass class-with-objids ()
  ((slot-1 :initarg :slot-1 :initform nil)
   (slot-2 :initarg :slot-2 :initform nil))
  (:metaclass persistent-metaclass)
  (:extent :object))

;;; ---------------------------------------------------------------------------
(defclass p-class ()
  ((p-slot-1 :initform "p-slot-1 slot of class p-class"
	     :accessor p-slot-1)
   (p-slot-2 :initform "p-slot-2 slot of class p-class"
	     :accessor p-slot-2)
   (p-slot-3 :initform "p-slot-3 slot of class p-class"
	     :accessor p-slot-3
	     :extent :transient))
  (:metaclass persistent-metaclass))

;;; ---------------------------------------------------------------------------
(defclass t-class ()
  ((t-slot-1 :initform "t-slot-1 slot of class t-class"
	     :accessor t-slot-1)
   (t-slot-2 :initform "t-slot-2 slot of class t-class"
	     :accessor t-slot-2)))

;;; ---------------------------------------------------------------------------
(defun check-p ()
  (let* ((instance (make-instance 'p-class))
	 (objid (make-persistent-object instance)))
    (clear-cache)
    (setf *p* (load-object objid))))

;;; ---------------------------------------------------------------------------
(defun check-q ()
  (let* ((p-instance (make-instance 'p-class))
	 (t-instance-1 (make-instance 't-class))
	 (t-instance-2 (make-instance 't-class))
	 (objid (make-persistent-object p-instance)))
    (setf (p-slot-3 p-instance) t-instance-1)
    (setf (p-slot-2 p-instance) t-instance-2)
    (clear-cache)
    (setf *o* objid)
    (setf *q* (load-object objid))))

;;; ---------------------------------------------------------------------------
(defun check-t ()
  (let* ((instance (make-instance 't-class))
	 (objid (store-object instance)))
    (clear-cache)
    (p-find-class 't-class)
    ;; (trace clos::slot-value-using-class)
    (let ((plob::*verbose* 99))
      (setf *t* (load-object objid)))
    ;; (untrace clos::slot-value-using-class)
    ))

;;; ---------------------------------------------------------------------------
(defun tst-bignum (number)
  (setf #!*bignum* number)
  (clear-cache)
  (let ((p-bignum #!*bignum*))
    (format t "               (= #x~X~%                  #x~X) ==> ~A~%~
               prints  ~A~%number  ~A~%returns ~A~%"
	    number p-bignum (= number p-bignum)
	    #!(*bignum* :object)
	    number
	    p-bignum)
    p-bignum))

;;; ---------------------------------------------------------------------------
(defun tst-array ()
  (let ((a (make-array '(7 3) :element-type '(unsigned-byte 32))))
    (dotimes (i (first (array-dimensions a)))
      (dotimes (j (second (array-dimensions a)))
	(setf (aref a i j) (+ (* i (second (array-dimensions a))) j))))
    (format t "a ~A~%" a)
    (setf *a* a)
    (setf #!*a* a)
    (clear-cache)
    (let ((b (make-array (array-dimensions a)
			   :element-type (array-element-type a)
			   :initial-element 0)))
      (setf b #!*a*)
      (format t "#!*a* ~A~%" b))))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
