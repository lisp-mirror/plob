;;;; -*- Package: CL-USER; Mode: LISP; Syntax: ANSI-Common-Lisp -*-------------
;;;; Module	plob-perf.lisp
;;;; Author	Heiko Kirschke
;;;;		kirschke@kogs26.informatik.uni-hamburg.de
;;;; Copyright	(C) 1993, 1994 Heiko Kirschke
;;;; Date	5.9.94
;;;; Description	PLOB performance tests
;;;;	PLOB is an acronym for Persistent Lisp OBjects
;;;;			       =          =    ==
;;;; --------------------------------------------------------------------------

(in-package :cl-user)
(use-package :plob)

(defstruct p-example-structure-class
  (slot-1 nil)
  (slot-3 nil)
  (slot-5 nil))

(defstruct t-example-structure-class
  (slot-1 nil)
  (slot-3 nil)
  (slot-5 nil))

(defclass p-example-clos-class ()
  ((slot-3 :initform nil
           :extent :transient)
   (slot-5 :initform nil
           :extent :cached)
   (slot-7 :initform nil
           :extent :cached-write-through)
   ;;(slot-9 :initform nil
   ;;        :extent :persistent)
   )
  (:metaclass persistent-metaclass))

(defclass p-example-index-class ()
  ((slot-i :initarg :slot-i
           :index (btree :test equal)
           :extent :cached-write-through))
  (:metaclass persistent-metaclass))

(defclass t-example-clos-class ()
  ((slot-3 :initform nil)
   (slot-5 :initform nil)
   (slot-7 :initform nil)
   (slot-9 :initform nil)))

(defmacro show-runtime (&rest forms)
  `(time (progn ,@forms)))

(defun time-nothing (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (show-runtime (dotimes (i times) nil)))

(defun time-transactions (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (show-runtime (dotimes (i times) (with-transaction () nil))))

(defun time-transactions-with-store-lock (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (show-runtime (dotimes (i times) (with-transaction
                                    ()
                                    (write-lock-store) nil))))

(defun time-make-instance-in-one-transaction
       (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (show-runtime (with-transaction
                 ()
                 (dotimes (i times)
                   (make-instance 'p-example-clos-class)))))

(defun time-make-instance-in-one-transaction-with-store-lock
       (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (show-runtime (with-transaction
                 ()
                 (write-lock-store)
                 (dotimes (i times)
                   (make-instance 'p-example-clos-class)))))

(defun time-allocate-cons (&optional (times 10000))
  (format t "Performing ~A times ...~%" times)
  (show-runtime (dotimes (i times) (p-allocate-cons))))

(defun time-setf-cons (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (show-runtime 
   (with-transaction ()
     (dotimes (i times)
       (let ((objid (p-allocate-cons)))
	 (setf (p-car objid) 1)))))
  (show-runtime 
   (with-transaction ()
     (dotimes (i times)
       (let ((objid (p-allocate-cons)))
	 (setf (p-car objid) 1)
	 (setf (p-cdr objid) 2))))))

(defun time-simple-cons (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (let ((objid nil)
        (test-list (make-list times :initial-element times)))
      (show-runtime
       (with-transaction ()
	 (setf objid (store-object test-list))))
      (clear-cache)
      (show-runtime
       (with-transaction ()
	 (load-object objid))))
  nil)

(defun time-symbol-cons (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (let ((objids (make-array times)))
    (show-runtime
     (with-transaction ()
       (dotimes (i times)
	(let ((empty-list (list 'symbol-1 'symbol-2)))
	  (setf (svref objids i)
	    (persistent-object-objid 
	     (store-object empty-list)))))))
    (clear-cache)
    (show-runtime
     (with-transaction ()
       (dotimes (i times)
	 (load-object (svref objids i)))))))

(defun time-strings (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (let ((objids (make-array times :initial-element nil)))
    (show-runtime
     (dotimes (i times)
       (let ((str (make-string 32 :initial-element #\Space)))
         (setf (aref objids i) (store-object str))))
     (clear-cache)
     (show-runtime
       (dotimes (i times)
         (load-object (aref objids i)))))))

(defun time-setf-vector (&optional (times 1000))
  (let ((objid (p-allocate-vector times)))
    (format t "Write vector with ~A elements ...~%" times)
    (show-runtime
     (setf (p-vector objid) (make-array times :initial-element times)))
    (format t "Write vector with ~A elements, one transaction, one vector lock ...~%"
            times)
    (show-runtime
     (with-transaction ()
       (plob::with-write-lock
	   (*default-persistent-heap* objid nil :nocache
				      plob::+vector-type-tag+ nil)
	   (dotimes (i times)
	     (setf (p-svref objid i) i)))))
    (format t "Write vector with ~A elements, one transaction, ~A element locks ...~%"
            times times)
    (show-runtime
     (with-transaction ()
	 (dotimes (i times)
	   (setf (p-svref objid i) i))))
    (format t "Write vector with ~A elements, ~A transactions, ~A element locks ...~%"
            times times times)
    (show-runtime
     (dotimes (i times)
       (setf (p-svref objid i) i)))))

(defun time-vector (&optional (times 1000))
  (let ((objid (p-allocate-vector times)))
    (setf (p-vector objid) (make-array times :initial-element times))
    (clear-cache)
    (format t "Read vector with ~A elements ...~%" times)
    (show-runtime
     (load-object objid))
    (format t "Read vector with ~A elements, one transaction, one vector lock ...~%"
            times)
    (show-runtime
     (with-transaction ()
       (plob::with-read-lock
	   (*default-persistent-heap* objid :nocache
                                      plob::+vector-type-tag+ nil)
	 (dotimes (i times)
	   (p-svref objid i)))))
    (format t "Read vector with ~A elements, one transaction, ~A element locks ...~%"
            times times)
    (show-runtime
     (with-transaction ()
       (dotimes (i times)
	 (p-svref objid i))))
    (format t "Read vector with ~A elements, ~A transactions, ~A element locks ...~%"
            times times times)
    (show-runtime
     (dotimes (i times)
       (p-svref objid i)))))
 
(defun time-big-array (&optional (times 10))
  (format t "Performing ~A times ...~%" times)
  (let ((dummy nil))
    (show-runtime
     (dotimes (i times)
        (let ((big-array (make-array '(256 256 3)
				     :element-type 'single-float
				     :initial-element 1.0e0)))
	  (setf dummy  big-array)))))
  (let ((objids (make-array times)))
    (show-runtime
     (with-transaction ()
       (dotimes (i times)
	 (let ((big-array (make-array '(256 256 3)
				      :element-type 'single-float
				      :initial-element 1.0e0)))
	   (setf (svref objids i)
		(persistent-object-objid 
		 (store-object big-array)))))))
    (clear-cache)
    (show-runtime
     (with-transaction ()
      (dotimes (i times)
        (load-object (svref objids i)))))))

(defun time-make-instance (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (p-delete-class 'p-example-clos-class)
  (make-instance 'p-example-clos-class)
  (let ((dummy nil))
    (format t "Making ~A transient instances ...~%" times)
    (show-runtime
     (dotimes (i times)
       (let ((object (make-instance 't-example-clos-class)))
	 (setf dummy object))))
    (let ((objids (make-array times)))
      (format t "~%~%Making ~A persistent instances ...~%" times)
      (show-runtime
       (with-transaction ()
	 (dotimes (i times)
	   (let ((object (make-instance 'p-example-clos-class)))
	     (setf (svref objids i)
	       (persistent-object-objid object))
	    (setf dummy object)))))
      (clear-cache)
      (p-find-class 'p-example-clos-class)
      (format t "~%~%Loading ~A persistent instances ...~%" times)
      (show-runtime
       (with-transaction ()
	 (dotimes (i times)
	   (setf dummy (load-object (svref objids i)))))))))

(defun time-setf-slot-value (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (let ((p-object (make-instance 'p-example-clos-class))
        (t-object (make-instance 't-example-clos-class)))
    (format t "Transient slot-value:~%")
    (show-runtime
     (dotimes (i (* 10 times))
       (setf (slot-value t-object 'slot-3) i)))
     (dolist (slot '(slot-3 slot-5 slot-7
                            ;; slot-9
                            ))
       (format t "~%~%Persistent (setf slot-value) of ~A:~%" slot)
        (show-runtime
         (with-transaction ()
          (dotimes (i times)
	    (setf (slot-value p-object slot) i)))))))

(defun time-slot-value (&optional (times 1000))
  (format t "Performing ~A times ...~%" times)
  (let ((dummy nil)
        (p-object (make-instance 'p-example-clos-class))
        (t-object (make-instance 't-example-clos-class)))
    (format t "Transient slot-value:~%")
    (show-runtime
     (dotimes (i (* 10 times))
       (setf dummy (slot-value t-object 'slot-3))))
    (dolist (slot '(slot-3 slot-5 slot-7
                           ;; slot-9
                           ))
      (format t "~%~%Persistent slot-value of ~A:~%" slot)
      (show-runtime
       (with-transaction ()
        (dotimes (i times)
          (setf dummy (slot-value p-object slot))))))))

(defun time-index (&optional (times 500))
  (p-delete-class 'p-example-index-class)
  (make-instance 'p-example-index-class)
  (format t "Performing ~A times ...~%" times)
  (format t "Forward insert ...~%")
  (show-runtime
   (with-transaction ()
    (dotimes (i times)
      (make-instance 'p-example-index-class
                     :slot-i (format nil "Slot-I with string ~A" i)))))
  (clear-cache)
  (p-find-class 'p-example-index-class)
  (format t "~%~%Backward insert ...~%")
  (show-runtime
   (with-transaction ()
    (dotimes (i times)
      (make-instance 'p-example-index-class
                     :slot-i (format nil "Slot-I with string ~A" (- (* times 2) i))))))
  (clear-cache)
  (p-find-class 'p-example-index-class)
  (show-runtime
   (with-transaction ()
    (p-select 'p-example-index-class
              :where 'slot-i)))
  nil)


;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
