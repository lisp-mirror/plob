
;;; HK 17.09.92: Interne Repraesentation von LispWorks LISP Objekten.

(use-package 'foreign)
(push "/usr/local/lib/gcc-lib/sparc-sun-sunos4.1.3/2.5.2/lib~A.a"
      foreign::*default-library-name-formats*)
(define-foreign-function (c-lw-register "_fnLWregisterConst")
                         ((|lwObject| :as-is) (|constObj| :fixnum)))
(define-foreign-function (c-lw-put "_fnLWput") ((|lwObject| :as-is)))
(defun rd ()
  (read-foreign-modules "lwobject.a" "-lgcc" "-lc")
  (read-foreign-modules "lwobject.o")
  (c-lw-register nil 0)
  (c-lw-register t 1))
(defun tst (x)
    (c-lw-put x))

(defclass with-2-slots ()
  (slot-1
   slot-2))

(setf *o* (make-instance 'with-2-slots))

(defclass with-4-slots ()
  (slot-1
   slot-2
   (slot-3 :allocation :class)
   (slot-4 :allocation :class)))

(setf *p* (make-instance 'with-4-slots))

(setf *a* (make-array '(2 2)))
(setf (aref *a* 1 1) *a*)

; Example for circular list from Steele, Common LISP, p. 537:
(setq x (list 'p 'q))
(setq y (list (list 'a 'b) x 'foo x))
(rplacd (last y) (cdr y))

(setf *l* '(1 2))
(setf (car *l*) *l*)
(rplacd (last *l*) *l*)

(defun show-markers ()
  (dolist (a (apropos-list "mark" 'constants))
    (format t "~40@A" a)
    (if (boundp a)
        (format t ", value ~12D = #x~:*~8X" (symbol-value a)))
    (format t "~%")))
(defun show-tags ()
  (dolist (a (apropos-list "poi-tag" 'constants))
    (format t "~40@A" a)
    (if (boundp a)
        (format t ", value ~12D = #x~:*~8X" (symbol-value a)))
    (format t "~%")))
(defun show-bitnums ()
  (dolist (a (apropos-list "bit-num" 'constants))
    (format t "~40@A" a)
    (if (boundp a)
        (format t ", value ~12D = #x~:*~8X" (symbol-value a)))
    (format t "~%")))
(defun show-free-flags ()
  (dolist (a (apropos-list "free-flag" 'constants))
    (format t "~40@A" a)
    (if (boundp a)
        (format t ", value ~12D = #x~:*~8X" (symbol-value a)))
    (format t "~%")))
(defun show-full ()
  (dolist (a (apropos-list "full" 'constants))
    (format t "~40@A" a)
    (if (boundp a)
        (format t ", value ~12D = #x~:*~8X" (symbol-value a)))
    (format t "~%")))
(defun show-masks ()
  (dolist (a (apropos-list "mask" 'constants))
    (format t "~40@A" a)
    (if (boundp a)
        (format t ", value ~12D = #x~:*~8X" (symbol-value a)))
    (format t "~%")))
