;;;
;;; HK 09.11.93: Persistent OBject Store
;;;              ~          ~~     ~

(use-package 'foreign)

(push "/usr/local/lib/gcc-lib/sparc-sun-sunos4.1.3/2.5.2/lib~A.a"
      foreign::*default-library-name-formats*)

;; LISP calls C:
(define-foreign-function (c-pobs-init "_fnPOBSinit")
    ((|lwNil| :as-is)))
(define-foreign-function (c-pobs-store-object "_fnPOBSstoreObject")
    ((|lwObjectToStore| :as-is)) :result-type :as-is)
(define-foreign-function (c-pobs-load-object "_fnPOBSloadObject")
    ((|lwnObjId| :as-is)) :result-type :as-is)

;; C calls LISP:

(foreign-callable cons (:as-is :as-is)
                  :foreign-name "_fnLISPcons" :result-type :as-is :no-check t)
(foreign-callable system::allocate-symbol (:as-is :as-is)
                  :foreign-name "_fnLISPallocateSymbol"
		  :result-type :as-is
		  :no-check t)
(foreign-callable system::fast-alloc-object (:as-is :as-is :as-is)
                  :foreign-name "_fnLISPallocObject"
		  :result-type :as-is
		  :no-check t)
(foreign-callable system::alloc-string (:as-is)
                  :foreign-name "_fnLISPallocString"
		  :result-type :as-is
		  :no-check t)

(defun rd ()
  (read-foreign-modules "lwobject.a" "-lgcc" "-lc")
  (read-foreign-modules "lwobject.o")
  (read-foreign-modules "pobs.o")
  (c-pobs-init nil))
(defun tst (x)
  (let ((result (c-pobs-load-object x)))
    (print (mark-and-sweep 0))
    result))

(define-foreign-function (start-trace "_fnPOBSstartTrace")
    ((arg-1 :as-is)(arg-2 :as-is))
    :result-type :as-is)

(define-foreign-function (trace-alloc-object "_fnPOBStraceAlloc")
    ((size :as-is)(arg-2 :as-is)(arg-3 :as-is))
    :result-type :as-is)

(defun tsu ()
  (start-trace 'system::fast-alloc-object 'trace-alloc-object)
  (make-string 2))
