;;;; -*- Package: PLOB; Mode: LISP; Syntax: ANSI-Common-Lisp -*----------------
;;;; Module	plob-inspect.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		kirschke@informatik.uni-hamburg.de
;;;; Date	1996/10/15	Created
;;;; Description	PLOB - Persistent Lisp OBjects
;;;;                           =          =    ==
;;;;		Support methods for LispWorks inspector
;;;;
;;;; Copyright	PLOB! Copyright 1994--1998 Heiko Kirschke.
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
;;;; --------------------------------------------------------------------------

(in-package :plob)

;;; ---------------------------------------------------------------------------
#+:lispworks
(defgeneric get-inspector-values-for-p-objid
     (p-objid p-objid-type-tag mode p-heap)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isa{\\funarg{p-objid}}
      {either a numeric immediate value or an \\objid}
 \\isatypetag{\\funarg{p-objid-type-tag}}
\\Valueslabel
 See \\fcite{get-inspector-values}.
\\Purposelabel
 Return the values needed by the \\lw\\ inspector to inspect a persistent
 object."))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod get-inspector-values-for-p-objid
     (p-objid (p-objid-type-tag (eql +btree-type-tag+)) mode p-heap)
  (let ((p-objid-root (sh-btree-root (persistent-object-objid p-heap)
                                     p-objid)))
    (if p-objid-root
        (get-inspector-values-for-p-objid
         p-objid-root +btree-page-tag+ mode p-heap)
      (call-next-method))))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defconstant +inspector-btree-page-slot-name-parent+ "  Parent ->"
  #+:lisp-doc "The slot name to show for a btree page parent slot.")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defconstant +inspector-btree-page-slot-name-next+   "  Next of `~A' ->"
  #+:lisp-doc "The slot name to show for a btree page next slot.")

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod get-inspector-values-for-p-objid
           (p-objid (p-objid-type-tag (eql +btree-page-tag+)) mode p-heap)
  (let ((depth :cached)
        (slot-names (list)) (parent nil) (items (list)))
    #-:lispworks4 ;; and hopefully not later
    (declare (dynamic-extent depth))
    (with-transaction
     (p-heap)
     (sh-btree-page-map
      (persistent-object-objid p-heap)
      p-objid
      #'(lambda (key-value key-type-tag data-value data-type-tag p-objid-next)
          (let ((next (when p-objid-next
                        (p-objid-to-t-object
                         p-objid-next +short-objid-tag+
                         depth p-heap))))
            (if (= key-type-tag +unbound-type-tag+)
                (when next
                  (push (format nil +inspector-btree-page-slot-name-next+
                                "parent")
                        slot-names)
                  (push next items))
              (let ((key (p-objid-to-t-object
                          key-value key-type-tag
                          depth p-heap))
                    (data (p-objid-to-t-object
                           data-value data-type-tag
                           depth p-heap)))
                (push key slot-names)
                (push data items)
                (when next
                  (push (format nil +inspector-btree-page-slot-name-next+
                                key)
                        slot-names)
                  (push next items)))))
          t))
     (setf parent (p-objid-to-t-object (sh-btree-page-parent
                                        (persistent-object-objid p-heap)
                                        p-objid)
                                       +short-objid-tag+
                                       depth
                                       p-heap)))
    (values
     (nconc (list +inspector-btree-page-slot-name-parent+)
            (nreverse slot-names))
     (nconc (list parent) (nreverse items))
     nil
     nil
     'p-btree-page)))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod get-inspector-values-for-p-objid
     (p-objid (p-objid-type-tag (eql +short-objid-tag+)) mode p-heap)
  #+:lisp-doc "Get the `real' type of the persistent object referenced by
 \\funarg{p-objid}\\ and go on with this \\typetag."
  (let ((type-tag (p-type-tag-of p-objid p-heap)))
    (get-inspector-values-for-p-objid p-objid type-tag mode p-heap)))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod get-inspector-values-for-p-objid
     (p-objid (p-objid-type-tag (eql +string-type-tag+)) mode p-heap)
  (let ((t-string (p-objid-to-t-object p-objid p-objid-type-tag
				       :cached p-heap)))
    (values '(0)
            (list t-string)
            nil
            #'(lambda (p-objid slot-name index new-value)
                (declare (ignore slot-name index))
                (setf (p-string p-objid *default-depth* p-heap)
                      new-value))
            'p-string)))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod get-inspector-values-for-p-objid
     (p-objid (p-objid-type-tag (eql +cons-type-tag+)) mode p-heap)
  (values '(car cdr)
          (with-transaction
           (p-heap)
           (list (p-car p-objid :cached p-heap)
                 (p-cdr p-objid :cached p-heap)))
          nil
          #'(lambda (p-objid slot-name index new-value)
              (declare (ignore index))
              (ecase slot-name
                (car
                 (setf (p-car p-objid *default-depth* p-heap) new-value))
                (cdr
                 (setf (p-cdr p-objid *default-depth* p-heap) new-value))))
          'p-cons))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod get-inspector-values-for-p-objid
     (p-objid (p-objid-type-tag (eql +symbol-type-tag+)) mode p-heap)
  (values '(name value function plist package)
          (with-transaction
           (p-heap)
           (list (p-symbol-name p-objid :cached p-heap)
                 (p-symbol-value p-objid :object p-heap)
                 (p-symbol-function p-objid :object p-heap)
                 (p-symbol-plist p-objid :object p-heap)
                 (p-symbol-package p-objid :cached p-heap)))
	  nil
	  #'(lambda (p-objid slot-name index new-value)
	      (declare (ignore index))
              (ecase slot-name
                (name
                 (error "Can't change the name of ~A." p-objid))
                (value
                 (setf (p-symbol-value p-objid *default-depth* p-heap)
                       new-value))
                (function
                 (setf (p-symbol-function p-objid *default-depth* p-heap)
                       new-value))
                (plist
                 (setf (p-symbol-plist p-objid *default-depth* p-heap)
                       new-value))
                (package
                 (error "Can't change the package of ~A." p-objid))))
	  'p-symbol))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod get-inspector-values-for-p-objid
     (p-objid (p-objid-type-tag (eql +vector-type-tag+)) mode p-heap)
  (let ((t-vector (coerce (p-objid-to-t-object p-objid p-objid-type-tag
				               :cached p-heap)
                          'list)))
    (values (loop for i from 0 below (length t-vector)
                  collect i)
            t-vector
            nil
            #'(lambda (p-objid slot-name index new-value)
                (declare (ignore slot-name))
                (setf (p-svref p-objid index *default-depth* p-heap)
                      new-value))
            'p-vector)))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod get-inspector-values-for-p-objid
     (p-objid p-objid-type-tag mode p-heap)
  #+:lisp-doc
  "Return the pure-transient top-level representation for inspection."
  (let* ((depth :cached)
         (t-object
          (p-objid-to-t-object p-objid p-objid-type-tag depth p-heap)))
    (lispworks:get-inspector-values t-object :single)))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod lispworks:get-inspector-values ((object persistent-object)
                                           mode)
  #+:lisp-doc "
 Dummy method to force a load of \\lw's inspector patches. This method
 will be overwritten in \\lisp{plob-inspect.lisp}. The loading of the
 inspector's patches must be done before loading
 \\lisp{plob-inspect}\\ since otherwise the inspector won't remember
 the methods specialized for \\plob."
  (values nil nil nil nil nil))

;;; ---------------------------------------------------------------------------
#+:lispworks
(defmethod lispworks:get-inspector-values ((object persistent-object)
                                           mode)
  (let ((p-heap *default-persistent-heap*))
    (assert-open-session-p p-heap)
    (if (eq mode :single)
        (call-next-method)
      (get-inspector-values-for-p-objid
       (persistent-object-objid object)
       (p-type-tag-of object)
       :cached p-heap))))

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
