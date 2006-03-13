;;;; -*-Lisp-*-----------------------------------------------------------------
;;;; Module	plob-dupkeys.lisp
;;;; Author	Heiko Kirschke
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	2005-05-24
;;;; Description	PLOB persistent btree class methods
;;;;			for handling btrees with duplicate keys
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

(defstruct (persistent-btree-with-dup-keys
            (:constructor make-persistent-btree-with-dup-keys (&optional btree)))
  #+:lisp-doc "
\\Purposelabel
 A structure for representing persistent BTrees with duplicate keys.
 This is an example implementation for using duplicate keys on btrees.
\\Seealsolabel
 \\Fcite{make-btree};
 \\fcite{cached-btree}."
  (btree
   (make-btree)
   #+:lisp-doc :documentation #+:lisp-doc "
 The btree used as carrier."))

;; 2005-06-03 hkirschk: Todo for Plob 2.12: Resolve the equivalence in Plob
;; on interpreting integers as objids. Causes more confusion than being
;; efficient. Convert to numeric objids only immediately before diving
;; into the foreign function code. This would also make the :around hack
;; described in the next comment unnecessary.

;; 2005-06-03 hkirschk: Sigh. p-btree is the 3rd argument, so the specialized
;; 2nd argument on string is intercepted by the method in plob-btree. Therefore,
;; I'm abusing an :around method here (yes, I know there is some way of replacing
;; the arguments precedence w.r.t. specialization of a method, but I'm afraid
;; this would cause even more confusion):
(defmethod getbtree-with-data :around
  (key (p-btree persistent-btree-with-dup-keys)
       depth p-heap)
  (let ((mapped ())
        (first-key (store-object (cons key +plob-min-marker+) depth p-heap))
        (last-key (store-object (cons key +plob-max-marker+) depth p-heap))
        (the-btree (persistent-btree-with-dup-keys-btree p-btree)))
    (mapbtree #'(lambda (key data)
                  (push data mapped))
              the-btree :>= first-key :<= last-key)
    (p-unlock-all p-heap first-key)
    (p-destroy first-key)
    (p-unlock-all p-heap last-key)
    (p-destroy last-key)
    mapped))

;; 2005-06-03 hkirschk: On using an :around method here, see my comment
;; above.
(defmethod (setf getbtree-with-data) :around
  (data key (p-btree persistent-btree-with-dup-keys)
        depth p-heap data-objid data-type-tag)
  (let ((stored-key (store-object key depth p-heap))
        (first-key (p-allocate-cons p-heap))
        (last-key (p-allocate-cons p-heap))
        (the-btree (persistent-btree-with-dup-keys-btree p-btree)))
    ;; I'm working directly on persistent objects here, without using
    ;; transient representation:
    (setf (p-car first-key :objid p-heap) (persistent-object-objid stored-key))
    (setf (p-cdr first-key :objid p-heap) +plob-min-marker+)
    (setf (p-car last-key :objid p-heap) (persistent-object-objid stored-key))
    (setf (p-cdr last-key :objid p-heap) +plob-max-marker+)
    (format t "(setf getbtree-with-data) :around~%")
    (let* ((current-key nil)
           (current-number nil)
           ;; use a descending btree mapper to find the maximum element
           ;; (it is the first element returned by a descending mapper):
           (found (mapbtree
                   #'(lambda (key data)
                       (setf current-key (p-car key :object))
                       (setf current-number (p-cdr key))
                       ;; Returning nil tells the mapper to stop the
                       ;; iteration:
                       nil)
                   the-btree
                   :>= (make-persistent-object first-key)
                   :<= (make-persistent-object last-key)
                   :descending t
                   :key-depth :object))
           (next-number (if current-number (1+ current-number) 0)))
      (p-unlock-all p-heap first-key)
      (p-destroy first-key)
      ;; Resuse last-key as new composed key:
      (when current-key
        ;; Alays use an identical key for the compound key:
        (setf (p-car last-key :objid  p-heap) (persistent-object-objid current-key))
        (p-unlock-all p-heap stored-key)
        (p-destroy stored-key))
      (setf (p-cdr last-key) next-number)
      (setf (getbtree-with-data (make-persistent-object last-key)
                                the-btree
                                depth p-heap data-objid data-type-tag)
            data))))

(in-package :cl-user)

(setf *b* (plob::make-persistent-btree-with-dup-keys))
(setf (getbtree "key1" *b*) "value1.0")
(setf (getbtree "key1" *b*) "value1.1")
(setf (getbtree "key1" *b*) "value1.2")
(setf (getbtree "key2" *b*) "value2.0")
(setf (getbtree "key2" *b*) "value2.1")
(setf (getbtree "key2" *b*) "value2.2")
(getbtree "key1" *b*)
;; Take a look at the btree with
(p-apropos-btree (plob::persistent-btree-with-dup-keys-btree *b*))

(mapbtree #'(lambda (k d) (format t "key ~a, data ~a~%" k d) t)
          (plob::persistent-btree-with-dup-keys-btree *b*)
          :filter (store-object (cons (p-make-regex "key2")
                                      +plob-match-any-marker+)))
