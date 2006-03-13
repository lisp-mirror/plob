;;;; -*-Lisp-*-----------------------------------------------------------------
;;;; Module	plob-btree.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	7.2.94
;;;; Description	PLOB persistent btree class methods.
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
(defgeneric clrbtree (p-btree
		      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isabtree{\\funarg{p-btree}}
\\Valueslabel
 \\retarg{\\funarg{p-btree}}
\\Purposelabel
 This generic function removes all entries from
 \\funarg{p-btree}\\ and returns the empty persistent BTree
 \\funarg{p-btree}.
\\Seealsolabel
 \\Fcite{sh-btree-clear};
 \\fcite{clrhash}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf getbtree-with-data) (data key p-btree depth p-heap
                                            data-objid data-type-tag)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 For the \\funarg{data}, \\funarg{key}\\ and
 \\funarg{p-btree} arguments see \\fcite{(setf getbtree)}.
 \\isanobjid{\\funarg{data-objid}}
 \\isatypetag{\\funarg{data-type-tag}}
\\Valueslabel
 See \\fcite{(setf getbtree)}.
\\Purposelabel
 Internal used workhorse for \\fcite{(setf getbtree)}.
 Arguments \\funarg{data-objid}\\ resp.\\ \\funarg{data-type-tag}\\ are
 the \\objid\\ resp.\\ \\typetag\\ of \\funarg{data}\\ already stored to the
 \\sh\\ by the caller of this function.
\\Seealsolabel
 \\Fcite{(setf getbtree)}."))

;;; ---------------------------------------------------------------------------
(defun (setf getbtree) (data key p-btree
			     &optional (depth *default-depth*)
			     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{data}}
 \\isakey{\\funarg{key}}{\\funarg{p-btree}}
 \\isabtree{\\funarg{p-btree}}
\\Valueslabel
 \\retarg{\\funarg{data}}
\\Purposelabel
 Insert \\funarg{data}\\ associated to \\funarg{key}\\ into
 \\funarg{p-btree}; if \\funarg{data}\\ or \\funarg{key}\\ are not
 yet persistent, they are made persistent before inserting them.
 Any old data found under \\funarg{key}\\ is removed before the
 new data is added.

 The function {\\bf (setf getbtree)} stores \\funarg{data}\\ to
 the \\sh\\ and calls the
 \\fcite{(setf getbtree-with-data)}\\ with the \\objid\\ and
 \\typetag\\ obtained at storing \\funarg{data}.
\\Remarkslabel
 Not all \\cl\\ types are supported for \\funarg{key}.

 Since BTrees store their elements ordered (in opposite to hash tables
 which store their elements unordered) when the \\keyarg{test}\\ mode
 is not \\lisp{eq}\\ (see \\fcite{make-btree}, argument \\keyarg{test}),
 it must be possible to impose an
 ordering relation on a new to-insert BTree key resp.\\ to the keys
 already contained in the BTree. If \\plob\\ cannot impose such an ordering
 relation an error is raised when trying to insert an incompatible key,
 e.g.\\ if there are stored only fixnum objects in a BTree,
 a key of type string cannot be inserted because there is no
 ordering relation defined between types fixnum and string.

 For BTrees with \\keyarg{test}\\ mode \\lisp{eq}, numeric comparision
 of the objects \\objid[s]\\ is used for the ordering relation;
 so BTrees with \\keyarg{test}\\ mode \\lisp{eq}\\ can handle all
 key types.

 Currently, these ordering relations are hand-knitted into the
 \\plob\\ C code for efficency reasons;
 there is no possibility to define additional
 ordering relations from the \\cl\\ side.
\\Seealsolabel
 \\Fcite{(setf getbtree-with-data)}; \\fcite{make-btree};
 \\fcite{(setf gethash)}."

  (with-transaction (p-heap)
   (multiple-value-bind (data-objid data-type-tag)
       (t-object-to-p-objid data depth p-heap)
     (setf (getbtree-with-data key p-btree depth
			       p-heap
			       data-objid
			       (if data-type-tag
				   data-type-tag
				 +short-objid-tag+))
           data)))
  data)

;;; ---------------------------------------------------------------------------
(defgeneric (setf getbtree-by-objid-with-data) (data key key-objid key-type-tag
						     p-btree depth p-heap
                                                     data-objid data-type-tag)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 For the \\funarg{data}, \\funarg{key}\\ and \\funarg{p-btree}\\ arguments
 see \\fcite{(setf getbtree-by-objid)}.
 \\isanobjid{\\funarg{data-objid}}
 \\isatypetag{\\funarg{data-type-tag}}
\\Valueslabel
 See \\fcite{(setf getbtree-by-objid)}.
\\Purposelabel
 Internal used workhorse for \\fcite{(setf getbtree-by-objid)}.
 Arguments \\funarg{data-objid}\\ resp.\\ \\funarg{data-type-tag}\\ are
 the \\objid\\ resp.\\ \\typetag\\ of \\funarg{data}\\ already stored to the
 \\sh\\ by the caller of this function.
\\Seealsolabel
 \\Fcite{(setf getbtree-by-objid)}."))

;;; ---------------------------------------------------------------------------
(defun (setf getbtree-by-objid) (data key p-btree
				      &optional (depth *default-depth*)
				      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 For the \\funarg{data}\\ and \\funarg{p-btree}\\ arguments see
 \\fcite{(setf getbtree)}.
 \\isanobjid{\\funarg{key}}
\\Valueslabel
 \\retarg{\\funarg{data}}
\\Purposelabel
 At first see \\fcite{(setf getbtree)}; this function differs
 from \\fcite{(setf getbtree)} in the sense that the
 \\funarg{key}\\ argument has to be a persistent object's \\objid;
 no transient \\cl\\ \\obj\\ may be passed directly as
 \\funarg{key}\\ argument.

 The function {\\bf (setf getbtree-by-objid)} stores \\funarg{data}\\ to
 the \\sh\\ and calls the
 \\fcite{(setf getbtree-by-objid-with-data)}\\ with the \\objid\\ and
 \\typetag\\ obtained at storing \\funarg{data}.
\\Seealsolabel
 \\Fcite{(setf getbtree-by-objid-with-data)}."

  (declare (inline (setf getbtree-by-objid)))
  (with-transaction (p-heap)
    (multiple-value-bind (data-objid data-type-tag)
	(t-object-to-p-objid data depth p-heap)
      (setf (getbtree-by-objid-with-data key
					 (persistent-object-objid key)
					 +short-objid-tag+
					 p-btree depth
					 (persistent-object-objid p-heap)
					 data-objid
					 (if data-type-tag
					     data-type-tag
					   +short-objid-tag+))
	data)))
  data)

;;; ---------------------------------------------------------------------------
(defgeneric getbtree-with-data (key p-btree depth p-heap)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 See \\fcite{getbtree}.
\\Valueslabel
 Four values are returned:
 \\begin{enumerate}
 \\item The transient representation of the persistent object
  found under \\funarg{key}\\ in the persistent BTree
  \\funarg{p-btree}.
 \\item A flag indicating if the \\funarg{key}\\ was found in the
  persistent BTree \\funarg{p-btree}, i.e.\\ if the
  first value is valid at all.
 \\item If an \\obj\\ was found, the \\objid\\ of the first
   returned value, i.e.\\ the \\objid\\ of the persistent object
   found under \\funarg{key}\\ in the persistent BTree
   \\funarg{p-btree}\\ is returned as 3rd value;
   \\lispnil\\ otherwise.
 \\item If an \\obj\\ was found, the \\typetag\\ of the first
   returned value, i.e.\\ the \\typetag\\ of the persistent object
   found under \\funarg{key}\\ in the persistent BTree
   \\funarg{p-btree}\\ is returned as 4th value;
   \\lispnil\\ otherwise.
 \\end{enumerate}
\\Purposelabel
 Internal used workhorse for \\fcite{getbtree}.
\\Seealsolabel
 \\Fcite{getbtree}."))

;;; ---------------------------------------------------------------------------
(defun getbtree (key p-btree
		     &optional (depth *default-depth*)
		     (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isakey{\\funarg{key}}{\\funarg{p-btree}}
 \\isabtree{\\funarg{p-btree}}
\\Valueslabel
 Two values are returned:
 \\begin{enumerate}
 \\item The transient representation of the persistent object
  found under \\funarg{key}\\ in \\funarg{p-btree}.
 \\item A flag indicating if the \\funarg{key}\\ was found in
  \\funarg{p-btree}.
 \\end{enumerate}
\\Purposelabel
 Search \\funarg{key}\\ in \\funarg{p-btree}; if \\funarg{key}\\ is
 found, the transient representation of the found \\obj\\ is returned.

 The kind of search performed depends on the \\keyarg{test}\\ mode
 of the persistent BTree; see \\fcite{make-btree}, argument
 \\keyarg{test}\\ for details.

 The function {\\bf getbtree} calls \\fcite{getbtree-with-data}.
\\Seealsolabel
 \\Fcite{getbtree-with-data}; \\fcite{make-btree};
 \\fcite{gethash}."

  (with-transaction (p-heap)
    (getbtree-with-data key p-btree depth p-heap)))

;;; ---------------------------------------------------------------------------
(defgeneric getbtree-by-objid-with-data (key key-objid key-type-tag
					 p-btree depth p-heap)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 See \\fcite{getbtree-by-objid}.
\\Valueslabel
 See \\fcite{getbtree-with-data}.
\\Purposelabel
 Internal used workhorse for \\fcite{getbtree-by-objid}.
\\Seealsolabel
 \\Fcite{getbtree-by-objid}."))

;;; ---------------------------------------------------------------------------
(defun getbtree-by-objid (key p-btree
		              &optional (depth *default-depth*)
		              (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanobjid{\\funarg{key}}
 For the \\funarg{p-btree} argument see \\fcite{getbtree}.
\\Valueslabel
 See \\fcite{getbtree}.
\\Purposelabel
 At first see \\fcite{getbtree}; this function differs from
 \\fcite{getbtree}\\ in the sense that the \\funarg{key}\\ argument
 has to be a persistent object's \\objid; no transient \\cl\\ object
 may be passed directly as \\funarg{key}\\ argument.

 The function {\\bf getbtree-by-objid} calls
 \\fcite{getbtree-by-objid-with-data}.
\\Seealsolabel
 \\Fcite{getbtree-by-objid-with-data}."

  (declare (inline getbtree-by-objid))
  (with-transaction (p-heap)
    (getbtree-by-objid-with-data key (persistent-object-objid key)
				 +short-objid-tag+
				 p-btree depth p-heap)))

;;; ---------------------------------------------------------------------------
(defgeneric rembtree (key p-btree
			  &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isakey{\\funarg{key}}{\\funarg{p-btree}}
 \\isabtree{\\funarg{p-btree}}
\\Valueslabel
 Returns \\nonnil\\ if the entry with key
 \\funarg{key}\\ was found in and deleted from the persistent BTree
 \\funarg{p-btree}, \\lispnil\\ if the entry was not found at all.
\\Purposelabel
 This generic function removes an entry with key \\funarg{key}\\ from
 the persistent BTree \\funarg{p-btree}.
\\Seealsolabel
 \\Fcite{remhash}."))

;;; ---------------------------------------------------------------------------
(defgeneric rembtree-by-objid
  (key-objid key-type-tag p-btree &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isanobjid{\\funarg{key}}
 \\isabtree{\\funarg{p-btree}}
\\Valueslabel
 See \\fcite{rembtree}.
\\Purposelabel
 At first see \\fcite{rembtree}; this function differs from
 \\fcite{rembtree}\\ in the sense that the \\funarg{key}\\ argument
 has to be a persistent object's \\objid; no transient
 \\cl\\ \\obj\\ may be passed directly as \\funarg{key}\\ argument.
\\Seealsolabel
 \\Fcite{rembtree}."))

;;; ---------------------------------------------------------------------------
(defun btree-cached-p (p-btree &optional (depth *default-depth*)
			       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabtree{\\funarg{p-btree}}
\\Purposelabel
 Returns \\nonnil\\ if \\funarg{p-btree}\\ is a
 cached persistent BTree, \\lispnil\\ otherwise.
\\Seealsolabel
 \\Fcite{(setf btree-cached-p)};
 argument \\keyarg{cached}\\ of \\fcite{make-btree}."

  (p-objid-to-t-slot p-btree +btree-location-cached+ depth p-heap nil
		     +btree-type-tag+))

;;; ---------------------------------------------------------------------------
(defun (setf btree-cached-p)
    (t-cached p-btree &optional (depth *default-depth*)
			      (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabool{\\funarg{t-cached}}
 \\isabtree{\\funarg{p-btree}}
\\Purposelabel
 Make \\funarg{p-btree}\\ a cached persistent BTree for
 \\funarg{t-cached}\\ \\nonnil\\, an uncached BTree otherwise.
 The cache becomes active resp.\\ de-active next time when
 \\funarg{p-btree}\\ is loaded from the \\sh.
\\Seealsolabel
 \\Fcite{btree-cached-p};
 argument \\keyarg{t-cached}\\ of \\fcite{make-btree}."

  (t-slot-to-p-objid t-cached depth p-heap p-btree
		     +btree-location-cached+ nil +btree-type-tag+))

;;; ---------------------------------------------------------------------------
(defun btree-count (p-btree &optional (p-heap
				       *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabtree{\\funarg{p-btree}}
\\Purposelabel
 Returns the current number of elements in \\funarg{p-btree}.
\\Seealsolabel
 \\Fcite{hash-table-count}."

  (c-sh-btree-count (persistent-object-objid p-heap)
                    (persistent-object-objid p-btree)))

;;; ---------------------------------------------------------------------------
(defun btree-size (p-btree &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabtree{\\funarg{p-btree}}
\\Purposelabel
 Returns the current size of \\funarg{p-btree},
 i.e.\\ the maximum number of elements
 \\funarg{p-btree}\\ can hold before a re-size will occure.
\\Seealsolabel
 \\Fcite{hash-table-size}."

  (c-sh-btree-size (persistent-object-objid p-heap)
		   (persistent-object-objid p-btree)))

;;; ---------------------------------------------------------------------------
(defgeneric btree-test (p-btree	&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation   "
\\Argumentslabel
 \\isabtree{\\funarg{p-btree}}
\\Purposelabel
 Returns the current test mode for the persistent BTree \\funarg{p-btree}.
\\Seealsolabel
 \\Fcite{(setf btree-test)};
 argument \\keyarg{test}\\ of \\fcite{make-btree}."))

;;; ---------------------------------------------------------------------------
(defgeneric (setf btree-test) (new-test-mode
                               p-btree
			       &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation   "
\\Argumentslabel
 \\isa{\\funarg{new-test-mode}}
      {one of the symbols \\lisp{eq}, \\lisp{eql}\\ or \\lisp{equal}}
 \\isabtree{\\funarg{p-btree}}
\\Purposelabel
 Sets the test mode of the persistent BTree \\funarg{p-btree}\\ to
 \\funarg{new-test-mode}. The test mode can only be chaged for empty
 BTrees; for non-empty BTrees the test mode is not changed.
\\Seealsolabel
 \\Fcite{btree-test};
 argument \\keyarg{test}\\ of \\fcite{make-btree}."))

;;; ---------------------------------------------------------------------------
(defconstant +btree-test-mode->symbol+
  `((,+eq+	. ,#'eq )
    (,+eq+	. eq )
    (,+eql+	. ,#'eql)
    (,+eql+	. eql)
    (,+equal+	. ,#'equal)
    (,+equal+	. equal))
  #+:lisp-doc "
\\Purposelabel
 Internal constant.
 Mapping between symbols \\lisp{eq}, \\lisp{eql}, \\lisp{equal}\\ and their
 \\plob\\ C level numeric BTree test-mode companions {\\bf +eq+},
 {\\bf +eql+}, {\\bf +equal+}.
\\Seealsolabel
  \\Fcite{btree-test}.")

;;; ---------------------------------------------------------------------------
(defmethod btree-test (p-btree &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Does a low-level read of the current test mode."
  (cdr (assoc (sh-btree-test-mode (persistent-object-objid p-heap)
                                  (persistent-object-objid p-btree)
                                  +btree-get-test-mode+)
              +btree-test-mode->symbol+)))

;;; ---------------------------------------------------------------------------
(defmethod (setf btree-test) (new-test-mode
                              p-btree
			      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Does a low-level write of the current test mode."
  (let ((numeric-test-mode (car (rassoc new-test-mode
                                        +btree-test-mode->symbol+))))
    (unless numeric-test-mode
      (setf numeric-test-mode new-test-mode))
    (sh-btree-test-mode (persistent-object-objid p-heap)
                        (persistent-object-objid p-btree)
                        numeric-test-mode)
    new-test-mode))

;;; ---------------------------------------------------------------------------
(defmethod (setf btree-test) :before
  (new-test-mode
   (p-btree cached-btree)
   &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
Changes the test mode of the cache of the cached persistent BTree
\\funarg{p-btree} too."

  (declare (ignore p-heap))
  (let* ((old-test-mode (btree-test p-btree))
         (new-cache (unless (eq old-test-mode new-test-mode)
                      (make-hash-table :test new-test-mode))))
    (when new-cache
      (setf (cached-btree-key->data-cache p-btree) new-cache))))

;;; ---------------------------------------------------------------------------
(defun p-allocate-btree (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new initially empty persistent BTree
 allocated in the \\sh.
\\Seealsolabel
 \\Fcite{make-btree};
 \\fcite{p-allocate}."
  (p-allocate p-heap +btree-type-tag+))

;;; ---------------------------------------------------------------------------
(defun make-btree (&key (cached nil)
                        (objid nil)
		        (test 'equal)
			pagesize
                        (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 \\Argumentslabel
  \\isabool{\\keyarg{cached}}
  \\isanobjid{\\keyarg{objid}}
  \\isa{\\keyarg{test}}
       {one of the symbols \\lisp{eq}, \\lisp{eql}\\ or \\lisp{equal}}
 \\Valueslabel
  A structure representing the persistent BTree is returned.
 \\Purposelabel
  This function creates resp.\\ gives access to an already existing
  cached or uncached persistent BTree.

  The \\keyarg{cached}\\ argument determines if the persistent BTree
  should have a transient cache.
  If its value is \\lispnil, all operations on the returned BTree
  are done exclusively by the low-level C functions, i.e.\\ each
  BTree access involves calls to the \\plob\\ C code.
  If its value is \\nonnil\\, all entries read from or written to
  the persistent BTree are cached. This speeds up access to BTree entries
  a lot since many calls to the low-level BTree access functions can
  be avoided\\footnote{Not the \\plob\\ low-level access functions are
  slow but the foreign-language calls itself from \\lw\\ to C.};
  furthermore, reading from the cache does not need an active
  transaction. The cache is organized as a write-through cache.

  If the \\keyarg{objid}\\ argument is passed, its value must be an
  \\objid\\ referencing an already allocated persistent BTree; this
  \\objid\\ is used as the \\objid\\ of the return value,
  i.e.\\ no new persistent BTree is allocated on the \\sh.
  If no \\keyarg{objid}\\ is passed, a new persistent BTree is allocated
  on the \\sh.

  The \\keyarg{test}\\ argument determines the kind of ordering which
  is to be imposed on the keys contained in the BTree. A value of
  \\lisp{eq}\\ or \\lisp{eql}\\ orders the keys in the BTree by their
  \\objid[s], i.e.\\ the ordering is done `by persistent
  {\\sl identity}' of the key objects and not by the {\\sl state}
  of the key objects. Since all \\cl\\ objects regardless
  of their type can be handled by \\plob, a BTree with \\keyarg{test}-mode
  \\lisp{eq}\\ or \\lisp{eql}\\ (actually, \\lisp{eq}\\ and
  \\lisp{eql}\\ mean the same) can contain any key objects independent of
  their type, but searching a key can be done only by the key's
  \\objid\\ and not by the key's state. In opposite, a
  \\keyarg{test}-mode of \\lisp{equal}\\ orders the keys in the BTree
  by their state and makes it possible to search for values, but
  the low-level \\plob\\ BTree functions cannot handle all
  \\cl\\ types; a rule of thumb is that \\plob\\ can handle as keys
  for \\lisp{equal}\\ BTrees all built-in
  numeric types consisting of only one component and all other built-in
  types and all structures which contain only these built-in types
  (i.e.\\ \\plob\\ can handle fixnums, short-floats, single-floats,
  strings and structures made up of these types, like conses or
  vectors containing only fixnums, short-floats, \\ldots;
  \\plob\\ cannot handle as keys for \\lisp{equal}\\ BTrees
  bignums, double-floats, complex\\footnote{Complex numbers would
  also require a two-dimensional indexing for searching them
  by their values; this cannot be accomplished principally by
  BTrees, since they handle one-dimensional keys.}\\ and rational
  numbers).
 \\Exampleslabel
  Create a persistent BTree (line~1), insert (line~2-4) and search
  (line~5-6) some elements. Please note that objects of type
  \\class{fixnum}\\ and \\class{short-float}\\ can be handled by a
  single \\lisp{equal}\\ BTree:
  \\begin{lispcode}
 (setf *b* (make-btree :test 'equal))
         ==> #<btree equal 0/0 short-objid=8377610>

 (setf (getbtree 1 *b*) \"Value of key 1\")
 (setf (getbtree 2.0s0 *b*) \"Value of key 2.0s0\")
 (setf (getbtree \"error\" *b*) 1) ==> raises an error

 (getbtree 1 *b*) ==> \"Value of key 1\"
 (getbtree 2 *b*) ==> \"Value of key 2.0s0\"
  \\end{lispcode}
 \\Seealsolabel
  \\Fcite{clrbtree};
  \\fcite{getbtree};
  \\fcite{getbtree-by-objid};
  \\fcite{(setf getbtree)};
  \\fcite{(setf getbtree-by-objid)};
  \\fcite{mapbtree};
  \\fcite{rembtree};
  \\fcite{rembtree-by-objid};
  \\fcite{p-apropos-btree};
  \\fcite{make-hash-table}."

  (let ((p-objid-btree (if objid
			   objid
			 (progn
			   (assert-sh-open-p)
			   (p-allocate-btree p-heap)))))
    (if objid
	(progn
	  (setf test (btree-test p-objid-btree p-heap))
	  (setf cached (btree-cached-p p-objid-btree :cached p-heap)))
      (progn
	(setf (btree-test p-objid-btree p-heap) test)
	(setf (btree-cached-p p-objid-btree :cached p-heap) cached)
	(when pagesize
	  (setf (btree-pagesize p-objid-btree p-heap) pagesize))))
    (if cached
	(let ((cache (make-hash-table :test test))
	      (btree (make-cached-btree p-objid-btree)))
	  (setf (cached-btree-key->data-cache btree) cache)
	  (register-to-cache p-objid-btree btree)
	  btree)
      (make-persistent-btree p-objid-btree))))

;;; ---------------------------------------------------------------------------
(defmethod clrbtree (p-btree
		     &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Calls the low-level function \\fcite{sh-btree-clear}."
  (with-transaction (p-heap)
    (sh-btree-clear (persistent-object-objid p-heap)
		    (persistent-object-objid p-btree))))

;;; ---------------------------------------------------------------------------
(defmethod clrbtree :before ((p-btree cached-btree)
		             &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 Clears the cache of the cached persistent BTree \\funarg{p-btree}\\ too."
  (declare (ignore p-heap))
  (clrhash (cached-btree-key->data-cache p-btree)))

;;; ---------------------------------------------------------------------------
(defmethod rembtree ((key integer)
                     p-btree
		     &optional (p-heap *default-persistent-heap*))
  (with-transaction (p-heap)
    (sh-btree-delete (persistent-object-objid p-heap)
		     (persistent-object-objid p-btree)
		     key +fixnum-type-tag+)))

;;; ---------------------------------------------------------------------------
(defmethod rembtree ((key float)
                     p-btree
		     &optional (p-heap *default-persistent-heap*))
  (typecase key
    (short-float
     (with-transaction (p-heap)
       (if +has-short-float-p+
	   (sh-btree-delete (persistent-object-objid p-heap)
			    (persistent-object-objid p-btree)
			    (short-float-to-fixnum key)
			    +short-float-type-tag+)
	 (sh-btree-delete-by-float (persistent-object-objid p-heap)
				   (persistent-object-objid p-btree)
				   key))))
    (single-float
     (with-transaction (p-heap)
       (sh-btree-delete-by-float (persistent-object-objid p-heap)
				 (persistent-object-objid p-btree)
				 key)))
    (double-float
     (with-transaction (p-heap)
       (sh-btree-delete-by-double (persistent-object-objid p-heap)
				  (persistent-object-objid p-btree)
				  key)))
    (t
     (call-next-method))))

;;; ---------------------------------------------------------------------------
(defmethod rembtree ((key character)
                     p-btree
		     &optional (p-heap *default-persistent-heap*))
  (with-transaction (p-heap)
    (sh-btree-delete (persistent-object-objid p-heap)
		     (persistent-object-objid p-btree)
		     (char-code key)
		     +character-type-tag+)))

;;; ---------------------------------------------------------------------------
(defmethod rembtree ((key string)
                     p-btree
		     &optional (p-heap *default-persistent-heap*))
  (with-transaction (p-heap)
    (sh-btree-delete-by-string (persistent-object-objid p-heap)
			       (persistent-object-objid p-btree)
			       key)))

;;; ---------------------------------------------------------------------------
(defmethod rembtree ((key symbol)
                     p-btree
		     &optional (p-heap *default-persistent-heap*))
  (with-transaction (p-heap)
    (sh-btree-delete-by-string (persistent-object-objid p-heap)
			       (persistent-object-objid p-btree)
			       (symbol-name key))))

;;; ---------------------------------------------------------------------------
(defmethod rembtree ((key persistent-object)
                     p-btree
		     &optional (p-heap *default-persistent-heap*))
  (rembtree-by-objid (persistent-object-internal-objid key)
		     +short-objid-tag+ p-btree p-heap))

;;; ---------------------------------------------------------------------------
(defmethod rembtree ((key persistent-immediate-object)
                     p-btree
		     &optional (p-heap *default-persistent-heap*))
  (rembtree-by-objid (persistent-immediate-object-objid key)
		     (persistent-immediate-object-type-tag key)
		     p-btree p-heap))

;;; ---------------------------------------------------------------------------
(defmethod rembtree ((key persistent-clos-object)
                     p-btree
		     &optional (p-heap *default-persistent-heap*))
  (rembtree-by-objid (persistent-object-objid key)
		     +short-objid-tag+ p-btree p-heap))

;;; ---------------------------------------------------------------------------
(defmethod rembtree (key p-btree
		     &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "Store \\funarg{key}\\ and continue."
  (multiple-value-bind (key-objid key-type-tag)
      (t-object-to-p-objid key :cached p-heap)
    (rembtree-by-objid key-objid key-type-tag p-btree p-heap)))

;;; ---------------------------------------------------------------------------
(defmethod rembtree-by-objid (key-objid key-type-tag
                              p-btree
		              &optional (p-heap *default-persistent-heap*))
  (with-transaction (p-heap)
    (sh-btree-delete (persistent-object-objid p-heap)
		     (persistent-object-objid p-btree)
		     key-objid key-type-tag)))

;;; ---------------------------------------------------------------------------
(defmethod rembtree :around (key
		             (p-btree cached-btree)
		             &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 Removes the element found under \\funarg{key}\\ from the cache of the
 cached persistent BTree \\funarg{p-btree}\\ too."
  (let ((deleted (call-next-method)))
    (when (and deleted (not (integerp key)))
      (remhash key (cached-btree-key->data-cache p-btree)))
    deleted))

;;; ---------------------------------------------------------------------------
(defmethod rembtree :around ((key persistent-object)
		             (p-btree cached-btree)
		             &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 Calls to this method are trapped to \\fcite{rembtree-by-objid}."
  (rembtree-by-objid (persistent-object-internal-objid key)
		     +short-objid-tag+ p-btree p-heap))

;;; ---------------------------------------------------------------------------
(defmethod rembtree :around ((key persistent-immediate-object)
		             (p-btree cached-btree)
		             &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 Calls to this method are trapped to \\fcite{rembtree-by-objid}."
  (rembtree-by-objid (persistent-object-internal-objid key)
		     (persistent-immediate-object-type-tag key)
		     p-btree p-heap))

;;; ---------------------------------------------------------------------------
(defmethod rembtree :around ((key persistent-clos-object)
		             (p-btree cached-btree)
		             &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 Calls to this method are trapped to \\fcite{rembtree-by-objid}."
  (rembtree-by-objid (persistent-object-objid key) +short-objid-tag+
		     p-btree p-heap))

;;; ---------------------------------------------------------------------------
(defmethod rembtree-by-objid (key-objid key-type-tag
		              (p-btree cached-btree)
		              &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
 Removes the element found under \\funarg{key}\\ from the cache of the
 cached persistent BTree \\funarg{p-btree} too."
  (let ((deleted (call-next-method)))
    (when (and deleted (not (p-immediatep key-type-tag)))
      (remhash key-objid (cached-btree-key->data-cache p-btree)))
    deleted))

;;; ---------------------------------------------------------------------------
(defstruct (btree-cache-entry
            (:constructor make-btree-cache-entry (objid type-tag
                                                        &optional
                                                        object datap data)))
  #+:lisp-doc "
\\Purposelabel
 A structure used for cached BTree cache entries.
\\Seealsolabel
 \\Fcite{make-btree}, argument \\keyarg{cached};
 \\fcite{cached-btree}."

  (objid nil
         #+:lisp-doc :documentation #+:lisp-doc "
 The \\objid\\ of the cached data object.")

  (type-tag nil
            #+:lisp-doc :documentation #+:lisp-doc "
 The \\typetag\\ of the cached data object.")

  (object nil
          #+:lisp-doc :documentation #+:lisp-doc "
 The instance of \\fcite{persistent-object}\\ containing the \\objid.")

  (datap nil
         :type symbol
         #+:lisp-doc :documentation #+:lisp-doc "
 A flag if {\\bf data} is a valid cached entry at all.")

  (data nil
        #+:lisp-doc :documentation #+:lisp-doc "
 The cached \\cl\\ data object."))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) (data (key integer)
                                           p-btree depth p-heap
                                           data-objid data-type-tag)
  (declare (ignore data depth))
  (sh-btree-insert (persistent-object-objid p-heap)
                   (persistent-object-objid p-btree)
		   key +fixnum-type-tag+
		   data-objid data-type-tag))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) (data (key float)
                                           p-btree depth p-heap
                                           data-objid data-type-tag)
  (declare (ignore data depth))
  (typecase key
    (short-float
     (if +has-short-float-p+
	 (sh-btree-insert (persistent-object-objid p-heap)
			  (persistent-object-objid p-btree)
			  (short-float-to-fixnum key)
			  +short-float-type-tag+
			  data-objid data-type-tag)
       (sh-btree-insert-by-float (persistent-object-objid p-heap)
				 (persistent-object-objid p-btree)
				 key data-objid data-type-tag)))
    (single-float
     (sh-btree-insert-by-float (persistent-object-objid p-heap)
			       (persistent-object-objid p-btree)
			       key data-objid data-type-tag))
    (double-float
     (sh-btree-insert-by-double (persistent-object-objid p-heap)
			        (persistent-object-objid p-btree)
				key data-objid data-type-tag))
    (t
     (call-next-method))))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) (data (key character)
                                           p-btree depth p-heap
                                           data-objid data-type-tag)
  (declare (ignore data depth))
  (sh-btree-insert (persistent-object-objid p-heap)
                   (persistent-object-objid p-btree)
		   (char-code key) +character-type-tag+
		   data-objid data-type-tag))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) (data (key string)
                                           p-btree depth p-heap
                                           data-objid data-type-tag)
  (declare (ignore data depth))
  (sh-btree-insert-by-string (persistent-object-objid p-heap)
                             (persistent-object-objid p-btree)
			     key
			     data-objid data-type-tag))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) (data (key symbol)
                                           p-btree depth p-heap
                                           data-objid data-type-tag)
  (declare (ignore data depth))
  (sh-btree-insert-by-string (persistent-object-objid p-heap)
                             (persistent-object-objid p-btree)
			     (symbol-name key) 
			     data-objid data-type-tag))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) (data (key persistent-object)
                                           p-btree depth p-heap
                                           data-objid data-type-tag)
  #+:lisp-doc "Calls to this method are trapped to
 \\fcite{(setf getbtree-by-objid-with-data)}."
  (let ((key-objid (persistent-object-objid key)))
    (setf (getbtree-by-objid-with-data key key-objid +short-objid-tag+
				       p-btree depth p-heap
				       data-objid data-type-tag)
        data)))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) (data (key persistent-immediate-object)
                                           p-btree depth p-heap
                                           data-objid data-type-tag)
  #+:lisp-doc "Calls to this method are trapped to
 \\fcite{(setf getbtree-by-objid-with-data)}."
  (let ((key-objid (persistent-object-objid key)))
    (setf (getbtree-by-objid-with-data key key-objid (p-type-tag-of key)
				       p-btree depth p-heap
				       data-objid data-type-tag)
      data)))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) (data (key persistent-clos-object)
                                           p-btree depth p-heap
                                           data-objid data-type-tag)
  #+:lisp-doc "Calls to this method are trapped to
 \\fcite{(setf getbtree-by-objid-with-data)}."
  (let ((key-objid (persistent-object-objid key)))
    (setf (getbtree-by-objid-with-data key key-objid +short-objid-tag+
				       p-btree depth p-heap
				       data-objid data-type-tag)
      data)))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) (data key p-btree depth p-heap
                                           data-objid data-type-tag)
  #+:lisp-doc "Store \\funarg{key}\\ and continue."
  (multiple-value-bind (key-objid key-type-tag)
      (t-object-to-p-objid key depth p-heap)
    (unless (p-immediatep key-type-tag)
      (setf key-type-tag +short-objid-tag+))
    (setf (getbtree-by-objid-with-data key key-objid key-type-tag
				       p-btree depth p-heap
				       data-objid data-type-tag)
	  data)))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-by-objid-with-data) (data key key-objid key-type-tag
                                                    p-btree depth p-heap
                                                    data-objid data-type-tag)
  (declare (ignore key data depth))
  (sh-btree-insert (persistent-object-objid p-heap)
		   (persistent-object-objid p-btree)
		   key-objid key-type-tag
		   data-objid data-type-tag))

;;; ---------------------------------------------------------------------------
(defun insert-into-btree-cache (data entry-key p-btree depth
                                     data-objid data-type-tag)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{data}}
 \\isanobject{\\funarg{entry-key}}
 \\isabtree{\\funarg{p-btree}}
 \\isanobjid{\\funarg{data-objid}}
 \\isatypetag{\\funarg{data-type-tag}}
\\Purposelabel
 Internal used function.
 Insert \\funarg{data}, \\funarg{data-objid},
 \\funarg{data-type-tag}\\ associated to \\funarg{entry-key}\\ into the
 cache of \\funarg{p-btree}."

  (let ((entry (make-btree-cache-entry data-objid data-type-tag)))
    (case depth
      (:objid
       nil)
      (:object
       (setf (btree-cache-entry-object entry) data))
      (t
       (setf (btree-cache-entry-datap entry) t)
       (setf (btree-cache-entry-data entry) data)))
    (setf (gethash entry-key (cached-btree-key->data-cache p-btree))
	  entry)))

;;; ---------------------------------------------------------------------------
(defun (setf getbtree-into-cached-btree) (data key entry-key
					       key-objid key-type-tag
                                               p-btree depth p-heap
				               data-objid data-type-tag
				               next-setf-method)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{data}}
 \\isanobject{\\funarg{key}}
 \\isanobject{\\funarg{entry-key}}
 \\isacbtree{\\funarg{p-btree}}
 \\isanobjid{\\funarg{data-objid}}
 \\isatypetag{\\funarg{data-type-tag}}
 \\isa{\\funarg{next-setf-method}}
      {a method function with arguments as accepted by the
       \\fcite{(setf getbtree-with-data)}}
\\Valueslabel
 Returns \\nonnil\\ if \\funarg{key}\\ was inserted in \\funarg{p-btree},
 \\lispnil\\ otherwise.
\\Purposelabel
 Internal used function.
 Calls \\funarg{next-setf-method}\\ to insert \\funarg{data}\\ associated
 to \\funarg{key}\\ into \\funarg{p-btree}\\ and 
 inserts afterwards \\funarg{data}, \\funarg{data-objid},
 \\funarg{data-type-tag}\\ associated to \\funarg{entry-key}\\ into the
 cache of \\funarg{p-btree}."

  (let ((inserted (funcall next-setf-method data key key-objid key-type-tag
			   p-btree depth p-heap data-objid data-type-tag)))
    (insert-into-btree-cache data entry-key p-btree
                             depth data-objid data-type-tag)
    inserted))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) :around (data key
                                                   (p-btree cached-btree)
                                                   depth p-heap
				                   data-objid data-type-tag)
  #+:lisp-doc "Inserts \\funarg{data}, \\funarg{data-objid},
 \\funarg{data-type-tag}\\ associated to \\funarg{key} into the cache of
 the cached persistent BTree \\funarg{p-btree}."
  (if (integerp key)
      (call-next-method)
  (let ((inserted (call-next-method data key p-btree depth p-heap data-objid data-type-tag)))
    (insert-into-btree-cache data key p-btree
                             depth data-objid data-type-tag)
    inserted)))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) :around (data (key persistent-object)
                                                   (p-btree cached-btree)
                                                   depth p-heap
				                   data-objid data-type-tag)
  #+:lisp-doc "Inserts \\funarg{data}, \\funarg{data-objid},
 \\funarg{data-type-tag}\\ associated to \\funarg{key} into the cache of
 the cached persistent BTree \\funarg{p-btree}."
  (let ((key-objid (persistent-object-objid key)))
    (setf (getbtree-into-cached-btree key key-objid
				      key-objid (p-type-tag-of key)
				      p-btree depth p-heap
				      data-objid data-type-tag
				      #'(setf getbtree-by-objid-with-data))
	  data)))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-with-data) :around (data (key persistent-clos-object)
                                                   (p-btree cached-btree)
                                                   depth p-heap
				                   data-objid data-type-tag)
  #+:lisp-doc "Inserts \\funarg{data}, \\funarg{data-objid},
 \\funarg{data-type-tag}\\ associated to \\funarg{key} into the cache of
 the cached persistent BTree \\funarg{p-btree}."
  (let ((key-objid (persistent-object-objid key)))
    (setf (getbtree-into-cached-btree key key-objid
				      key-objid +short-objid-tag+
				      p-btree depth p-heap
				      data-objid data-type-tag
				      #'(setf getbtree-by-objid-with-data))
	data)))

;;; ---------------------------------------------------------------------------
(defmethod (setf getbtree-by-objid-with-data) (data key key-objid key-type-tag
	                                            (p-btree cached-btree)
	                                            depth p-heap
	                                            data-objid data-type-tag)
  #+:lisp-doc "Inserts \\funarg{data}, \\funarg{data-objid},
 \\funarg{data-type-tag}\\ associated to \\funarg{key} into the cache of
 the cached persistent BTree \\funarg{p-btree}."
  (setf (getbtree-into-cached-btree key key-objid
				    key-objid key-type-tag
				    p-btree depth p-heap
				    data-objid data-type-tag
				    #'call-next-method)
	data))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data ((key integer)
                               p-btree depth p-heap)
  (multiple-value-bind (found key-objid key-type-tag data-objid data-type-tag)
      (sh-btree-search (persistent-object-objid p-heap)
                       (persistent-object-objid p-btree)
		       key +fixnum-type-tag+)
    (declare (ignore key-objid key-type-tag)
	     #-:lispworks4 ;; and hopefully not later
             (dynamic-extent found))
    (when found
      (values (p-objid-to-t-object data-objid data-type-tag depth p-heap)
	      t
	      data-objid
	      data-type-tag))))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data ((key float)
                               p-btree depth p-heap)
  (let ((found nil)
        (key-objid +null-objid+)
        (key-type-tag +null-type-tag+)
        (data-objid +null-objid+)
        (data-type-tag +null-type-tag+))
    (declare (ignore key-objid key-type-tag)
	     #-:lispworks4 ;; and hopefully not later
             (dynamic-extent found))
    (typecase key
      (short-float
       (with-transaction (p-heap)
	 (multiple-value-setq (found key-objid key-type-tag
			       data-objid data-type-tag)
	   (if +has-short-float-p+
	       (sh-btree-search (persistent-object-objid p-heap)
				(persistent-object-objid p-btree)
				(short-float-to-fixnum key)
			       +short-float-type-tag+)
	     (sh-btree-search-by-float (persistent-object-objid p-heap)
				       (persistent-object-objid p-btree)
				       key))))
       (when found
           (values (p-objid-to-t-object
                    data-objid data-type-tag depth p-heap)
                   t
                   data-objid
                   data-type-tag)))
      (single-float
       (with-transaction (p-heap)
	 (multiple-value-setq (found key-objid key-type-tag
			       data-objid data-type-tag)
	   (sh-btree-search-by-float (persistent-object-objid p-heap)
				     (persistent-object-objid p-btree)
				     key)))
       (when found
	 (values (p-objid-to-t-object data-objid data-type-tag depth p-heap)
		 t
		 data-objid
		 data-type-tag)))
      (double-float
       (with-transaction (p-heap)
	 (multiple-value-setq (found key-objid key-type-tag
			       data-objid data-type-tag)
	   (sh-btree-search-by-double (persistent-object-objid p-heap)
				      (persistent-object-objid p-btree)
				      key)))
       (when found
	 (values (p-objid-to-t-object
		  data-objid data-type-tag depth p-heap)
		 t
		 data-objid
		 data-type-tag)))
      (t
         (call-next-method)))))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data ((key character)
                               p-btree depth p-heap)
  (multiple-value-bind (found key-objid key-type-tag data-objid data-type-tag)
      (sh-btree-search (persistent-object-objid p-heap)
                       (persistent-object-objid p-btree)
		       (char-code key) +character-type-tag+)
    (declare (ignore key-objid key-type-tag)
	     #-:lispworks4 ;; and hopefully not later
             (dynamic-extent found))
    (when found
      (values (p-objid-to-t-object data-objid data-type-tag depth p-heap) t
	      data-objid data-type-tag))))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data ((key string)
                               p-btree depth p-heap)
  (multiple-value-bind (found key-objid key-type-tag data-objid data-type-tag)
      (sh-btree-search-by-string (persistent-object-objid p-heap)
                                 (persistent-object-objid p-btree)
				 key +dynamic-cstring-ptr-tag+)
    (declare (ignore key-objid key-type-tag)
	     #-:lispworks4 ;; and hopefully not later
             (dynamic-extent found))
    (when found
      (values (p-objid-to-t-object data-objid data-type-tag depth p-heap)
	      t
	      data-objid
	      data-type-tag))))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data ((key symbol)
                               p-btree depth p-heap)
  (multiple-value-bind (found key-objid key-type-tag data-objid data-type-tag)
      (sh-btree-search-by-string (persistent-object-objid p-heap)
                                 (persistent-object-objid p-btree)
				 (symbol-name key)
				 +dynamic-cstring-ptr-tag+)
    (declare (ignore key-objid key-type-tag)
	     #-:lispworks4 ;; and hopefully not later
	     (dynamic-extent found))
    (when found
      (values (p-objid-to-t-object data-objid data-type-tag depth p-heap)
	      t
	      data-objid
	      data-type-tag))))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data ((key persistent-object)
                               p-btree depth p-heap)
  #+:lisp-doc "Calls to this method are trapped to \\fcite{getbtree-by-objid-with-data}."
  (getbtree-by-objid-with-data key (persistent-object-objid key)
			       +short-objid-tag+
			       p-btree depth p-heap))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data ((key persistent-immediate-object)
                               p-btree depth p-heap)
  #+:lisp-doc "Calls to this method are trapped to \\fcite{getbtree-by-objid-with-data}."
  (getbtree-by-objid-with-data key (persistent-object-objid key)
			       (p-type-tag-of key)
			       p-btree depth p-heap))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data ((key persistent-clos-object)
                               p-btree depth p-heap)
  #+:lisp-doc "Calls to this method are trapped to \\fcite{getbtree-by-objid-with-data}."
  (getbtree-by-objid-with-data key (persistent-object-objid key)
			       +short-objid-tag+ p-btree depth p-heap))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data (key p-btree depth p-heap)
  #+:lisp-doc "Store \\funarg{key}\\ and continue."
  (multiple-value-bind (key-objid key-type-tag)
      (t-object-to-p-objid key :cached p-heap)
    (unless (p-immediatep key-type-tag)
      (setf key-type-tag +short-objid-tag+))
    (getbtree-by-objid-with-data key key-objid key-type-tag
				 p-btree depth p-heap)))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-by-objid-with-data (key key-objid key-type-tag
					p-btree depth p-heap)
  (multiple-value-bind (found found-key-objid found-key-type-tag
			data-objid data-type-tag)
      (sh-btree-search (persistent-object-objid p-heap)
                       (persistent-object-objid p-btree)
		       key-objid key-type-tag)
    (declare (ignore found-key-objid found-key-type-tag))
    (when found
      (values (p-objid-to-t-object data-objid data-type-tag depth p-heap)
	      t
	      data-objid
	      data-type-tag))))

;;; ---------------------------------------------------------------------------
(defun getbtree-from-cached-btree (key entry-key
				   key-objid key-type-tag
				   p-btree depth p-heap
				   next-method)
  #+:lisp-doc "
\\Argumentslabel
 \\isanobject{\\funarg{key}}
 \\isanobject{\\funarg{entry-key}}
 \\isacbtree{\\funarg{p-btree}}
 \\isa{\\funarg{next-method}}
      {a method function with arguments as accepted by the
       \\fcite{getbtree-with-data}}
\\Valueslabel
 See \\fcite{getbtree-with-data}.
\\Purposelabel
 Internal used function.
 Searches \\funarg{key}\\ in the cache of \\funarg{p-btree}. If a value
 was found, that value is returned as the result of the BTree search.
 If no value was found, the \\funarg{next-method}\\ is called to
 search the \\funarg{key}\\ in \\funarg{p-btree}\\ and the found value
 is inserted into \\funarg{p-btree}'s cache."

  (let ((entry (gethash entry-key (cached-btree-key->data-cache p-btree))))
    (if entry
        (let ((objid (btree-cache-entry-objid entry))
	      (type-tag (btree-cache-entry-type-tag entry)))
          (case depth
            (:objid
             (values objid t objid type-tag ))
            (:object
             (let ((object (btree-cache-entry-object entry)))
               (unless object
                 (setf object (make-persistent-object
                               (btree-cache-entry-objid entry)
                               (btree-cache-entry-type-tag entry)))
                 (setf (btree-cache-entry-object entry) object))
               (values object t objid type-tag)))
            (t
             (let ((data (btree-cache-entry-data entry)))
               (unless (btree-cache-entry-datap entry)
	         (setf data (p-objid-to-t-object objid type-tag depth p-heap))
                 (setf (btree-cache-entry-datap entry) t)
                 (setf (btree-cache-entry-data entry) data))
               (values data t objid type-tag)))))
      (multiple-value-bind (data foundp data-objid data-type-tag)
	  (funcall next-method key key-objid key-type-tag p-btree depth p-heap)
	(when foundp
          (insert-into-btree-cache data entry-key p-btree depth
                                   data-objid data-type-tag))
	(values data foundp data-objid data-type-tag)))))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data :around (key
			               (p-btree cached-btree)
			               depth p-heap)
  #+:lisp-doc "Searches \\funarg{key}\\ at first in the cache of the
 cached persistent BTree \\funarg{p-btree}."
  (if (integerp key)
      ;; No caching for real fixnums because fixnums are used as objids:
      (call-next-method)
    (getbtree-from-cached-btree key key
				nil nil ;; ???
				p-btree depth p-heap
                                #'(lambda (key key-objid key-type-tag p-btree
					   depth p-heap)
				    (declare (ignore key-objid key-type-tag))
				    (call-next-method
				     key p-btree depth p-heap)))))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data :around ((key persistent-object)
			               (p-btree cached-btree)
			               depth p-heap)
  #+:lisp-doc "Searches \\funarg{key}\\ at first in the cache of the
 cached persistent BTree \\funarg{p-btree}."
  (let ((key-objid (persistent-object-objid key)))
    (getbtree-from-cached-btree key key-objid
				key-objid +short-objid-tag+
				p-btree depth p-heap
				#'getbtree-by-objid-with-data)))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-with-data :around ((key persistent-clos-object)
			               (p-btree cached-btree)
			               depth p-heap)
  #+:lisp-doc "Searches \\funarg{key}\\ at first in the cache of the
 cached persistent BTree \\funarg{p-btree}."
  (let ((key-objid (persistent-object-objid key)))
    (getbtree-from-cached-btree key key-objid
				key-objid +short-objid-tag+
				p-btree depth p-heap
				#'getbtree-by-objid-with-data)))

;;; ---------------------------------------------------------------------------
(defmethod getbtree-by-objid-with-data (key key-objid key-type-tag
			                (p-btree cached-btree)
			                depth p-heap)
  #+:lisp-doc "Searches \\funarg{key}\\ at first in the cache of the
 cached persistent BTree \\funarg{p-btree}."
  (getbtree-from-cached-btree key key-objid
			      key-objid key-type-tag
			      p-btree depth p-heap
			      #'call-next-method))

;;; ---------------------------------------------------------------------------
(defun btree-pagesize (p-btree &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Return the btree's internal used page size. The page size is the number
 of objects hold on a single btree page.
\\Seealsolabel
 \\Fcite{(setf btree-pagesize)}."
  (c-sh-btree-set-page-size (persistent-object-objid p-heap)
			    (persistent-object-objid p-btree)
			    -1))

;;; ---------------------------------------------------------------------------
(defun (setf btree-pagesize) (new-value p-btree
			      &optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Set the btree's internal used page size. It can only be set for empty
 btrees. The passed size must be an even number between 4 and 2712. 
\\Seealsolabel
 \\Fcite{btree-pagesize}."
  (c-sh-btree-set-page-size (persistent-object-objid p-heap)
			    (persistent-object-objid p-btree)
			    new-value)
  new-value)

;;; ---------------------------------------------------------------------------
(defun btree-minkey (p-btree &optional (depth *default-depth*)
				       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Get the minimum key contained in \\funarg{p-btree}.
\\Seealsolabel
 \\Fcite{btree-maxkey}."
  (with-transaction (p-heap)
    (multiple-value-bind (found key-objid key-type-tag
			  data-objid data-type-tag)
	(sh-btree-search (persistent-object-objid p-heap)
			 (persistent-object-objid p-btree)
			 +min-tag+ +min-tag+)
      (declare (ignore data-objid data-type-tag)
	       #-:lispworks4 ;; and hopefully not later
	       (dynamic-extent found))
      (when found
	(values (p-objid-to-t-object key-objid key-type-tag depth p-heap)
		t
		key-objid
		key-type-tag)))))

;;; ---------------------------------------------------------------------------
(defun btree-maxkey (p-btree &optional (depth *default-depth*)
				       (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Get the maximum key contained in \\funarg{p-btree}.
\\Seealsolabel
 \\Fcite{btree-minkey}."
  (with-transaction (p-heap)
    (multiple-value-bind (found key-objid key-type-tag
			  data-objid data-type-tag)
	(sh-btree-search (persistent-object-objid p-heap)
			 (persistent-object-objid p-btree)
			 +max-tag+ +max-tag+)
      (declare (ignore data-objid data-type-tag)
	       #-:lispworks4 ;; and hopefully not later
	       (dynamic-extent found))
      (when found
	(values (p-objid-to-t-object key-objid key-type-tag depth p-heap)
		t
		key-objid
		key-type-tag)))))

;;; ---------------------------------------------------------------------------
;;; Loading of BTrees
;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +btree-type-tag+))
				depth p-heap)
  #+:lisp-doc "Loads a persistent BTree from the \\sh."
  (declare (ignore depth))
  (let ((object (is-registered-objid p-objid)))
    (unless object
      (setf object (make-btree :objid p-objid :p-heap p-heap)))
    object))

;;; ---------------------------------------------------------------------------
(defmethod p-objid-to-t-object (p-objid
				(p-objid-type-tag (eql +btree-page-tag+))
				depth p-heap)
  #+:lisp-doc "Loads a persistent BTree page from the \\sh."
  (declare (ignore depth))
  (let ((object (is-registered-objid p-objid)))
    (unless object
      (setf object (make-persistent-object p-objid p-objid-type-tag))
      (register-to-cache p-objid object))
    object))

;;;; Local variables:
;;;; buffer-file-coding-system: raw-text-unix
;;;; End:
