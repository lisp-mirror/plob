;;;; -*-Lisp-*-----------------------------------------------------------------
;;;; Module	plob-btree-mapper.lisp
;;;; Author	Heiko Kirschke, Fachbereich Informatik, Universitaet Hamburg
;;;;		mailto:Heiko.Kirschke@acm.org
;;;; Date	1998/06/26
;;;; Description	PLOB persistent btree mapper class methods.
;;;;
;;;; Copyright	PLOB! Copyright 1994--2001 Heiko Kirschke.
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
(defgeneric mapbtree (map-function p-btree
		                   &key
				   ((:< less) nil lessp)
				   ((:<= less-equal) nil less-equal-p)
				   ((:> greater) nil greaterp)
				   ((:>= greater-equal) nil greater-equal-p)
				   descending
                                   (key-depth *default-depth*)
                                   (data-depth *default-depth*)
                                   (p-heap *default-persistent-heap*))
  #+:lisp-doc (:documentation "
\\Argumentslabel
 \\isa{\\funarg{map-function}}
      {a function which accepts two arguments}
 \\isabtree{\\funarg{p-btree}}
 \\isakey{\\keyarg{<}\\ resp.\\ \\keyarg{<=}
          \\keyarg{>}\\ resp.\\ \\keyarg{>=}}{\\funarg{p-btree}}
 \\isa{\\keyarg{key-depth}\\ resp.\\ \\keyarg{data-depth}}
      {a keyword symbol}
\\Valueslabel
 The number of mapped elements is returned, i.e.\\ the number of times
 the \\funarg{map-function}\\ was called.
\\Purposelabel
 For each entry in \\funarg{p-btree}\\ with a key in the range
 specified from \\keyarg{>}\\ resp.\\ \\keyarg{>=}\\ to
 \\keyarg{<}\\ resp.\\ \\keyarg{<=}, {\\bf mapbtree} calls
 \\funarg{map-function}\\ on
 two arguments: the key and associated data of the entry loaded
 to the depths specified by
 \\keyarg{key-depth}\\ resp.\\ \\keyarg{data-depth}. The called
 \\funarg{map-function}\\ has to return with \\nonnil\\ to
 continue the mapping; if
 \\funarg{map-function}\\ returns \\lispnil, the mapping is stopped.

 If no \\keyarg{start}-argument is passed, the iteration starts at
 the first element of \\funarg{p-btree}.
 If no \\keyarg{end}-argument is passed, the iteration stops {\\sl after}
 the very last element of \\funarg{p-btree}.
\\Remarkslabel
 The keys of the elements in \\funarg{p-btree}\\ are ordered; the
 passed keys to \\funarg{map-function}\\ obey the order placed on the
 keys in \\funarg{p-btree}.

 Insert- and delete-operations of any elements may be performed on
 \\funarg{p-btree}\\ by the called \\funarg{map-function}; this is
 different to \\cl\\ hash tables where these operations have undefined
 effects.
\\Seealsolabel
 \\fcite{maphash}."))

;;; ---------------------------------------------------------------------------
(defgeneric mapbtree-internal (map-function
			       p-btree-objid
			       start-value start-type-tag compare-start
			       end-value end-type-tag compare-end
			       descending p-heap)
  #+:lisp-doc (:documentation "
\\Argumentslabel
 For the \\funarg{map-function}, \\funarg{start-value}\\ and
 \\funarg{end-value}\\ arguments see \\fcite{mapbtree}.
 \\isanobjid{\\funarg{p-btree-objid}}
 \\isatypetag{\\funarg{start-type-tag}}
 \\isatypetag{\\funarg{end-type-tag}}
\\Purposelabel
 Internal used function for \\fcite{mapbtree}.
\\Remarkslabel
 There are some problems with \\lw\\ 3.2.0: The methods of
 {\\bf mapbtree-internal} call C code which calls \\cl\\ code
 which might call C code again.
 This seems to trash either the foreign language stack or
 the \\cl\\ stack a little bit so that there are shown error
 messages like
 \\begin{quote}\\tt
  <**> pointer out of bounds in fixup-stack pointer 7ff4ea
  field 10ca0fcc stack 10c92393
 \\end{quote}
 in the shell window from which \\lw\\ has been started up.
 These kind of messages normally indicate heavy problems
 with the garbage collector which normally crashes the system,
 but \\lw\\ 3.2.0 keeps on running (stable?).

 In \\lw\\ 3.1.1 these problems did not occure; so the indicated
 problem is perhaps an effect of an `optimization degration' done
 for the foreign language interface of 3.2.0.
\\Seealsolabel
 \\Fcite{mapbtree}."))

;;; ---------------------------------------------------------------------------
(defgeneric btree-mapper-search-internal
    (p-mapper-objid p-btree-objid
     start-value start-type-tag compare-start
     end-value end-type-tag compare-end
     descending p-heap)
  #+:lisp-doc (:documentation "
\\Argumentslabel
\\Purposelabel
 Internal used function for \\fcite{btree-mapper-search}.
\\Remarkslabel
\\Seealsolabel
 \\Fcite{btree-mapper-search}."))

;;; ---------------------------------------------------------------------------
(defmethod mapbtree-internal (map-function
			      p-btree-objid
			      (start-value integer) start-type-tag
			      compare-start
			      (end-value integer) end-type-tag
			      compare-end
                              descending p-heap)
  (sh-btree-map p-heap p-btree-objid map-function
  		start-value start-type-tag compare-start
                end-value end-type-tag compare-end descending))

;;; ---------------------------------------------------------------------------
(defmethod mapbtree-internal (map-function
			      p-btree-objid
			      (start-value float) start-type-tag
			      compare-start
			      (end-value float) end-type-tag
			      compare-end
			      descending p-heap)
  (typecase start-value
    (single-float
     (sh-btree-map-by-float p-heap p-btree-objid map-function
                            start-value start-type-tag compare-start
                            end-value end-type-tag compare-end descending))
    (double-float
     (sh-btree-map-by-double p-heap p-btree-objid map-function
                             start-value start-type-tag compare-start
                             end-value end-type-tag compare-end descending))
    (t
     (call-next-method))))

;;; ---------------------------------------------------------------------------
(defmethod mapbtree-internal (map-function
			      p-btree-objid
			      (start-value string) start-type-tag
			      compare-start
			      (end-value string) end-type-tag
			      compare-end
			      descending p-heap)
  (sh-btree-map-by-string p-heap p-btree-objid map-function
                          start-value start-type-tag compare-start
                          end-value end-type-tag compare-end descending))

;;; ---------------------------------------------------------------------------
(defun make-map-key (value type-tag depth p-heap)
  (typecase value
    ((or simple-string string)
     (values value (if (= type-tag +string-type-tag+)
		       +dynamic-cstring-ptr-tag+
		     type-tag)))
    (symbol
     (values (symbol-name value)
	     (if (= type-tag +symbol-type-tag+)
		 +dynamic-cstring-ptr-tag+
	       type-tag)))
    (short-float
     (if +has-short-float-p+
	 (values (short-float-to-fixnum value) type-tag)
       (values value
	       (if (= type-tag +single-float-type-tag+)
		   +dynamic-cfloat-ptr-tag+
		 type-tag))))
    (single-float
     (values value
	     (if (= type-tag +single-float-type-tag+)
		 +dynamic-cfloat-ptr-tag+
	       type-tag)))
    (double-float
     (values value
	     (if (= type-tag +double-float-type-tag+)
		 +dynamic-cdouble-ptr-tag+
	       type-tag)))
    (persistent-immediate-object
     (values (persistent-object-objid value)
	     (p-type-tag-of value)))
    (persistent-object
     (values (persistent-object-objid value)
	     +short-objid-tag+))
    (persistent-clos-object
     (values (persistent-object-objid value)
	     +short-objid-tag+))
    (t
     (if type-tag
	 (if (p-immediatep type-tag)
	     (values value type-tag)
	   (values value +short-objid-tag+))
       (progn
	 (multiple-value-setq (value type-tag)
	   (t-object-to-p-objid value depth p-heap))
	 (unless (p-immediatep type-tag)
	   (setf type-tag +short-objid-tag+))
	 (values value type-tag))))))

;;; ---------------------------------------------------------------------------
(defun make-2-map-keys (less lessp
			less-equal less-equal-p
			greater greaterp
			greater-equal greater-equal-p
			depth p-heap)
    (let ((start nil) (startp nil)
	  (start-value +min-tag+) (start-type-tag +min-tag+)
	  (compare-start +greater-equal+)
	  (end nil) (endp nil)
	  (end-value +max-tag+) (end-type-tag +max-tag+)
	  (compare-end +less-equal+))
      (when (or lessp less-equal-p)
	(when (and lessp less-equal-p)
	  (error "Only none or one of :< and :<= must be given."))
	(setf endp t)
	(if lessp
	    (progn
	      (setf end less)
	      (setf compare-end +less+))
	    (progn
	      (setf end less-equal)
	      (setf compare-end +less-equal+))))
      (when (or greaterp greater-equal-p)
	(when (and greaterp greater-equal-p)
	  (error "Only none or one of :> and :>= must be given."))
	(setf startp t)
	(if greaterp
	    (progn
	      (setf start greater)
	      (setf compare-start +greater+))
	    (progn
	      (setf start greater-equal)
	      (setf compare-start +greater-equal+))))
      (cond
       ((and startp endp)
	;; Both start and end key have been specified:
	(multiple-value-setq (start-value start-type-tag)
	  (make-map-key start (t-type-tag-of start) depth p-heap))
	(multiple-value-setq (end-value end-type-tag)
	  (make-map-key end (t-type-tag-of end) depth p-heap)))
       ((and startp (not endp))
	;; Only a start key has been specified:
	(multiple-value-setq (start-value start-type-tag)
	  (make-map-key start (t-type-tag-of start) depth p-heap))
	;; end-type-tag set to +max-tag+ will break current
	;; setting of end-value:
	(setf end-value start-value))
       ((and (not startp) endp)
	;; Only an end key has been specified:
	(multiple-value-setq (end-value end-type-tag)
	  (make-map-key end (t-type-tag-of end) depth p-heap))
	;; start-type-tag set to +min-tag+ will break current
	;; setting of start-value:
	(setf start-value end-value)))
      (values start-value start-type-tag compare-start
	      end-value end-type-tag compare-end)))
	      

;;; ---------------------------------------------------------------------------
(defmethod mapbtree (map-function p-btree
		     &key
		     ((:< less) nil lessp)
		     ((:<= less-equal) nil less-equal-p)
		     ((:> greater) nil greaterp)
		     ((:>= greater-equal) nil greater-equal-p)
		     descending
		     (key-depth *default-depth*)
		     (data-depth *default-depth*)
		     (p-heap *default-persistent-heap*))
  (labels
      ((make-map-object
	   (value type-tag depth p-heap)
	 (case depth
	   (:objid value)
	   (:object (make-persistent-object value type-tag))
	   (t (p-objid-to-t-object value type-tag depth p-heap)))))

    (multiple-value-bind (start-value start-type-tag compare-start
			  end-value end-type-tag compare-end)
	(make-2-map-keys less lessp less-equal less-equal-p
			 greater greaterp greater-equal greater-equal-p
			 :cached p-heap)
      (with-transaction (p-heap)
	(mapbtree-internal
	 #'(lambda (key-value key-type-tag data-value data-type-tag)
	     (funcall map-function
		      (make-map-object key-value key-type-tag key-depth
				       p-heap)
		      (make-map-object data-value data-type-tag data-depth
				       p-heap)))
	 (persistent-object-objid p-btree)
	 start-value start-type-tag compare-start
	 end-value end-type-tag compare-end
	 (if descending +c-true+ +c-false+)
	 (persistent-object-objid p-heap))))))

;;; ---------------------------------------------------------------------------
(defmethod btree-mapper-search-internal
    (p-mapper-objid p-btree-objid
     (start-value integer) start-type-tag compare-start
     (end-value integer) end-type-tag compare-end
     descending p-heap)
  (sh-btree-map-search p-heap p-mapper-objid p-btree-objid
		       start-value start-type-tag compare-start
		       end-value end-type-tag compare-end
		       descending))

;;; ---------------------------------------------------------------------------
(defmethod btree-mapper-search-internal
    (p-mapper-objid p-btree-objid
     (start-value float) start-type-tag compare-start
     (end-value float) end-type-tag compare-end
     descending p-heap)
  (typecase start-value
    (single-float
     (sh-btree-map-search-by-float p-heap p-mapper-objid p-btree-objid
				   start-value start-type-tag compare-start
				   end-value end-type-tag compare-end
				   descending))

    (double-float
     (sh-btree-map-search-by-double p-heap p-mapper-objid p-btree-objid
				    start-value start-type-tag compare-start
				    end-value end-type-tag compare-end
				    descending))
    (t
     (call-next-method))))

;;; ---------------------------------------------------------------------------
(defmethod btree-mapper-search-internal
    (p-mapper-objid p-btree-objid
     (start-value string) start-type-tag compare-start
     (end-value string) end-type-tag compare-end
     descending p-heap)
  (sh-btree-map-search-by-string p-heap p-mapper-objid p-btree-objid
				 start-value start-type-tag compare-start
				 end-value end-type-tag compare-end
				 descending))

;;; ---------------------------------------------------------------------------
(defun p-allocate-btree-mapper (&optional (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Returns the \\objid\\ of a new initially empty persistent BTree
 mapper allocated in the \\sh.
\\Seealsolabel
 \\Fcite{make-btree-mapper};
 \\fcite{p-allocate}."
  (p-allocate p-heap +btree-mapper-tag+))

;;; ---------------------------------------------------------------------------
(defun make-btree-mapper (p-btree
			  &key
			  (objid nil)
			  ((:< less) nil lessp)
			  ((:<= less-equal) nil less-equal-p)
			  ((:> greater) nil greaterp)
			  ((:>= greater-equal) nil greater-equal-p)
			  descending
			  (depth :cached)
			  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Purposelabel
 Make a btree mapper.
\\Seealsolabel
 \\Fcite{btree-mapper-search}; \\fcite{btree-mapper-seek}."
  (let* ((p-objid-mapper (if objid
			     objid
			   (progn
			     (assert-sh-open-p)
			     (p-allocate-btree-mapper p-heap))))
	 (p-mapper (make-persistent-btree-mapper p-objid-mapper)))
    (multiple-value-bind (start-value start-type-tag compare-start
			  end-value end-type-tag compare-end)
	(make-2-map-keys less lessp less-equal less-equal-p
			 greater greaterp greater-equal greater-equal-p
			 depth p-heap)
      (with-transaction (p-heap)
	(btree-mapper-search-internal
	 (persistent-object-objid p-mapper)
	 (persistent-object-objid p-btree)
	 start-value start-type-tag compare-start
	 end-value end-type-tag compare-end
	 descending
	 (persistent-object-objid p-heap))))
    p-mapper))

;;; ---------------------------------------------------------------------------
(defun btree-mapper-search (p-mapper
			    &key
			    btree
			    ((:< less) nil lessp)
			    ((:<= less-equal) nil less-equal-p)
			    ((:> greater) nil greaterp)
			    ((:>= greater-equal) nil greater-equal-p)
			    descending
			    (depth :cached)
			    (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isabtree{\\keyarg{btree}}
 \\isakey{\\keyarg{<}\\ resp.\\ \\keyarg{<=}
          \\keyarg{>}\\ resp.\\ \\keyarg{>=}}{\\keyarg{btree}}
\\Purposelabel
 Set the search interval of a btree mapper. The \\keyarg{btree}\\ argument
 specifies the btree which should be mapped; if no btree is given here,
 the mapper will continue to use the btree it currently refers to.
 The \\keyarg{>}\\ or \\keyarg{>=}\\ argument specify the lower
 interval boundary. The \\keyarg{<}\\ or \\keyarg{<=}\\ argument
 specify the upper interval boundary.
\\Seealsolabel
 \\Fcite{make-btree-mapper}, \\fcite{btree-mapper-seek}."
  (multiple-value-bind (start-value start-type-tag compare-start
			end-value end-type-tag compare-end)
      (make-2-map-keys less lessp less-equal less-equal-p
		       greater greaterp greater-equal greater-equal-p
		       depth p-heap)
      (with-transaction (p-heap)
	(btree-mapper-search-internal
	 (persistent-object-objid p-mapper)
	 (if btree
	     (persistent-object-objid btree)
	   +null-objid+)
	 start-value start-type-tag compare-start
	 end-value end-type-tag compare-end
	 descending
	 (persistent-object-objid p-heap)))))

;;; ---------------------------------------------------------------------------
(defun btree-mapper-seek (p-mapper &optional
				   (increment 0)
				   (origin :current)
				   (depth *default-depth*)
				   (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanumber{\\funarg{increment}}
 \\isakeyword{\\funarg{origin}}
\\Valueslabel
 If there was no key found matching the current search interval,
 \\lispnil\\ is returned. If there is at least one key within
 the current search interval, up to three values are returned:
 \\begin{enumerate}
 \\item As first value, the absolute number of the position
   increment is returned, that means, a number which is less
   or equal to \\lisp{(abs \\funarg{increment})}. A value of 0
   means that the mapper has not changed its current position.
   (Negative numbers are used internally as error conditions
   from the lower C layer.)
 \\item If \\funarg{depth}\\ is \\nonnil, the key object of the new
   position is loaded and returned as second value.
 \\item If \\funarg{depth}\\ is \\nonnil, the data object of the new
   position is loaded and returned as third value.
 \\end{enumerate}
\\Purposelabel
 Seek a new current position for \\funarg{p-mapper}.
 The \\funarg{origin}\\ argument must be a symbol out of the set
 \\{ \\lisp{:set}, \\lisp{:current}, \\lisp{:end} \\}. For \\lisp{:set},
 the position in \\funarg{increment}\\ is searched relative to the
 start of the interval. For \\lisp{:current}, the position in
 \\funarg{increment}\\ is searched relative to the current position
 in the interval. For \\lisp{:end}, the position in
 \\funarg{increment}\\ is searched relative to the end of the interval.
 For getting the current key and data object the mapper refers to,
 call this function with \\funarg{increment}\\ set to 0. For doing
 only an increment of the mapper's current position, pass the
 \\funarg{depth}\\ argument as \\lispnil.

 An \\funarg{origin}\\ of \\lisp{:current}\\ should be used whenever
 possible, since using an \\funarg{origin}\\ of \\lisp{:set}\\ or
 \\lisp{:end}\\ involves at most \\funarg{increment}\\ iterated
 position changes in the btree mapper.
\\Seealsolabel
 \\Fcite{make-btree-mapper}, \\fcite{btree-mapper-search}."

  (with-transaction (p-heap)
    (multiple-value-bind (mapped key-value key-type-tag
			  data-value data-type-tag)
	(sh-btree-map-seek (persistent-object-objid p-heap)
			   (persistent-object-objid p-mapper)
			   increment origin)
      (cond
       ((= mapped +error-key-not-found+)
	nil)
       (depth
	(values mapped
		(p-objid-to-t-object key-value key-type-tag depth p-heap)
		(p-objid-to-t-object data-value data-type-tag
				     depth p-heap)))
       (t mapped)))))

;;; ---------------------------------------------------------------------------
(defun (setf btree-mapper-seek) (new-value
				 p-mapper &optional
					  (increment 0)
					  (origin :current)
					  (depth *default-depth*)
					  (p-heap *default-persistent-heap*))
  #+:lisp-doc "
\\Argumentslabel
 \\isanumber{\\funarg{increment}}
 \\isakeyword{\\funarg{origin}}
\\Valueslabel
 \\retarg{\\funarg{new-value}}
\\Purposelabel
 Seek a new current position for \\funarg{p-mapper}\\ and set the
 data entry to \\funarg{new-value}.
\\Seealsolabel
 \\Fcite{btree-mapper-seek}, \\fcite{make-btree-mapper}."

  (with-transaction (p-heap)
    (multiple-value-bind (immediate-data-value immediate-data-type-tag)
	(t-object-to-p-objid new-value depth p-heap)
      (sh-btree-map-seek-set (persistent-object-objid p-heap)
			     (persistent-object-objid p-mapper)
			     increment origin
			     immediate-data-value
			     immediate-data-type-tag)))
  new-value)

;;;; Local variables:
;;;; buffer-file-coding-system: iso-latin-1-unix
;;;; End:
