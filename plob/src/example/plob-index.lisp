
(defclass foo ()
  (
   (slot-1 :initarg :slot-1 :index (btree :test equal))
   )
  (:metaclass persistent-metaclass))

(defun make-foo (n)
  (with-transaction ()
    (dotimes (i n)
      (make-instance 'foo :slot-1 (format nil "instance #~a" i)))))

