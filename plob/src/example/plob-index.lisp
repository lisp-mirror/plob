
(defclass foo ()
  (
   (slot-1 :initarg :slot-1 :index (btree :test equal))
   )
  (:metaclass persistent-metaclass))
