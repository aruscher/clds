(in-package #:clds)

(defclass queue ()
  ((content :initform (make-dlist)
            :reader queue-content
            :documentation "The content of the queue.")))

(defun make-queue ()
  "Creates a new queue instance."
  (make-instance 'queue))

(defmethod print-object ((q queue) stream)
  (print-unreadable-object (q stream :type t :identity t)
    (format stream "~a" (dlist-get-elements (queue-content q)))))

(defmethod queue-size ((q queue))
  "Returns the size of the queue."
  (dlist-size (queue-content q)))

(defmethod queue-empty-p ((q queue))
  "Return t if queue is empty, nil otherwise."
  (dlist-empty-p (queue-content q)))

(defmethod queue-peek ((q queue))
  (when (queue-empty-p q)
    (error "Cant peek on empty queue."))
  (dlist-get-element-front (queue-content q)))

(defmethod queue-enqueue-element ((q queue) element)
  (dlist-add-element-end (queue-content q) element))

(defmethod queue-enqueue-elements ((q queue) &rest elements)
  (dolist (element elements)
    (queue-enqueue-element q element)))

(defmethod queue-dequeue ((q queue))
  (when (queue-empty-p q)
    (error "Cant dequeue from empty queue."))
  (let ((rv (queue-peek  q)))
    (dlist-remove-element-front (queue-content q))
    rv))
