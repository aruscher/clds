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

(defmethod queue-peek-element ((q queue))
  "Return the first element of Q. Throws error if Q is empty."
  (when (queue-empty-p q)
    (error "Cant peek on empty queue."))
  (dlist-get-element-front (queue-content q)))

(defmethod queue-get-elements ((q queue))
  "Returns all elements in Q."
  (dlist-get-elements (queue-content q)))

(defmethod queue-enqueue-element ((q queue) element)
  "Enqueues ELEMENT in Q."
  (dlist-add-element-end (queue-content q) element))

(defmethod queue-enqueue-elements ((q queue) &rest elements)
  "Enqueues the ELEMENTS in Q."
  (dolist (element elements)
    (queue-enqueue-element q element)))

(defmethod queue-dequeue-element ((q queue))
  "Returns the first element in Q and removes it. Throws error if Q is empty."
  (when (queue-empty-p q)
    (error "Cant dequeue from empty queue."))
  (let ((rv (queue-peek-element  q)))
    (dlist-remove-element-front (queue-content q))
    rv))
