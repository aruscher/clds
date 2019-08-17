(in-package #:clds)

(defclass queue ()
  ((content :initform (make-dlist)
            :reader queue-content
            :documentation "The content of the queue.")))

(defun make-queue ()
  (make-instance 'queue))

(defmethod print-object ((q queue) stream)
  (print-unreadable-object (q stream :type t :identity t)
    (format stream "~a" (dlist-get-elements (queue-content q)))))

(defmethod queue-size ((q queue))
  (dlist-size (queue-content q)))

(defmethod queue-empty-p ((q queue))
  (dlist-empty-p (queue-content q)))

(defmethod queue-peek ((q queue))
  (when (queue-empty-p q)
    (error "Cant peek on empty queue."))
  (dlist-get-element-front (queue-content q)))

(defmethod queue-enqueue ((q queue) element)
  (dlist-add-element-end (queue-content q) element))

(defmethod queue-dequeue ((q queue))
  (when (queue-empty-p q)
    (error "Cant dequeue from empty queue."))
  (let ((rv (queue-peek  q)))
    (dlist-remove-element-front (queue-content q))
    rv))

(defparameter *q* (make-queue))
(queue-enqueue *q* 1)
(queue-enqueue *q* 2)
(queue-enqueue *q* 3)
(queue-enqueue *q* 4)
(queue-enqueue *q* 5)