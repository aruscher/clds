(in-package #:clds)

(defclass dlist-element ()
  ((value :initarg :value
          :reader dlist-element-value
          :documentation "The value of the element.")
   (next :initarg :next
         :initform nil
         :accessor dlist-element-next
         :documentation "Pointer to the next element.")
   (previous :initarg :previous
             :initform nil
             :accessor dlist-element-previous
             :documentation "Pointer to the previous element.")))

(defmethod print-object ((elem dlist-element) stream)
  (print-unreadable-object (elem stream :type t :identity t)
    (format stream "~a" (dlist-element-value elem))))

(defmethod link-dlist-elements ((e1 dlist-element) (e2 dlist-element))
  (setf (dlist-element-next e1) e2)
  (setf (dlist-element-previous e2) e1))

(defun make-dlist-element (value next previous)
  (make-instance 'dlist-element
                 :value value
                 :next next
                 :previous previous))
