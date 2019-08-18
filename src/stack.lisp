(in-package #:clds)

(defclass stack ()
  ((content :initform (make-dlist)
            :reader %stack-content
            :documentation "The content of the stack")))

(defun make-stack ()
  (make-instance 'stack))

(defmethod stack-size ((s stack))
  "Returns the size of S."
  (dlist-size (%stack-content s)))

(defmethod stack-empty-p ((s stack))
  "Returns T if stack is empty, nil otherwise"
  (dlist-empty-p (%stack-content s)))

(defmethod stack-peek-element ((s stack))
  "Return the first value of S. Throws error if S is empty."
  (when (stack-empty-p s)
    (error "Cant peek on empty stack"))
  (dlist-get-element-front (%stack-content s)))

(defmethod stack-get-elements ((s stack))
  "Returns a list with all elements in S in pop order."
  (dlist-get-elements (%stack-content s)))

(defmethod stack-push-element ((s stack) element)
  (dlist-add-element-front (%stack-content s) element))

(defmethod stack-push-elements ((s stack) &rest elements)
  (dolist (element elements)
    (stack-push-element s element)))

(defmethod stack-pop-element ((s stack))
  (when (stack-empty-p s)
    (error "Cant pop from empty stack"))
  (let ((rv (dlist-get-element-front (%stack-content s))))
    (dlist-remove-element-front (%stack-content s))
    rv))
