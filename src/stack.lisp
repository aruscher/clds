(in-package #:clds)

(defclass stack ()
  ((content :initform (make-dlist)
            :reader %stack-content
            :documentation "The content of the stack")))

(defun make-stack ()
  (make-instance 'stack))

(defmethod stack-push-element ((s stack) element)
  (dlist-add-element-front (%stack-content s) element))

(defmethod stack-size ((s stack))
  (dlist-size (%stack-content s)))

(defmethod stack-empty-p ((s stack))
  (dlist-empty-p (%stack-content s)))

(defmethod stack-peek-element ((s stack))
  (when (stack-empty-p s)
    (error "Cant peek on empty stack"))
  (dlist-get-element-front (%stack-content s)))

(defmethod stack-pop-element ((s stack))
  (when (stack-empty-p s)
    (error "Cant pop from empty stack"))
  (let ((rv (dlist-get-element-front (%stack-content s))))
    (dlist-remove-element-front (%stack-content s))
    rv))
