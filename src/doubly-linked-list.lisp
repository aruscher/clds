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

(defmethod %dlist-link-elements ((e1 dlist-element) (e2 dlist-element))
  (setf (dlist-element-next e1) e2)
  (setf (dlist-element-previous e2) e1))

(defun make-dlist-element (value next previous)
  (make-instance 'dlist-element
                 :value value
                 :next next
                 :previous previous))

(defclass dlist ()
  ((first :initform nil
          :accessor dlist-first
          :documentation "The first element of the list.")
   (last :initform nil
         :accessor dlist-last
         :documentation "The last element of the list.")
   (size :initform 0
         :accessor dlist-size
         :documentation "The number of elements in the list.")))

(defun make-dlist ()
  (make-instance 'dlist))

(defmethod dlist-empty-p ((dl dlist))
  "Checks if a DL is empty. T if empty. NIL otherwise."
  (= (dlist-size dl) 0))

(defmethod dlist-add-element-end ((dl dlist) value)
  "Adds an element with VALUE to the end of DL."
  (let ((element (make-dlist-element value nil nil)))
    (if (dlist-empty-p dl)
        (setf (dlist-first dl) element)
        (%dlist-link-elements (dlist-last dl) element))
    (setf (dlist-last dl) element)
    (incf (dlist-size dl))
    dl))

(defmethod dlist-add-element-front ((dl dlist) value)
  "Adds an element with VALUE at the begin of DL."
  (let ((element (make-dlist-element value nil nil)))
    (if (dlist-empty-p dl)
        (setf (dlist-last dl) element)
        (%dlist-link-elements element (dlist-first dl)))
    (setf (dlist-first dl) element)
    (incf (dlist-size dl))
    dl))

(defmethod %dlist-check-index ((dl dlist) index)
  (when (>= index (dlist-size dl))
    (error "Index has to be between 0 and ~a." (- (dlist-size dl) 1)))
  (when (< index 0)
    (error "Index has to be positive.")))

(defmacro do-dlist ((node-var index-var) dlist &body body)
  `(loop :for ,node-var = (dlist-first ,dlist) :then (dlist-element-next ,node-var)
         :for ,index-var :from 0 :below (dlist-size ,dlist)
         :do ,@body))

(defmethod dlist-get-element ((dl dlist) index)
  "Returns the element at INDEX from the DL. Throws error if INDEX < 0 or INDEX >= size of dlist."
  (%dlist-check-index dl index)
  (do-dlist (node i) dl
    (when (= i index)
      (return node))))

(defmethod dlist-find-element ((dl dlist) element &key (test #'eql))
  "Returns the index of the first occurence of ELEMENT in DL. NIL otherwise. TEST is used to test the equality of the elements."
  (do-dlist (node i) dl
    (when (funcall test (dlist-element-value node) element)
      (return-from dlist-find-element i)))
  nil)

(defparameter *dl* (make-dlist))
(dlist-add-element-end *dl* 1)
(dlist-add-element-end *dl* 2)
(dlist-add-element-end *dl* 3)
(dlist-add-element-end *dl* 2)
