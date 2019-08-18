(in-package #:clds)

(defclass dlist-element ()
  ((value :initarg :value
          :reader %dlist-element-value
          :documentation "The value of the element.")
   (next :initarg :next
         :initform nil
         :accessor %dlist-element-next
         :documentation "Pointer to the next element.")
   (previous :initarg :previous
             :initform nil
             :accessor %dlist-element-previous
             :documentation "Pointer to the previous element.")))

(defmethod print-object ((elem dlist-element) stream)
  (print-unreadable-object (elem stream :type t :identity t)
    (format stream "~a" (%dlist-element-value elem))))

(defmethod %dlist-link-elements ((e1 dlist-element) (e2 dlist-element))
  (setf (%dlist-element-next e1) e2)
  (setf (%dlist-element-previous e2) e1))

(defun make-dlist-element (value next previous)
  (make-instance 'dlist-element
                 :value value
                 :next next
                 :previous previous))

(defclass dlist ()
  ((first :initform nil
          :accessor %dlist-first
          :documentation "The first element of the list.")
   (last :initform nil
         :accessor %dlist-last
         :documentation "The last element of the list.")
   (size :initform 0
         :accessor %dlist-size
         :documentation "The number of elements in the list.")))

(defmethod print-object ((dl dlist) stream)
  (print-unreadable-object (dl stream :type t :identity t)
    (format stream "~a" (dlist-get-elements dl))))

(defun make-dlist ()
  (make-instance 'dlist))


(defmethod dlist-size ((dl dlist))
  (%dlist-size dl))

(defmethod dlist-empty-p ((dl dlist))
  "Checks if a DL is empty. T if empty. NIL otherwise."
  (= (%dlist-size dl) 0))

(defmethod dlist-add-element-at ((dl dlist) value index)
  "Adds an element with VALUE at INDEX in DL. Throws error if INDEX < 0 or INDEX >= size of DL."
  (%dlist-check-index dl index)
  (if (= index 0)
      (dlist-add-element-front dl value)
      (progn
        (let ((element (make-dlist-element value nil nil))
              (o-element (%dlist-get-element-node-at dl index)))
          (%dlist-link-elements (%dlist-element-previous o-element) element)
          (%dlist-link-elements element o-element)
          (incf (%dlist-size dl)))))
  dl)

(defmethod dlist-remove-element-at ((dl dlist) index)
  "Remove an element at INDEX in DL. Throws error if INDEX < 0 or INDEX >= size of DL."
  (%dlist-check-index dl index)
  (cond
    ((= index 0) (dlist-remove-element-front dl))
    ((= index (%dlist-max-index dl)) (dlist-remove-element-end dl))
    (t (let ((element (%dlist-get-element-node-at dl index)))
         (%dlist-link-elements (%dlist-element-previous element)
                               (%dlist-element-next element))
         (decf (%dlist-size dl)))))
  dl)

(defmethod dlist-add-element-end ((dl dlist) value)
  "Adds an element with VALUE to the end of DL."
  (let ((element (make-dlist-element value nil nil)))
    (if (dlist-empty-p dl)
        (setf (%dlist-first dl) element)
        (%dlist-link-elements (%dlist-last dl) element))
    (setf (%dlist-last dl) element)
    (incf (%dlist-size dl))
    dl))

(defmethod dlist-add-element-front ((dl dlist) value)
  "Adds an element with VALUE at the begin of DL."
  (let ((element (make-dlist-element value nil nil)))
    (if (dlist-empty-p dl)
        (setf (%dlist-last dl) element)
        (%dlist-link-elements element (%dlist-first dl)))
    (setf (%dlist-first dl) element)
    (incf (%dlist-size dl))
    dl))

(defmethod dlist-add-elements ((dl dlist) &rest values)
  "Adds the VALUES one after another to the end of DL."
  (dolist (value values)
    (dlist-add-element-end dl value)))

(defmethod dlist-remove-element-end ((dl dlist))
  "Removes the last element from DL. Throws error if list is empty."
  (when (dlist-empty-p dl)
    (error "Cant remove from empty dlist."))
  (let* ((element (%dlist-last dl))
         (prev-element (%dlist-element-previous element)))
    (when (= (%dlist-size dl) 1)
      (setf (%dlist-last dl) nil))
    (when prev-element
      (setf (%dlist-element-next prev-element) nil))
    (setf (%dlist-last dl) prev-element)
    (decf (%dlist-size dl)))
  dl)

(defmethod dlist-remove-element-front ((dl dlist))
  "Removes the first element from DL. Throws error if list is empty."
  (when (dlist-empty-p dl)
    (error "Cant remove from empty dlist."))
  (let* ((element (%dlist-first dl))
         (next-element (%dlist-element-next element)))
    (when (= (%dlist-size dl) 1)
      (setf (%dlist-last dl) nil))
    (when next-element
      (setf (%dlist-element-previous next-element) nil))
    (setf (%dlist-first dl) next-element)
    (decf (%dlist-size dl)))
  dl)

(defmethod %dlist-check-index ((dl dlist) index)
  (when (> index (%dlist-max-index dl))
    (error "Index has to be between 0 and ~a." (%dlist-max-index dl)))
  (when (< index 0)
    (error "Index has to be positive.")))

(defmethod %dlist-max-index ((dl dlist))
  (max 0
       (- (%dlist-size dl) 1)))

(defmacro %do-dlist ((node-var index-var) dlist &body body)
  `(loop :for ,node-var = (%dlist-first ,dlist) :then (%dlist-element-next ,node-var)
         :for ,index-var :from 0 :below (%dlist-size ,dlist)
         :do ,@body))

(defmethod dlist-get-elements ((dl dlist))
  "Return a list of all elements in DL."
  (let ((elements '()))
    (%do-dlist  (node i) dl
      (push (%dlist-element-value node) elements))
    (reverse elements)))


(defmethod dlist-get-element-front ((dl dlist))
  "Returns the first element in DL. Throws error if DL is empty."
  (when (dlist-empty-p dl)
    (error "Cant get first element of empty list"))
  (%dlist-element-value (%dlist-first dl)))

(defmethod dlist-get-element-end ((dl dlist))
  "Returns the last element in DL. Throws error if DL is empty."
  (when (dlist-empty-p dl)
    (error "Cant get last element of empty list"))
  (%dlist-element-value (%dlist-last dl)))

(defmethod dlist-get-element-at ((dl dlist) index)
  "Returns the element at INDEX from the DL. Throws error if INDEX < 0 or INDEX >= size of dlist."
  (%dlist-check-index dl index)
  (%dlist-element-value (%dlist-get-element-node-at dl index)))

(defmethod %dlist-get-element-node-at ((dl dlist) index)
  (%do-dlist (node i) dl
    (when (= i index)
      (return node))))

(defmethod dlist-find-element ((dl dlist) element &key (test #'eql))
  "Returns the index of the first occurence of ELEMENT in DL. NIL otherwise. TEST is used to test the equality of the elements."
  (%do-dlist (node i) dl
    (when (funcall test (%dlist-element-value node) element)
      (return-from dlist-find-element i)))
  nil)
