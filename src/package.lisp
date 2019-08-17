;;;; package.lisp

(defpackage #:clds
  (:use #:cl)
  (:export
   ;Doubly-linked-list
   #:make-dlist
   #:dlist-size
   #:dlist-empty-p
   #:dlist-add-element-at
   #:dlist-add-element-front
   #:dlist-add-element-end
   #:dlist-remove-element-at
   #:dlist-remove-element-front
   #:dlist-remove-element-end
   #:dlist-get-elements
   #:dlist-get-element-at
   #:dlist-get-element-front
   #:dlist-get-element-end
   #:dlist-find-element
   ))
