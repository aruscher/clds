;;;; package.lisp

(defpackage #:clds
  (:use #:cl)
  (:export
   ; Doubly-linked-list
   #:make-dlist
   #:dlist-size
   #:dlist-empty-p
   #:dlist-add-element-at
   #:dlist-add-element-front
   #:dlist-add-element-end
   #:dlist-add-elements
   #:dlist-remove-element-at
   #:dlist-remove-element-front
   #:dlist-remove-element-end
   #:dlist-get-elements
   #:dlist-get-element-at
   #:dlist-get-element-front
   #:dlist-get-element-end
   #:dlist-find-element
   ; Queue
   #:make-queue
   #:queue-size
   #:queue-empty-p
   #:queue-peek-element
   #:queue-get-elements
   #:queue-enqueue-element
   #:queue-enqueue-elements
   #:queue-dequeue-element
   ; Stack
   #:make-stack
   #:stack-size
   #:stack-empty-p
   #:stack-peek-element
   #:stack-get-elements
   #:stack-push-element
   #:stack-push-elements
   #:stack-pop-element


   ))
