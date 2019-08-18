;;;; clds.asd

(asdf:defsystem #:clds
  :description "Describe clds here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :serial t
                :components
                ((:file "package")
                 (:file "doubly-linked-list")
                 (:file "queue")
                 (:file "stack")
                 (:file "clds")))))
