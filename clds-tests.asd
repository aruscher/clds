;;;; clds.asd

(asdf:defsystem #:clds-tests
  :description "Describe clds here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on ("clds" "fiveam")
  :components ((:module "tests"
                :serial t
                :components
                ((:file "package")
                 (:file "clds-tests")
                 (:file "doubly-linked-list-tests")
                 (:file "queue-tests")
                 (:file "stack-tests")))))
