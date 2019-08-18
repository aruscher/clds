(in-package #:clds-tests)

(def-suite doubly-linked-list-tests
  :in clds-tests)

(in-suite doubly-linked-list-tests)

(test dlist-add-element-at-test
  (let ((dl (make-dlist)))
    (is (dlist-empty-p dl))
    (is (= (dlist-size dl) 0))
    (dlist-add-element-at dl 1 0)
    (is (not (dlist-empty-p dl)))
    (is (= (dlist-size dl) 1))
    (is (equal (dlist-get-elements dl) '(1)))
    (is (= (dlist-get-element-at dl 0) 1)))
  (let ((dl (make-dlist)))
    (dlist-add-element-at dl 2 0)
    (dlist-add-element-at dl 1 0)
    (is (equal (dlist-get-elements dl) '(1 2)))
    (dlist-add-element-at dl 3 1)
    (is (equal (dlist-get-elements dl) '(1 3 2)))
    (dlist-add-element-at dl 4 1)
    (is (equal (dlist-get-elements dl) '(1 4 3 2)))
    (is (= (dlist-size dl) 4)))
  (let ((dl (make-dlist)))
    (signals error (dlist-add-element-at dl 42 2))
    (signals error (dlist-add-element-at dl 42 -1))))

(test dlist-add-element-front-test
  (let ((dl (make-dlist)))
    (dlist-add-element-front dl 1)
    (is (equal (dlist-get-elements dl) '(1)))
    (is (= (dlist-size dl) 1))
    (dlist-add-element-front dl 2)
    (is (equal (dlist-get-elements dl) '(2 1)))
    (is (= (dlist-size dl) 2))
    (dlist-add-element-front dl 3)
    (is (equal (dlist-get-elements dl) '(3 2 1)))))

(test dlist-add-element-end-test
  (let ((dl (make-dlist)))
    (dlist-add-element-end dl 1)
    (is (equal (dlist-get-elements dl) '(1)))
    (is (= (dlist-size dl) 1))
    (dlist-add-element-end dl 2)
    (is (equal (dlist-get-elements dl) '(1 2)))
    (is (= (dlist-size dl) 2))
    (dlist-add-element-end dl 3)
    (is (equal (dlist-get-elements dl) '(1 2 3)))))

(test dlist-add-elements-test
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 2 3)
    (is (equal (dlist-get-elements dl) '(1 2 3)))
    (is (= (dlist-size dl) 3))))

(test dlist-remove-element-at-test
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 2 3 4)
    (is (equal (dlist-get-elements dl) '(1 2 3 4)))
    (dlist-remove-element-at dl 0)
    (is (equal (dlist-get-elements dl) '(2 3 4)))
    (is (= (dlist-size dl) 3))
    (dlist-remove-element-at dl 1)
    (is (equal (dlist-get-elements dl) '(2 4)))
    (is (= (dlist-size dl) 2))
    (dlist-remove-element-at dl 1)
    (is (equal (dlist-get-elements dl) '(2)))
    (is (= (dlist-size dl) 1)))
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 2 3 4)
    (signals error (dlist-remove-element-at dl -1))
    (signals error (dlist-remove-element-at dl 5)))
  (let ((dl (make-dlist)))
    (signals error (dlist-remove-element-at dl 0))))

(test dlist-remove-element-front-test
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 2 3 4)
    (is (equal (dlist-get-elements dl) '(1 2 3 4)))
    (dlist-remove-element-front dl)
    (is (equal (dlist-get-elements dl) '(2 3 4)))
    (dlist-remove-element-front dl)
    (is (equal (dlist-get-elements dl) '(3 4)))
    (dlist-remove-element-front dl)
    (is (equal (dlist-get-elements dl) '(4)))
    (dlist-remove-element-front dl)
    (is (equal (dlist-get-elements dl) '()))
    (is (dlist-empty-p dl))
    (is (= (dlist-size dl) 0))
    (signals error (dlist-remove-element-front dl))))

(test dlist-remove-element-end-test
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 2 3 4)
    (is (equal (dlist-get-elements dl) '(1 2 3 4)))
    (dlist-remove-element-end dl)
    (is (equal (dlist-get-elements dl) '(1 2 3)))
    (dlist-remove-element-end dl)
    (is (equal (dlist-get-elements dl) '(1 2)))
    (dlist-remove-element-end dl)
    (is (equal (dlist-get-elements dl) '(1)))
    (dlist-remove-element-end dl)
    (is (equal (dlist-get-elements dl) '()))
    (is (dlist-empty-p dl))
    (is (= (dlist-size dl) 0))
    (signals error (dlist-remove-element-end dl))))

(test dlist-get-elements-test
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 2 3 4)
    (is (equal (dlist-get-elements dl) '(1 2 3 4))))
  (let ((dl (make-dlist)))
    (is (equal (dlist-get-elements dl) '()))))

(test dlist-get-element-at-test
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 2 3 4)
    (is (= (dlist-get-element-at dl 0) 1))
    (is (= (dlist-get-element-at dl 1) 2))
    (is (= (dlist-get-element-at dl 3) 4)))
  (let ((dl (make-dlist)))
    (signals error (dlist-get-element-at dl 0))
    (signals error (dlist-get-element-at dl -1))
    (signals error (dlist-get-element-at dl 10))))

(test dlist-get-element-front-test
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 2 3 4)
    (is (= (dlist-get-element-front dl) 1))
    (dlist-remove-element-front dl)
    (is (= (dlist-get-element-front dl) 2))
    (dlist-remove-element-front dl)
    (is (= (dlist-get-element-front dl) 3)))
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1)
    (is (= (dlist-get-element-front dl) 1))
    (is (= (dlist-get-element-front dl)
           (dlist-get-element-end dl))))
  (let ((dl (make-dlist)))
    (signals error (dlist-get-element-front dl))))

(test dlist-get-element-end-test
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 2 3 4)
    (is (= (dlist-get-element-end dl) 4))
    (dlist-remove-element-end dl)
    (is (= (dlist-get-element-end dl) 3))
    (dlist-remove-element-end dl)
    (is (= (dlist-get-element-end dl) 2)))
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1)
    (is (= (dlist-get-element-end dl) 1))
    (is (= (dlist-get-element-front dl)
           (dlist-get-element-end dl))))
  (let ((dl (make-dlist)))
    (signals error (dlist-get-element-end dl))))

(test dlist-find-element-test
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 2 3 4)
    (is (= (dlist-find-element dl 1) 0))
    (is (= (dlist-find-element dl 3) 2)))
  (let ((dl (make-dlist)))
    (is (null (dlist-find-element dl 1))))
  (let ((dl (make-dlist)))
    (dlist-add-elements dl 1 3 3 4)
    (is (= (dlist-find-element dl 3) 1))))
