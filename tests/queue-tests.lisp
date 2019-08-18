(in-package #:clds-tests)

(def-suite queue-tests
  :in clds-tests)

(in-suite queue-tests)

(test queue-size-test
  (let ((q (make-queue)))
    (is (= (queue-size q) 0))
    (queue-enqueue-element q 1)
    (is (= (queue-size q) 1))
    (queue-dequeue-element q)
    (is (= (queue-size q) 0))))

(test queue-empty-p-test
  (let ((q (make-queue)))
    (is (queue-empty-p q))
    (queue-enqueue-element q 1)
    (is (not (queue-empty-p q)))
    (queue-dequeue-element q)
    (is (queue-empty-p q))))

(test queue-peek-element-test
  (let ((q (make-queue)))
    (signals error (queue-peek-element q))
    (queue-enqueue-element q 1)
    (is (= (queue-peek-element q) 1))
    (queue-enqueue-element q 2)
    (is (= (queue-peek-element q) 1))))

(test queue-get-elements-test
  (let ((q (make-queue)))
    (is (equal (queue-get-elements q) '()))
    (queue-enqueue-element q 1)
    (is (equal (queue-get-elements q) '(1)))
    (queue-enqueue-element q 2)
    (queue-enqueue-element q 3)
    (is (equal (queue-get-elements q) '(1 2 3)))
    (queue-dequeue-element q)
    (is (equal (queue-get-elements q) '(2 3)))
    (queue-dequeue-element q)
    (is (equal (queue-get-elements q) '(3)))
    (queue-dequeue-element q)
    (is (equal (queue-get-elements q) '()))))

(test queue-enqueue-element-test
  (let ((q (make-queue)))
    (queue-enqueue-element q 1)
    (is (= (queue-peek-element q) 1))
    (is (equal (queue-get-elements q) '(1)))
    (queue-enqueue-element q 2)
    (is (= (queue-peek-element q) 1))
    (is (equal (queue-get-elements q) '(1 2)))))

(test queue-enqueue-elements-test
  (let ((q (make-queue)))
    (queue-enqueue-element q 1)
    (is (= (queue-peek-element q) 1))
    (is (equal (queue-get-elements q) '(1)))
    (queue-enqueue-elements q 2 3 4 5)
    (is (equal (queue-get-elements q) '(1 2 3 4 5)))))

(test queue-dequeue-element-test
  (let ((q (make-queue)))
    (queue-enqueue-elements q 1 2 3 4)
    (is (equal (queue-get-elements q) '(1 2 3 4)))
    (is (= (queue-dequeue-element q) 1))
    (is (= (queue-dequeue-element q) 2))
    (is (= (queue-dequeue-element q) 3))
    (is (= (queue-dequeue-element q) 4))
    (signals error (queue-dequeue-element q))))

