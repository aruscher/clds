(in-package #:clds-tests)

(def-suite stack-tests
  :in clds-tests)

(in-suite stack-tests)

(test stack-size-test
  (let ((s (make-stack)))
    (is (= (stack-size s) 0))
    (stack-push-element s 1)
    (is (= (stack-size s) 1))
    (stack-pop-element s)
    (is (= (stack-size s) 0))))

(test stack-empty-p-test
  (let ((s (make-stack)))
    (is (stack-empty-p s))
    (stack-push-element s 1)
    (is (not (stack-empty-p s)))
    (stack-pop-element s)
    (is (stack-empty-p s))))

(test stack-peek-element-test
  (let ((s (make-stack)))
    (signals error (stack-peek-element s))
    (stack-push-element s 1)
    (is (= (stack-peek-element s) 1))
    (stack-push-element s 2)
    (is (= (stack-peek-element s) 2))
    (stack-pop-element s)
    (is (= (stack-peek-element s) 1))
    (stack-pop-element s)
    (signals error (stack-peek-element s))))

(test stack-get-elements-test
  (let ((s (make-stack)))
    (is (equal (stack-get-elements s) '()))
    (stack-push-element s 1)
    (is (equal (stack-get-elements s) '(1)))
    (stack-push-element s 2)
    (is (equal (stack-get-elements s) '(2 1)))
    (stack-pop-element s)
    (is (equal (stack-get-elements s) '(1)))
    (stack-pop-element s)
    (is (equal (stack-get-elements s) '()))))

(test stack-push-element-test
  (let ((s (make-stack)))
    (is (equal (stack-get-elements s) '()))
    (stack-push-element s 1)
    (is (equal (stack-get-elements s) '(1)))
    (stack-push-element s 2)
    (stack-push-element s 3)
    (stack-push-element s 2)
    (stack-push-element s 4)
    (is (= (stack-size s) 5))
    (is (equal (stack-get-elements s) '(4 2 3 2 1)))))

(test stack-push-elements-test
  (let ((s (make-stack)))
    (is (equal (stack-get-elements s) '()))
    (stack-push-elements s 1 2 3 4)
    (is (= (stack-size s) 4))
    (is (equal (stack-get-elements s) '(4 3 2 1)))
    (stack-push-elements s 1 2 3 4)
    (is (= (stack-size s) 8))
    (is (equal (stack-get-elements s) '(4 3 2 1 4 3 2 1)))))


(test stack-pop-elements-test
  (let ((s (make-stack)))
    (signals error (stack-pop-element s))
    (stack-push-elements s 1 2 3 4)
    (is (= (stack-pop-element s) 4))
    (is (= (stack-pop-element s) 3))
    (is (= (stack-pop-element s) 2))
    (is (= (stack-pop-element s) 1))
    (signals error (stack-pop-element s))))
