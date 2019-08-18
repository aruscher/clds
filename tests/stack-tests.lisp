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

(test stack-peek-element-test
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


