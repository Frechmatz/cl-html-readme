(in-package :cl-html-readme-test)

(define-test test-plist-util-1 ()
  (let ((result nil))
    (cl-html-readme-plist-util:with-properties
	(list :a "AA" :b "BB")
      (lambda (key value)
	(push key result)
	(push value result)))
    (assert-equal 4 (length result))
    (setf result (reverse result))
    (assert-equal :a (first result))
    (assert-equal "AA" (second result))
    (assert-equal :b (third result))
    (assert-equal "BB" (fourth result))))

(define-test test-plist-util-2 ()
  (let ((result
	  (cl-html-readme-plist-util:filter-properties
	   (list :a "AA" :b "BB")
	   (lambda (key) (eq key :a)))))
    (assert-equal 2 (length result))
    (assert-equal :a (first result))
    (assert-equal "AA" (second result))))
