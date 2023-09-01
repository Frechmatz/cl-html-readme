(in-package :cl-html-readme-test)

(define-test walk-tree-test-1 ()
  (let ((doc '("STR-1" "STR-2" "STR-3")))
    (let ((stringified (doc-to-string '("STR-1" "STR-2" "STR-3"))))
      (assert-equal "('STR-1' 'STR-2' 'STR-3')" stringified))))

(define-test walk-tree-test-2 ()
  (let ((doc '("STR-1" (heading (:name "HEADING-1-NAME")) "STR-2" "STR-3")))
    (let ((stringified (doc-to-string doc)))
      (assert-equal
       "('STR-1' (heading (:name 'HEADING-1-NAME')) 'STR-2' 'STR-3')"
       stringified))))

(define-test walk-tree-test-3-syntax-error ()
  (let ((doc '("STR-1" nil))
	(catched-error nil))
    (handler-case
	(cl-html-readme-dsl:walk-tree
	 doc
	 :open-element
	 (lambda(element properties content)
	   (declare (ignore element properties content))
	   nil) 
	 :close-element
	 (lambda(context)
	   (declare (ignore context))
	   nil)
	 :text
	 (lambda(str)
	   (declare (ignore str))
	   nil))
      (error (err)
	(setf catched-error err)))
    (assert-true catched-error)
    (assert-true (typep catched-error 'cl-html-readme-dsl:dsl-syntax-error))))

(define-test walk-tree-test-4-syntax-error ()
  (let ((doc '("STR-1" (mausi (:name "A")) "STR-2"))
	(catched-error nil))
    (handler-case
	(cl-html-readme-dsl:walk-tree
	 doc
	 :open-element
	 (lambda(element properties content)
	   (declare (ignore element properties content))
	   nil) 
	 :close-element
	 (lambda(context)
	   (declare (ignore context))
	   nil)
	 :text
	 (lambda(str)
	   (declare (ignore str))
	   nil))
      (error (err)
	(setf catched-error err)))
    (assert-true catched-error)
    (assert-true (typep catched-error 'cl-html-readme-dsl:dsl-syntax-error))))

(define-test walk-tree-test-5-close-element-context ()
  (let ((doc '("STR-1" (heading (:name "HEADING-1-NAME")) "STR-2"))
	(recorded nil))
    (cl-html-readme-dsl:walk-tree
     doc
     :open-element
     (lambda(element properties content)
       (declare (ignore element content))
       (push "HEADING" recorded)
       (push (getf properties :name) recorded)
       "CLOSE-CONTEXT") 
     :close-element
     (lambda(context)
       (push context recorded)
       nil)
     :text
     (lambda(str)
       (push str recorded)))
    (setf recorded (reverse recorded))
    (assert-equal "STR-1" (first recorded))
    (assert-equal "HEADING" (second recorded))
    (assert-equal "HEADING-1-NAME" (third recorded))
    (assert-equal "CLOSE-CONTEXT" (fourth recorded))
    (assert-equal "STR-2" (fifth recorded))))

(define-test walk-tree-test-6 ()
  (let ((doc '("STR-0" (heading (:name "HEADING-1-NAME") "STR-1.1" "STR-1.2"))))
    (let ((stringified (doc-to-string doc)))
      (assert-equal
       "('STR-0' (heading (:name 'HEADING-1-NAME') 'STR-1.1' 'STR-1.2'))"
       stringified))))
