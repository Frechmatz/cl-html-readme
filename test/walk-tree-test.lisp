(in-package :cl-readme-test)

(define-test walk-tree-test-1 ()
	     (let ((doc '("STR-1" "STR-2" "STR-3"))
		   (recorded nil))
	       (cl-readme-dsl:walk-tree
		doc
		:open-element
		(lambda(element properties)
		  (declare (ignore properties))
		  (push element recorded) nil) 
		:close-element
		(lambda(context)
		  (declare (ignore context))
		  nil)
		:text
		(lambda(str)
		  (push str recorded)))
	       (setf recorded (reverse recorded))
	       (assert-equal "STR-1" (first recorded))
	       (assert-equal "STR-2" (second recorded))
	       (assert-equal "STR-3" (third recorded))))


(define-test walk-tree-test-2 ()
	     (let ((doc '("STR-1" (heading (:name "HEADING-1-NAME")) "STR-2" "STR-3"))
		   (recorded nil))
	       (cl-readme-dsl:walk-tree
		doc
		:open-element
		(lambda(element properties)
		  (declare (ignore element))
		  (push "HEADING" recorded)
		  (push (getf properties :name) recorded)
		  nil) 
		:close-element
		(lambda(context)
		  (declare (ignore context))
		  nil)
		:text
		(lambda(str)
		  (push str recorded)))
	       (setf recorded (reverse recorded))
	       (assert-equal "STR-1" (first recorded))
	       (assert-equal "HEADING" (second recorded))
	       (assert-equal "HEADING-1-NAME" (third recorded))
	       (assert-equal "STR-2" (fourth recorded))
	       (assert-equal "STR-3" (fifth recorded))))


(define-test walk-tree-test-3 ()
	     (let ((doc '("STR-1" nil))
		   (catched-error nil))
	       (handler-case
		   (cl-readme-dsl:walk-tree
		    doc
		    :open-element
		    (lambda(element properties)
		      (declare (ignore element properties))
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
	       (assert-true (typep catched-error 'cl-readme-dsl:dsl-syntax-error))))

(define-test walk-tree-test-4 ()
	     (let ((doc '("STR-1" (mausi (:name "A")) "STR-2"))
		   (catched-error nil))
	       (handler-case
		   (cl-readme-dsl:walk-tree
		    doc
		    :open-element
		    (lambda(element properties)
		      (declare (ignore element properties))
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
	       (assert-true (typep catched-error 'cl-readme-dsl:dsl-syntax-error))))

(define-test walk-tree-test-5 ()
	     (let ((doc '("STR-1" (heading (:name "HEADING-1-NAME")) "STR-2"))
		   (recorded nil))
	       (cl-readme-dsl:walk-tree
		doc
		:open-element
		(lambda(element properties)
		  (declare (ignore element))
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
