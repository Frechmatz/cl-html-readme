(in-package :cl-html-readme-test)

;;
;; Test validation of document objects
;;

(defun validation-test-try-walk-public-dsl (doc)
  "Helper function to try to traverse a documentation object following the syntax of the public DSL"
  (cl-html-readme-base-dsl:walk-tree-ng
   (cl-html-readme-public-dsl:instance)
   doc
   :open-form-handler
   (lambda(form-symbol form-properties content)
     (declare (ignore form-symbol form-properties content))
     nil)
   :close-form-handler
   (lambda (context)
     (declare (ignore context))
     nil)
   :text-handler
   (lambda (text)
     (declare (ignore text))
     nil)))

(define-test validation-test-1 ()
  "All good"
  (let ((doc '("TEXT-1" "TEXT-2"))
	(catched-error nil))
    (handler-case
	(validation-test-try-walk-public-dsl doc)
      (error (err)
	(setf catched-error err)))
    (assert-false catched-error)))

(define-test validation-test-2 ()
  "All good"
  (let ((doc '("TEXT-1" "TEXT-2" (heading (:name "H1"))))
	(catched-error nil))
    (handler-case
	(validation-test-try-walk-public-dsl doc)
      (error (err)
	(setf catched-error err)))
    (assert-false catched-error)))

(define-test validation-test-3 ()
  "Text must not be nil"
  (let ((doc '("TEXT-1" nil))
	(catched-error nil))
    (handler-case
	(validation-test-try-walk-public-dsl doc)
      (error (err)
	(setf catched-error err)))
    (assert-true catched-error)
    (assert-true (typep catched-error 'cl-html-readme-base-dsl:syntax-error))))

(define-test validation-test-4 ()
  "Unknown special form of DSL"
  (let ((doc '("TEXT-1" (some-undefined-form (:name "form")) "TEXT-2"))
	(catched-error nil))
    (handler-case
	(validation-test-try-walk-public-dsl doc)
      (error (err)
	(setf catched-error err)))
    (assert-true catched-error)
    (assert-true (typep catched-error 'cl-html-readme-base-dsl:syntax-error))))

(define-test validation-test-5 ()
  "Text cannot be a keyword"
  (let ((doc '("TEXT-1" :keyword))
	(catched-error nil))
    (handler-case
	(validation-test-try-walk-public-dsl doc)
      (error (err)
	(setf catched-error err)))
    (assert-true catched-error)
    (assert-true (typep catched-error 'cl-html-readme-base-dsl:syntax-error))))

(define-test validation-test-6 ()
  "Missing mandatory :name property"
  (let ((doc '("TEXT-1" (heading ()) "TEXT-2"))
	(catched-error nil))
    (handler-case
	(validation-test-try-walk-public-dsl doc)
      (error (err)
	(setf catched-error err)))
    (assert-true catched-error)
    (assert-true (typep catched-error 'cl-html-readme-base-dsl:syntax-error))))
