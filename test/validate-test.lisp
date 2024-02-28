(in-package :cl-html-readme-test)

;;
;; Test validation of document objects
;;

(define-test validate-test-1 ()
  "All good"
  (let ((doc '("TEXT-1" "TEXT-2"))
	(catched-error nil))
    (handler-case
	(cl-html-readme-public-dsl:validate doc)
      (error (err)
	(setf catched-error err)))
    (assert-false catched-error)))

(define-test validate-test-2 ()
  "All good"
  (let ((doc '("TEXT-1" "TEXT-2" (heading (:name "H1"))))
	(catched-error nil))
    (handler-case
	(cl-html-readme-public-dsl:validate doc)
      (error (err)
	(setf catched-error err)))
    (assert-false catched-error)))

(define-test validate-test-3 ()
  "Text must not be nil"
  (let ((doc '("TEXT-1" nil))
	(catched-error nil))
    (handler-case
	(cl-html-readme-public-dsl:validate doc)
      (error (err)
	(setf catched-error err)))
    (assert-true catched-error)
    (assert-true (typep catched-error 'cl-html-readme:syntax-error))))

(define-test validate-test-4 ()
  "Unknown special form of DSL"
  (let ((doc '("TEXT-1" (some-undefined-form (:name "form")) "TEXT-2"))
	(catched-error nil))
    (handler-case
	(cl-html-readme-public-dsl:validate doc)
      (error (err)
	(setf catched-error err)))
    (assert-true catched-error)
    (assert-true (typep catched-error 'cl-html-readme:syntax-error))))

(define-test validate-test-5 ()
  "Text cannot be a keyword"
  (let ((doc '("TEXT-1" :keyword))
	(catched-error nil))
    (handler-case
	(cl-html-readme-public-dsl:validate doc)
      (error (err)
	(setf catched-error err)))
    (assert-true catched-error)
    (assert-true (typep catched-error 'cl-html-readme:syntax-error))))

(define-test validate-test-6 ()
  "Missing mandatory :name property"
  (let ((doc '("TEXT-1" (heading ()) "TEXT-2"))
	(catched-error nil))
    (handler-case
	(cl-html-readme-public-dsl:validate doc)
      (error (err)
	(setf catched-error err)))
    (assert-true catched-error)
    (assert-true (typep catched-error 'cl-html-readme:syntax-error))))
