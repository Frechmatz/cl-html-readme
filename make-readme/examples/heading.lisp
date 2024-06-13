(defpackage :cl-html-readme-make-readme-dsl-example-heading
  (:use :cl)
  (:documentation "Example: Heading elements"))
(in-package :cl-html-readme-make-readme-dsl-example-heading)

(defun example ()
  (let ((documentation
	  `((heading (:name "Example")
		     "Lorem ipsum"
		     (heading (:name "Chapter 1"))
		     "Lorem ipsum"
		     (heading (:name "Chapter 2"))
		     "<b>Lorem ipsum</b>"))))
    (cl-html-readme:doc-to-html nil documentation)))

;;(example)
