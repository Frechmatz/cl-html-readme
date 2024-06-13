(defpackage :cl-html-readme-make-readme-dsl-example-semantic
  (:use :cl)
  (:documentation "Example: Semantic elements"))
(in-package :cl-html-readme-make-readme-dsl-example-semantic)

(defun example ()
  (let ((documentation
	  `((semantic (:name "header")
		      (heading (:name "Example")))
	    (semantic (:name "section")
		      (heading (:name "Chapter 1"))
		      "Lorem ipsum")
	    (semantic (:name "footer") "Lorem ipsum"))))
    (cl-html-readme:doc-to-html nil documentation)))

;;(example)
