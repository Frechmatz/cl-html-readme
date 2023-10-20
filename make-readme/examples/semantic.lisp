(defpackage :cl-html-readme-make-readme-dsl-example-semantic
  (:use :cl)
  (:documentation "Example: Semantic elements"))
(in-package :cl-html-readme-make-readme-dsl-example-semantic)

(defun example ()
  (let ((documentation
	  `((semantic (:name "header")
		      (heading (:name "Example")))
	    (semantic (:name "nav")
		      (heading (:name "Table of contents")
			       (toc)))
	    (semantic (:name "section")
		      (heading (:name "Chapter 1" :toc t))
		      "Lorem ipsum"
		      (heading (:name "Chapter 2" :toc t))
		      "<b>Lorem ipsum</b>")
	    (semantic (:name "footer") "Lorem ipsum"))))
    (cl-html-readme:doc-to-html nil documentation)))

;;(example)
