(defpackage :cl-html-readme-make-readme-dsl-example-plain
  (:use :cl)
  (:documentation "Plain example"))
(in-package :cl-html-readme-make-readme-dsl-example-plain)

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
		      "Lorem ipsum")
	    (semantic (:name "footer") "Lorem ipsum"))))
    (cl-html-readme:doc-to-html nil documentation)))

;;(example)
