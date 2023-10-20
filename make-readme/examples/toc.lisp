(defpackage :cl-html-readme-make-readme-dsl-example-toc
  (:use :cl)
  (:documentation "Example: Table of contents"))
(in-package :cl-html-readme-make-readme-dsl-example-toc)

(defun example ()
  (let ((documentation
	  `((heading (:name "Example"))
	    (heading (:name "Table of contents")
		     (toc))
	    (heading (:name "Chapter 1" :toc t)) "Lorem ipsum"
	    (heading (:name "Chapter 2" :toc t) "Lorem ipsum"
		     (heading (:name "Chapter 2.1") "Lorem ipsum")))))
    (cl-html-readme:doc-to-html nil documentation)))

;;(example)
