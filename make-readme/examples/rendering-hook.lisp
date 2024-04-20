(defpackage :cl-html-readme-make-readme-example-rendering-hook
  (:use :cl)
  (:documentation "Example: Rendering hook"))
(in-package :cl-html-readme-make-readme-example-rendering-hook)

(defun example ()
  (let ((documentation
	  `((heading (:name "Header" :toc t :app (:class "heading-class"))))))
    (let ((cl-html-readme:*get-heading-attributes*
	    (lambda (properties)
	      (list
	       :class (getf (getf properties :app) :class)
	       :a nil
	       :b (list "")
	       :c ""))))
      (cl-html-readme:doc-to-html nil documentation))))

;;(example)
