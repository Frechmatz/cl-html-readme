(defpackage :cl-html-readme-make-readme-dsl-example-escape
  (:use :cl)
  (:documentation "Example: Escaping of characters"))
(in-package :cl-html-readme-make-readme-dsl-example-escape)

(defun example ()
  (let ((documentation
	  `((heading (:name "Example"))
	    ,(cl-html-readme:read-string "<b>Lorum ipsum</b>" :escape t))))
    (cl-html-readme:doc-to-html nil documentation)))

;;(example)
