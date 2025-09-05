(defpackage :cl-html-readme-make-readme-intro-example
  (:use :cl)
  (:documentation "Example"))
(in-package :cl-html-readme-make-readme-intro-example)

(defun hello-world ()
  "Prints <code>Hello, World</code>"
  (format t "Hello, World"))
  
(defun example ()
  (let ((documentation
	  `("<html><body>Documentation"
	    (heading
	     (:name "Table of contents")
	     (toc))
	    (heading
	     (:name "API" :toc t)
	     (heading
	      (:name "hello-world" :toc t)
	      ,(documentation #'hello-world 'function)))
	    "</body></html>")))
    (cl-html-readme:doc-to-html nil documentation)))

;;(example)
