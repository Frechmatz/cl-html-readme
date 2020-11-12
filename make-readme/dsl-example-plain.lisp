(defpackage :cl-html-readme-make-readme-dsl-example-plain (:use :cl))
(in-package :cl-html-readme-make-readme-dsl-example-plain)

(defun example ()
  (let ((example
	  `((heading (:name "Example"))
	    (heading (:name "Table of contents")(toc))
	    (heading (:name "Chapter 1" :toc t)) "Lorem ipsum"
	    (heading (:name "Chapter 2" :toc t)) "Lorem ipsum")))
    (let ((str (make-string-output-stream)))
      (cl-html-readme:doc-to-html str example)
      (format t "~%~a~%" example)
      (format t "~%~a~%" (get-output-stream-string str)))))

;;(example)
