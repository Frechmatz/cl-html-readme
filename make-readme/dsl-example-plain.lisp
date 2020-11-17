(defpackage :cl-html-readme-make-readme-dsl-example-plain (:use :cl))
(in-package :cl-html-readme-make-readme-dsl-example-plain)

(defun example ()
  (let ((example
	  ;; A copy of dsl-example-plain.dsl :(
	  `((heading (:name "Example"))
	    (heading (:name "Table of contents")(toc))
	    (heading (:name "Chapter 1" :toc t)) "Lorem ipsum"
	    (heading (:name "Chapter 2" :toc t)) "Lorem ipsum")))
    (let ((str (make-string-output-stream)))
      (cl-html-readme:doc-to-html str example)
      (format t "~%~a~%" example)
      ;; To be pasted into dsl-example-plain.html and formatted via sgml-pretty-print :(
      (format t "~%~a~%" (get-output-stream-string str)))))

;;(example)
