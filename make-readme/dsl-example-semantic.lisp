(defpackage :cl-html-readme-make-readme-dsl-example-semantic (:use :cl))
(in-package :cl-html-readme-make-readme-dsl-example-semantic)

(defun example ()
  (let ((example
	  ;; A copy of dsl-example-semantic.dsl :(
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
    (let ((str (make-string-output-stream)))
      (cl-html-readme:doc-to-html str example)
      (format t "~%~a~%" example)
      ;; To be pasted into dsl-example-semantic.html and formatted via sgml-pretty-print :(
      (format t "~%~a~%" (get-output-stream-string str)))))

;;(example)
