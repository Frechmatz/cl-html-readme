(in-package :cl-readme-make-readme)

(defun write-html ()
  (let ((cl-readme:*HOME-DIRECTORY* "/Users/olli/src/lisp/cl-readme/"))
    (let ((docstr (concatenate
		   'string
		   "<html><body>"
		   "<h1>cl-readme</h1>"
		   "Readme generation utilities for my Common Lisp projects."
		   "<h2>API</h2>"
		   (make-variable-string 'cl-readme:*HOME-DIRECTORY* :append-separator t)
		   (make-function-string 'cl-readme:make-function-string :append-separator t)
		   (make-function-string 'cl-readme:make-condition-string :append-separator t)
		   (make-function-string 'cl-readme:read-text-file :append-separator t)
		   (make-function-string 'cl-readme:example-code :append-separator t)
		   (make-function-string 'cl-readme:current-date :append-separator t)
		   (make-function-string 'cl-readme:make-path :append-separator nil)
		   ;;"<h2>Example</h2>"
		   ;; Including this code as an example does not work because it contains HTML markup.
		   ;;(example-code "make-readme/make-readme.lisp")
		   "<hr/><p><small>Generated " (current-date) "</small></p>"
		   "</body></html>")))
      (with-open-file (fh (make-path "make-readme/generated/readme.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(format fh "~a" docstr)))))

;;(write-html)

