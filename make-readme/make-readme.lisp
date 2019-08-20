(in-package :cl-readme-make-readme)

(defun write-html ()
  (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-readme/")
	(cl-readme:*tab-width* 8))
    (let ((docstr (concatenate
                   'string
                   "<html><body>"
                   "<h1>cl-readme</h1>"
		   (read-text-file "make-readme/introduction.html")
		   "<h2>Installation</h2>"
		   (read-text-file "make-readme/installation.html")
		   "<h2>Example</h2>"
		   "<h3>Generate the HTML file</h3>"
                   (example-code "make-readme/make-readme.lisp" :omit-header t)
		   "<h3>Convert the HTML file to Markdown</h3>"
                   (read-text-file "make-readme/html2md.html")
                   "<h2>API</h2>"
                   (make-variable-string 'cl-readme:*home-directory*)
                   (make-variable-string 'cl-readme:*tab-width*)
                   (make-function-string 'cl-readme:make-function-string)
                   (make-function-string 'cl-readme:make-condition-string)
                   (make-function-string 'cl-readme:make-variable-string)
                   (make-function-string 'cl-readme:read-text-file)
                   (make-function-string 'cl-readme:example-code)
                   (make-function-string 'cl-readme:current-date)
                   (make-function-string 'cl-readme:make-path)
                   "<hr/><p><small>Generated " (current-date) "</small></p>"
                   "</body></html>")))
      (with-open-file (fh (make-path "make-readme/generated/readme.html")
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
        (format fh "~a" docstr)))))

;;(write-html)

