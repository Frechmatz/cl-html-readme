(in-package :cl-readme-make-readme)

;;
;; Formatting functions
;;

(defun make-function-string (f)
  (concatenate
   'string
   "<b>" (string-downcase (symbol-name f)) "</b>&nbsp;"
   (cl-readme:sbcl-make-function-lambda-list-str f) 
   "<p>" (documentation f 'function) "</p>"))

(defun make-variable-string (v)
  (concatenate
   'string
   "<b>" (string-downcase  (symbol-name v)) "</b>"
   "<p>" (documentation v 'variable) "</p>"))

(defun read-code-file (path)
  (concatenate
   'string
   "<p><pre><code>"
   (cl-readme:read-file path :replace-tabs t :escape t)
   "</code></pre></p>"))

;;
;; Readme
;;

(defun get-readme ()
  `("<html><body>"
    (semantic (:name "header")
	      (heading (:name "cl-readme"))
	      ,(cl-readme:read-file "make-readme/introduction.html")
	      "<p>The source code is available <a href=\"https://github.com/Frechmatz/cl-readme\">here</a>.</p>")
    (semantic (:name "nav")
	      (heading (:name "Table of contents")
		       (toc)))
    (semantic (:name "section")
	      (heading (:name "Installation" :toc t)
		       ,(cl-readme:read-file "make-readme/installation.html"))
	      (heading (:name "Example" :toc t)
		       ,(read-code-file "make-readme/make-readme.lisp"))
	      (heading (:name "API":toc t)
		       ,(make-variable-string 'cl-readme:*home-directory*)
		       ,(make-variable-string 'cl-readme:*tab-width*)
		       ,(make-variable-string 'cl-readme:*get-heading-class*)
		       ,(make-variable-string 'cl-readme:*get-toc-container-class*)
		       ,(make-variable-string 'cl-readme:*get-toc-item-class*)
		       ,(make-function-string 'cl-readme:read-file)
		       ,(make-function-string 'cl-readme:read-verbatim)
		       ,(make-function-string 'cl-readme:read-code)
		       ,(make-function-string 'cl-readme:make-path)
		       ,(make-function-string 'cl-readme:current-date)
		       ,(make-function-string 'cl-readme:sbcl-make-function-lambda-list-str)
		       ,(make-function-string 'cl-readme:sbcl-make-function-decl)))
    (semantic (:name "footer")
	      "<hr/><p><small>Generated " ,(cl-readme:current-date) "</small></p>")
    "</body></html>"))

;;
;; Generate readme
;;

(defun make-readme ()
  (let ((cl-readme:*home-directory* (asdf:system-source-directory :cl-readme-make-readme)))
    (with-open-file (fh (cl-readme:make-path "docs/index.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (cl-readme:doc-to-html fh (get-readme))))
  "DONE")

;;(make-readme)

