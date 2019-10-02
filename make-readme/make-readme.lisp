(in-package :cl-readme-make-readme)

;;
;; Helper functions
;;

(defun make-function-string (f)
  (concatenate
   'string
   "<p>"
   (cl-readme:sbcl-make-function-decl f)
   "</p><p>"
   (documentation f 'function)
   "</p>"))

(defun make-variable-string (v)
  (concatenate
   'string
   "<b>" (string-downcase (package-name (symbol-package v))) ":" (string-downcase  (symbol-name v)) "</b>"
   "<p>"
   (documentation v 'variable)
   "</p>"))

;;
;; Documentation
;;

(defun get-documentation ()
  "Returns the documentation as a list using the DSL."
  (let ((documentation
	 `("<html><body>"
	   (semantic (:name "header")
		     (heading (:name "cl-readme"))
		     ,(cl-readme:read-verbatim "make-readme/introduction.html"))
	   (semantic (:name "nav")
		     (heading (:name "Table of contents")
			      TOC))
	   (semantic (:name "section")
		     (heading (:name "Installation" :toc t)
			      ,(cl-readme:read-verbatim "make-readme/installation.html"))
		     (heading (:name "Example" :toc t)
			      ,(cl-readme:read-code "make-readme/make-readme.lisp"))
		     (heading (:name "API":toc t)
			      ,(make-variable-string 'cl-readme:*home-directory*)
			      ,(make-variable-string 'cl-readme:*tab-width*)
			      ,(make-function-string 'cl-readme:read-verbatim)
			      ,(make-function-string 'cl-readme:read-code)
			      ,(make-function-string 'cl-readme:make-path)
			      ,(make-function-string 'cl-readme:current-date)
			      ,(make-function-string 'cl-readme:sbcl-make-function-decl)))
	   (semantic (:name "footer")
		     "<hr/><p><small>Generated " ,(cl-readme:current-date) "</small></p>")
	   "</body></html>")))
    documentation))

;;
;; Generate HTML file
;;

(defclass cl-readme-readme-writer (cl-readme:html-writer) ())

;; Override open-semantic in order to add a class attribute
(defmethod cl-readme:open-semantic ((writer cl-readme-readme-writer) semantic-element-settings)
  (format nil "<~a class=\"container\">" (getf semantic-element-settings :name)))

(defun make-readme ()
  (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-readme/")
	(cl-readme:*tab-width* 8))
    (with-open-file (fh (cl-readme:make-path "docs/index.html")
			:direction :output
			:if-exists :supersede
			:if-does-not-exist :create
			:external-format :utf-8)
      (let ((w (make-instance 'cl-readme-readme-writer)))
	(cl-readme:doc-to-html w fh (get-documentation))))
  "DONE"))

;;(make-readme)

