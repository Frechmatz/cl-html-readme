(in-package :cl-html-readme-make-readme)

;;
;; Helper functions
;;

(defun create-index (system)
  (docparser:parse system))

(defun get-index-node (index package-name symbol-name)
  (aref (docparser:query
	 index
	 :package-name (string-upcase package-name)
	 :symbol-name (string-upcase symbol-name))
	0))
  
(defun make-function-string (index package-name symbol-name)
  "Returns HTML representation of a function"
  (let* ((node (get-index-node index package-name symbol-name))
	 (lambda-list (docparser:operator-lambda-list node)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>&nbsp;"
     (string-downcase (format nil "~a" (if lambda-list lambda-list "()")))
     "<p>" (docparser:node-docstring node) "</p>")))

(defun make-variable-string (index package-name symbol-name)
  "Returns HTML representation of a variable"
  (let ((node (get-index-node index package-name symbol-name)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>"
     "<p>" (docparser:node-docstring node) "</p>")))
  
(defun make-code-string (path)
  "Returns HTML representation of a source code file"
  (concatenate
   'string
   "<p><pre><code>"
   (cl-html-readme:read-file path :replace-tabs t :escape t)
   "</code></pre></p>"))

(defun now ()
  "Returns a string representing the current date and time."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    ;; 2018-09-19 21:28:16
    (let ((str (format nil "~4,'0d-~2,'0d-~2,'0d  ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec)))
      str)))

;;
;; Readme
;;

(defun get-readme (index)
    `("<html><body>"
      (semantic (:name "header")
		(heading (:name "cl-html-readme"))
		,(cl-html-readme:read-file "make-readme/introduction.html")
		"<p>The source code is available <a href=\"https://github.com/Frechmatz/cl-html-readme\">here</a>.</p>")
      (semantic (:name "nav")
		(heading (:name "Table of contents")
			 (toc)))
      (semantic (:name "section")
		(heading (:name "Installation" :toc t)
			 ,(cl-html-readme:read-file "make-readme/installation.html"))
		(heading (:name "DSL" :toc t)
			 "<p>The DSL (Domain Specific Language) of cl-html-readme shall make it easy
to define an automatically generated HTML skeleton consisting of semantic elements, heading elements and
a table of contents. Beside these predefined elements the actual \"content\" consists of plain strings.</p><p>The following examples shall demonstrate the concept.</p>"
			 (heading (:name "Example: Heading elements and Table of Contents" :toc t)
				  (heading (:name "DSL")
					   ,(make-code-string "make-readme/dsl-example-plain.dsl"))
				  (heading (:name "Generated HTML")
					   ,(make-code-string "make-readme/dsl-example-plain.html")))
			 (heading (:name "Example: Semantic elements" :toc t)
				  (heading (:name "DSL")
					   ,(make-code-string "make-readme/dsl-example-semantic.dsl"))
				  (heading (:name "Generated HTML")
					   ,(make-code-string "make-readme/dsl-example-semantic.html")))
			 (heading (:name "Example: HTML Classes and Styles" :toc t)
				  (heading (:name "DSL")
					   ,(make-code-string "make-readme/dsl-example-styling.dsl"))
				  (heading (:name "Generated HTML")
					   ,(make-code-string "make-readme/dsl-example-styling.html"))))
		(heading (:name "API":toc t)
			 ,(make-function-string index "cl-html-readme" "doc-to-html")
			 ,(make-variable-string index "cl-html-readme" "*home-directory*")
			 ,(make-variable-string index "cl-html-readme" "*tab-width*")
			 ,(make-function-string index "cl-html-readme" "make-path")
			 ,(make-function-string index "cl-html-readme" "read-file"))
		(heading (:name "Example: Readme Generation" :toc t)
			 "<p>This library was not just built for the fun of it, but for easy generation of documentation using HTML as markup language. The following example shows how the documentation of cl-html-readme is generated. 
For extraction of metadata such as docstrings the <a href=\"https://github.com/eudoxia0/docparser\">docparser</a> library is being used.</p>"
			 ,(make-code-string "make-readme/make-readme.lisp")))
      (semantic (:name "footer")
		"<hr/><p><small>Generated " ,(now) "</small></p>")
      "</body></html>"))

;;
;; Generate readme
;;

(defun make-readme ()
  (let ((index (create-index :cl-html-readme)))
    (let ((cl-html-readme:*home-directory* (asdf:system-source-directory :cl-html-readme-make-readme)))
      (with-open-file (fh (cl-html-readme:make-path "docs/index.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(cl-html-readme:doc-to-html fh (get-readme index)))))
    "DONE")

;;(make-readme)

