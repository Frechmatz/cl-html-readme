(in-package :cl-html-readme-make-doc)

;;
;; Generate documentation of cl-html-readme
;; Uses the docparser library https://github.com/eudoxia0/docparser
;;

;;
;; Utility functions for metadata retrieval and HTML formatting
;;

(defun make-index (system)
  "Create a system index"
  (docparser:parse system))

(defun get-index-node (index package-name symbol-name)
  "Retrieve a node from a system index"
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
     "<b>" package-name ":" (string-downcase symbol-name) "</b>&nbsp;"
     (string-downcase (format nil "~a" (if lambda-list lambda-list "()")))
     "<p>" (docparser:node-docstring node) "</p>")))

(defun make-variable-string (index package-name symbol-name)
  "Returns HTML representation of a variable"
  (let ((node (get-index-node index package-name symbol-name)))
    (concatenate
     'string
     "<b>" package-name ":" (string-downcase symbol-name) "</b>&nbsp;"
     "<p>" (docparser:node-docstring node) "</p>")))
  
(defun make-code-string-from-file (path)
  "Returns HTML representation of a source code file"
  (concatenate
   'string
   "<p><pre><code>"
   (cl-html-readme:read-file path :replace-tabs t :escape t)
   "</code></pre></p>"))

(defun make-code-string-from-string (str)
  "Returns HTML representation of a string representing source code"
  (concatenate
   'string
   "<p><pre><code>"
   (cl-html-readme:read-string str :replace-tabs t :escape t)
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
;; Create the documentation object of cl-html-readme
;;

(defun get-readme (index)
  `("<html>"
    "<head><link href=\"cl-html-readme.css\" rel=\"stylesheet\" type=\"text/css\"/></head>"
    "<body>"
    (semantic
     (:name "header")
     (heading
      (:name "cl-html-readme"))
     ,(cl-html-readme:read-file "make-readme/snippets/introduction.html")
     "<p>The source code is available <a href=\"https://github.com/Frechmatz/cl-html-readme\">here</a>.</p>")
    (semantic
     (:name "nav")
     (heading
      (:name "Table of contents")
      (toc)))
    (semantic
     (:name "section")
     (heading
      (:name "Change-Log" :toc t)
      (heading
       (:name "Version 1.0.0")
       "<p>The first release of cl-html-readme.</p>")
      (heading
       (:name "Version 1.0.1")
       "<p><b>This version is the current quicklisp release.</b></p>"
       (heading
	(:name "Changes")
	"<ul>"
	"<li>HTML serialization inserts linebreaks for better readability and diff friendliness.</li>"
	"<li>Removed a couple of too fragile tests.</li>"
	"</ul>"))
      (heading
       (:name "Version 2.0.0")
       (heading
	(:name "Breaking changes")
	"<ul>"
	"<li>Support of HTML style and class attributes has been removed.</li></ul>")
       (heading
	(:name "Changes")
	"<ul>"
	"<li>Added read-stream and read-string to the API.</li>"
	"<li>Improved documentation.</li>"
	"<li>Increased test coverage.</li>"
	"<li>A more efficient implementation of the TOC generation.</li>"
	"</ul>")))
     (heading
      (:name "Installation" :toc t)
      ,(cl-html-readme:read-file "make-readme/snippets/installation.html"))
     (heading
      (:name "DSL" :toc t)
      ,(cl-html-readme:read-file "make-readme/snippets/dsl-introduction.html")
      (heading
       (:name "Examples")
       (heading
	(:name "Table of contents" :toc t)
	,(make-code-string-from-file "make-readme/examples/toc.lisp")
	(heading
	 (:name "Generated HTML")
	 ,(make-code-string-from-string
	   (cl-html-readme-make-readme-dsl-example-toc::example))))
       (heading
	(:name "Semantic elements" :toc t)
	,(make-code-string-from-file "make-readme/examples/semantic.lisp")
	(heading
	 (:name "Generated HTML")
	 ,(make-code-string-from-string
	   (cl-html-readme-make-readme-dsl-example-semantic::example))))))
     (heading
      (:name "API" :toc t)
      (heading
       (:name "*home-directory*" :toc t)
       ,(make-variable-string index "cl-html-readme" "*home-directory*"))
      (heading
       (:name "*tab-width*" :toc t)
       ,(make-variable-string index "cl-html-readme" "*tab-width*"))
      (heading
       (:name "doc-to-html" :toc t)
       ,(make-function-string index "cl-html-readme" "doc-to-html"))
      (heading
       (:name "make-path" :toc t)
       ,(make-function-string index "cl-html-readme" "make-path"))
      (heading
       (:name "read-string" :toc t)
       ,(make-function-string index "cl-html-readme" "read-string")
       (heading
	(:name "Example")
	,(make-code-string-from-file "make-readme/examples/escape.lisp")
	(heading
	 (:name "Generated HTML")
	 ,(make-code-string-from-string
	   (cl-html-readme-make-readme-dsl-example-escape::example)))))
      (heading
       (:name "read-file" :toc t)
       ,(make-function-string index "cl-html-readme" "read-file"))
      (heading
       (:name "read-stream" :toc t)
       ,(make-function-string index "cl-html-readme" "read-stream")))
     (heading
      (:name "Run tests" :toc t)
      ,(make-code-string-from-string "(asdf:test-system :cl-html-readme)"))
     (heading
      (:name "Generate documentation" :toc t)
      ,(make-code-string-from-file "make-readme/snippets/generate-doc.lisp")))
    (semantic
     (:name "footer")
     "<hr/><p><small>Generated " ,(now) "</small></p>")
    "</body></html>"))

;;
;; Generate the documentation
;;

(defun make-doc ()
  (let ((index (make-index :cl-html-readme)))
    (let ((cl-html-readme:*home-directory* (asdf:system-source-directory :cl-html-readme))
	  (cl-html-readme:*tab-width* 2))
      (with-open-file
	  (fh (cl-html-readme:make-path "docs/index.html")
	      :direction :output
	      :if-exists :supersede
	      :if-does-not-exist :create
	      :external-format :utf-8)
	(cl-html-readme:doc-to-html fh (get-readme index)))))
    "DONE")

;;(make-doc)

