(in-package :cl-readme-make-readme)

;;
;; Helper functions
;;

(defun get-node (index package-name symbol-name)
  (aref (docparser:query
	 index
	 :package-name (string-upcase package-name)
	 :symbol-name (string-upcase symbol-name))
	0))
  
(defun make-function-string (index package-name symbol-name)
  (let* ((node (get-node index package-name symbol-name))
	 (lambda-list (docparser:operator-lambda-list node)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>&nbsp;"
     (string-downcase (format nil "~a" (if lambda-list lambda-list "()")))
     "<p>" (docparser:node-docstring node) "</p>")))

(defun make-variable-string (index package-name symbol-name)
  (let ((node (get-node index package-name symbol-name)))
    (concatenate
     'string
     "<b>" (string-downcase symbol-name) "</b>"
     "<p>" (docparser:node-docstring node) "</p>")))
  
(defun make-code-string (path)
  (concatenate
   'string
   "<p><pre><code>"
   (cl-readme:read-file path :replace-tabs t :escape t)
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
			 ,(make-code-string "make-readme/make-readme.lisp"))
		(heading (:name "API":toc t)
			 ,(make-variable-string index "cl-readme" "*home-directory*")
			 ,(make-variable-string index "cl-readme" "*tab-width*")
			 ,(make-function-string index "cl-readme" "make-path")
			 ,(make-function-string index "cl-readme" "read-file")))
      (semantic (:name "footer")
		"<hr/><p><small>Generated " ,(now) "</small></p>")
      "</body></html>"))

;;
;; Generate readme
;;

(defun make-readme ()
  (let ((index (docparser:parse :cl-readme)))
    (let ((cl-readme:*home-directory* (asdf:system-source-directory :cl-readme-make-readme)))
      (with-open-file (fh (cl-readme:make-path "docs/index.html")
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create
			  :external-format :utf-8)
	(cl-readme:doc-to-html fh (get-readme index)))))
    "DONE")

;;(make-readme)

