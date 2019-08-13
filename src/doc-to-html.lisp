(in-package :cl-readme)

(defun make-id (name)
  ;; Table of content in Github flavored markdown
  ;; https://gist.github.com/asabaylus/3071099
  ;; tolowercase
  ;; spaces durch - ersetzen
  (format nil "Generated-Id-~a" name))

(defun section-p (l)
  (and (symbolp (first l)) (string= (symbol-name (first l)) "SECTION")))

(defun toc-p (i)
  (and (symbolp i) (string= (symbol-name i) "TOC")))

(defun section-settings (section)
  (second section))

(defun section-settings-name (section)
  "Returns the :name property of section. Signals an error when the name is nil or empty."
  (let ((name (getf (section-settings section) :name)))
    (if (or (not name) (string= "" name))
	(error (format nil "Section must have a :name property that is not empty and not nil: ~a" section)))
    name))

(defun section-id (section)
  "Returns an id generated out of the :name property of the section."
  (make-id (section-settings-name section)))

(defun section-settings-toc (section)
  "Returns the :toc property of section. Signals an error when the property has an invalid value."
  (let ((toc (getf (section-settings section) :toc)))
    (if (and toc (not (eq :section toc)) (not (eq :item toc)))
	(error (format nil ":toc property of section must be one of nil :section :item ~a" section)))
    toc))

(defun toc-section-p (l)
  (and (section-p l) (getf (section-settings l) :toc)))

(defun toc-section-section-p (l)
  (and (section-p l) (eq :section (getf (section-settings l) :toc))))

(defun toc-section-item-p (l)
  (and (section-p l) (eq :item (getf (section-settings l) :toc))))

(defun generate-html-toc (output-stream doc)
  "Traverse doc and generate a HTML list representing the TOC"
  (labels ((generate-html-toc-impl (sub-list)
	     (cond
	       ((not (listp sub-list))
		nil)
	       ((toc-section-section-p sub-list)
		;; <li>...<ul>...</ul></li>
		(format
		 output-stream "<li><a href=\"#~a\">~a</a><ul>"
		 (section-id sub-list)
		 (section-settings-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (generate-html-toc-impl item))
		(format output-stream "</ul></li>"))
	       ((toc-section-item-p sub-list)
		;; <li>...</li>
		(format
		 output-stream "<li><a href=\"#~a\">~a</a></li>"
		 (section-id sub-list)
		 (section-settings-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (generate-html-toc-impl item)))
	       (t
		(dolist (item sub-list)
		  (generate-html-toc-impl item))))))
    (format output-stream "<ul>")
    (generate-html-toc-impl doc)
    (format output-stream "</ul>")))


(defun doc-to-html (output-stream doc)
  ""
  (labels ((doc-to-html-impl (heading-level sub-list)
	     (cond
	       ((toc-p sub-list)
		(generate-html-toc output-stream doc))
	       ((not (listp sub-list))
		(format output-stream "~a" sub-list))
	       ((section-p sub-list)
		(format
		 output-stream "<h~a~a>~a</h~a>"
		 (+ 1 heading-level)
		 (if (section-id sub-list)
		     (format nil " id=\"~a\"" (section-id sub-list))
		     "")
		 (section-settings-name sub-list)
		 (+ 1 heading-level))
		(dolist (item (rest (rest sub-list)))
		  (doc-to-html-impl (+ 1 heading-level) item)))
	       (t
		(dolist (item sub-list)
		  (doc-to-html-impl heading-level item))))))

    (doc-to-html-impl 0 doc)
    nil))
