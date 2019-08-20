(in-package :cl-readme)

(defun make-id (name)
  ;; Table of content in Github flavored markdown
  ;; https://gist.github.com/asabaylus/3071099
  ;; tolowercase
  ;; spaces durch - ersetzen
  (format nil "Generated-Id-~a" name))

;;
;; SECTION
;;

(defun section-p (l)
  (and (symbolp (first l)) (string= (symbol-name (first l)) "SECTION")))

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

;;
;; Table of contents related SECTION
;;

(defun toc-section-p (l)
  (and (section-p l) (getf (section-settings l) :toc)))

(defun toc-section-chapter-p (l)
  (and (section-p l) (eq :chapter (getf (section-settings l) :toc))))

(defun toc-section-item-p (l)
  (and (section-p l) (eq :item (getf (section-settings l) :toc))))

;;
;; TOC (table of contents)
;;

(defun toc-p (i)
  (and (symbolp i) (string= (symbol-name i) "TOC")))

;;
;;
;; SEMANTIC
;;
;;


(defun semantic-p (l)
  (and (symbolp (first l)) (string= (symbol-name (first l)) "SEMANTIC")))

(defun semantic-settings (element)
  (second element))

(defun semantic-settings-name (semantic-element)
  "Returns the :name property of semantic-element. Signals an error when the name is nil or empty."
  (let ((name (getf (semantic-settings semantic-element) :name)))
    (if (or (not name) (string= "" name))
	(error (format nil "Semantic must have a :name property that is not empty and not nil: ~a" semantic-element)))
    name))


(defun generate-html-toc (output-stream doc)
  "Traverse doc and generate a HTML list representing the TOC"
  (labels ((generate-html-toc-impl (sub-list)
	     (cond
	       ((not (listp sub-list))
		nil)
	       ((toc-section-chapter-p sub-list)
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

;;
;;
;; HTML-Writer
;;
;;

(defclass html-writer () ())
(defgeneric open-semantic (html-writer section-element))
(defgeneric close-semantic (html-writer section-element))

(defmethod open-semantic ((writer html-writer) semantic-element-settings)
  (format nil "<~a>" (getf semantic-element-settings :name)))

(defmethod close-semantic ((writer html-writer) semantic-element-settings)
  (format nil "</~a>" (getf semantic-element-settings :name)))

(defun doc-to-html (html-writer output-stream doc)
  ""
  (labels ((doc-to-html-impl (heading-level sub-list)
	     (cond
	       ((toc-p sub-list)
		(generate-html-toc output-stream doc))
	       ((not (listp sub-list))
		(format output-stream "~a" sub-list))
	       ((semantic-p sub-list)
		(format output-stream "~a" (open-semantic html-writer (semantic-settings sub-list)))
		(dolist (item (rest (rest sub-list)))
		  (doc-to-html-impl (+ 1 heading-level) item))
		(format output-stream "~a" (close-semantic html-writer (semantic-settings sub-list))))
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
