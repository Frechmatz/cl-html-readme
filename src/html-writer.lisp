(in-package :cl-readme)

(defun make-id (name)
  ;; Table of content in Github flavored markdown
  ;; https://gist.github.com/asabaylus/3071099
  ;; tolowercase
  ;; spaces durch - ersetzen
  (format nil "Generated-Id-~a" name))

;;
;; HEADING
;;

(defun heading-p (l)
  (and (symbolp (first l)) (string= (symbol-name (first l)) "HEADING")))

(defun heading-settings (heading)
  (second heading))

(defun heading-settings-name (heading)
  "Returns the :name property of heading. Signals an error when the name is nil or empty."
  (let ((name (getf (heading-settings heading) :name)))
    (if (or (not name) (string= "" name))
	(error (format nil "Heading must have a :name property that is not empty and not nil: ~a" heading)))
    name))

(defun heading-id (heading)
  "Returns an id generated out of the :name property of the heading."
  (make-id (heading-settings-name heading)))

;;
;; Table of contents related HEADING
;;

(defun toc-heading-p (l)
  (and (heading-p l) (getf (heading-settings l) :toc)))

(defun toc-heading-chapter-p (l)
  (and (heading-p l) (eq :chapter (getf (heading-settings l) :toc))))

(defun toc-heading-item-p (l)
  (and (heading-p l) (eq :item (getf (heading-settings l) :toc))))

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
	       ((toc-heading-chapter-p sub-list)
		;; <li>...<ul>...</ul></li>
		(format
		 output-stream "<li><a href=\"#~a\">~a</a><ul>"
		 (heading-id sub-list)
		 (heading-settings-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (generate-html-toc-impl item))
		(format output-stream "</ul></li>"))
	       ((toc-heading-item-p sub-list)
		;; <li>...</li>
		(format
		 output-stream "<li><a href=\"#~a\">~a</a></li>"
		 (heading-id sub-list)
		 (heading-settings-name sub-list))
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
(defgeneric open-semantic (html-writer heading-element))
(defgeneric close-semantic (html-writer heading-element))

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
	       ((heading-p sub-list)
		(format
		 output-stream "<h~a~a>~a</h~a>"
		 (+ 1 heading-level)
		 (if (heading-id sub-list)
		     (format nil " id=\"~a\"" (heading-id sub-list))
		     "")
		 (heading-settings-name sub-list)
		 (+ 1 heading-level))
		(dolist (item (rest (rest sub-list)))
		  (doc-to-html-impl (+ 1 heading-level) item)))
	       (t
		(dolist (item sub-list)
		  (doc-to-html-impl heading-level item))))))

    (doc-to-html-impl 0 doc)
    nil))
