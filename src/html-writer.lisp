(in-package :cl-readme)

(defun make-id (name)
  (format nil "id-~a" name))

(defun dsl-element-p (element-list symbol)
  (if (and (symbolp (first element-list)) (string= (symbol-name (first element-list)) (symbol-name symbol)))
      (if (not (listp (second element-list))) ;; TODO More strict check. Must be a property list
	  (error (format nil "Element ~a does not have a property list" symbol))
	  t)
      nil))
      
(defun get-dsl-element-settings (element-list)
  (second element-list))

(defun get-dsl-element-settings-property (l property &key (mandatory nil))
  (let ((v (getf (get-dsl-element-settings l) property)))
    (if (and mandatory (not v))
	(error (format nil "Missing mandatory property ~a in element ~a" property l)))
    v))

;;
;; HEADING
;;

(defun heading-p (l)
  (dsl-element-p l 'heading))

(defun heading-settings (l)
  (get-dsl-element-settings l))

(defun heading-settings-name (l)
  (get-dsl-element-settings-property l :name :mandatory t))

(defun heading-id (heading)
  "Returns an id generated out of the :name property of the heading."
  (make-id (heading-settings-name heading)))

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

(defun semantic-settings-name (l)
  (get-dsl-element-settings-property l :name :mandatory t))

;;
;; TOC generation
;;

(defun generate-html-toc (output-stream doc)
  "Traverse doc and generate a HTML list representing the TOC"
  (labels ((toc-heading-p (l)
	     (and (heading-p l) (getf (heading-settings l) :toc)))
	   (sub-toc-p (sub-list)
	     "Check if sub-list contains toc relevant headings.
              Not a very efficient implementation as sub-tree may get parsed two times."
	     (cond
	       ((not (listp sub-list))
		nil)
	       ((toc-heading-p sub-list)
		t)
	       (t
		(let ((found nil))
		  (dolist (item sub-list)
		    (if (sub-toc-p item)
			(setf found t)))
		  found))))
	   (generate-html-toc-impl (sub-list)
	     (cond
	       ((not (listp sub-list))
		nil)
	       ((and (toc-heading-p sub-list) (sub-toc-p sub-list))
		;; Current element is toc relevant and has toc-relevant sub elements
		;; <li>...<ul>...</ul></li>
		(format
		 output-stream "<li><a href=\"#~a\">~a</a><ul>"
		 (heading-id sub-list)
		 (heading-settings-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (generate-html-toc-impl item))
		(format output-stream "</ul></li>"))
	       ((and (toc-heading-p sub-list) (not (sub-toc-p sub-list)))
		;; Current element is toc relevant and has no toc-relevant sub elements
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

(defun doc-to-html (output-stream doc)
  "Convert documentation to HTML"
  (labels ((doc-to-html-impl (heading-level sub-list)
	     (cond
	       ((toc-p sub-list)
		(generate-html-toc output-stream doc))
	       ((not (listp sub-list))
		(format output-stream "~a" sub-list))
	       ((semantic-p sub-list) ;; TODO Write class attribute if present
		(format output-stream "<~a>" (semantic-settings-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (doc-to-html-impl heading-level item))
		(format output-stream "</~a>" (semantic-settings-name sub-list)))
	       ((heading-p sub-list) ;; TODO Write class attribute if present
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
