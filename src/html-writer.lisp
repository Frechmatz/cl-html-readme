(in-package :cl-readme)

;;
;; DSL helper functions
;;

(defun heading-p (l)
  (and (symbolp (first l)) (string= (symbol-name (first l)) "HEADING")))

(defun toc-heading-p (l)
  (and (heading-p l) (getf (second l) :toc)))

(defun get-heading-name (l)
  (getf (second l) :name))

(defun get-heading-id (l)
  (getf (second l) :id))

(defun toc-p (l)
  (and (symbolp (first l)) (string= (symbol-name (first l)) "TOC")))

(defun semantic-p (l)
  (and (symbolp (first l)) (string= (symbol-name (first l)) "SEMANTIC")))

(defun get-semantic-name (l)
  (getf (second l) :name))

;;
;; Rewriting
;;

(defun set-toc-heading-ids (doc)
  "Assign ids to toc-headings"
  (let ((counter 0))
    (labels ((make-id (heading-settings)
	       (setf counter (+ 1 counter))
	       (format nil "~a-~a" (getf heading-settings :name) counter))
	     (clone-list (l)
               (if (not (listp l))
                   l
                   (let ((c (list)))
		     (cond
		       ((toc-heading-p l)
			(let ((heading-settings (copy-list (second l))))
			  (push 'heading c)
			  (setf (getf heading-settings :id) (make-id heading-settings))
			  (push heading-settings c)
			  (dolist (item (rest (rest l)))
			    (push (clone-list item) c))))
		       (t
			(dolist (item l)
			  (push (clone-list item) c))
			))
                     (reverse c)))))
      (clone-list doc))))

;;
;; HTML Generation
;;

(defun toc-to-html (output-stream doc)
  "Traverse doc and generate a HTML list representing the TOC"
  (labels ((sub-toc-p (sub-list)
	     "Brute force implementation to check if a toc-heading contains sub toc-headings"
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
	   (toc-to-html-impl (sub-list)
	     (cond
	       ((not (listp sub-list))
		nil)
	       ((and (toc-heading-p sub-list) (sub-toc-p (rest (rest sub-list))))
		;; Current element is a toc-heading and contains sub toc-headings
		;; <li>...<ul>...</ul></li>
		(format
		 output-stream "<li><a href=\"#~a\">~a</a><ul>"
		 (get-heading-id sub-list)
		 (get-heading-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (toc-to-html-impl item))
		(format output-stream "</ul></li>"))
	       ((toc-heading-p sub-list)
		;; Current element is a toc-heading that has no sub toc-headings
		;; <li>...</li>
		(format
		 output-stream "<li><a href=\"#~a\">~a</a></li>"
		 (get-heading-id sub-list)
		 (get-heading-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (toc-to-html-impl item)))
	       (t
		(dolist (item sub-list)
		  (toc-to-html-impl item))))))
    (format output-stream "<ul>")
    (toc-to-html-impl doc)
    (format output-stream "</ul>")))

(defun doc-to-html-internal (output-stream doc)
  "Traverse doc and generate HTML"
  (labels ((doc-to-html-impl (heading-level sub-list)
	     (cond
	       ((not (listp sub-list))
		(format output-stream "~a" sub-list))
	       ((toc-p sub-list)
		(toc-to-html output-stream doc))
	       ((semantic-p sub-list) ;; TODO Write class attribute if present
		(format output-stream "<~a>" (get-semantic-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (doc-to-html-impl heading-level item))
		(format output-stream "</~a>" (get-semantic-name sub-list)))
	       ((heading-p sub-list) ;; TODO Write class attribute if present
		(format
		 output-stream "<h~a~a>~a</h~a>"
		 (+ 1 heading-level) ;; TODO Take care of maximum HTML heading level
		 (if (get-heading-id sub-list)
		     (format nil " id=\"~a\"" (get-heading-id sub-list)) "")
		 (get-heading-name sub-list)
		 (+ 1 heading-level))
		(dolist (item (rest (rest sub-list)))
		  (doc-to-html-impl (+ 1 heading-level) item)))
	       (t
		(dolist (item sub-list)
		  (doc-to-html-impl heading-level item))))))
 
    (doc-to-html-impl 0 doc)
    nil))

;;
;; API
;;

(defun doc-to-html (output-stream doc)
  "Convert documentation to HTML"
  (doc-to-html-internal output-stream (set-toc-heading-ids doc)))
