(in-package :cl-readme)

;;
;; DSL
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
;; TOC generation
;;

(defun generate-html-toc (output-stream doc)
  "Traverse doc and generate a HTML list representing the TOC"
  (labels ((sub-toc-p (sub-list)
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
		 (get-heading-id sub-list)
		 (get-heading-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (generate-html-toc-impl item))
		(format output-stream "</ul></li>"))
	       ((and (toc-heading-p sub-list) (not (sub-toc-p sub-list)))
		;; Current element is toc relevant and has no toc-relevant sub elements
		;; <li>...</li>
		(format
		 output-stream "<li><a href=\"#~a\">~a</a></li>"
		 (get-heading-id sub-list)
		 (get-heading-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (generate-html-toc-impl item)))
	       (t
		(dolist (item sub-list)
		  (generate-html-toc-impl item))))))
    (format output-stream "<ul>")
    (generate-html-toc-impl doc)
    (format output-stream "</ul>")))

(defun set-toc-heading-ids (doc)
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
;;
;; HTML-Writer
;;
;;

(defun doc-to-html-internal (output-stream doc)
  "Convert documentation to HTML"
  (labels ((doc-to-html-impl (heading-level sub-list)
	     (cond
	       ((not (listp sub-list))
		(format output-stream "~a" sub-list))
	       ((toc-p sub-list)
		(generate-html-toc output-stream doc))
	       ((semantic-p sub-list) ;; TODO Write class attribute if present
		(format output-stream "<~a>" (get-semantic-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (doc-to-html-impl heading-level item))
		(format output-stream "</~a>" (get-semantic-name sub-list)))
	       ((heading-p sub-list) ;; TODO Write class attribute if present
		(format
		 output-stream "<h~a~a>~a</h~a>"
		 (+ 1 heading-level)
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

(defun doc-to-html (output-stream doc)
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (let ((rewritten-doc (set-toc-heading-ids doc)))
    ;;(break)
    (doc-to-html-internal output-stream rewritten-doc)))
