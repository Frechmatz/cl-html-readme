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

(defun has-toc-headings (sub-list)
  "Brute force implementation to check if a list contains toc-headings"
  (cond
    ((not (listp sub-list))
     nil)
    ((toc-heading-p sub-list)
     t)
    (t
     (let ((found nil))
       (dolist (item sub-list)
	 (if (has-toc-headings item)
	     (setf found t)))
       found))))

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

;; TODO Set properties such as id and name at toc elements
;; TODO Introduce make-node function
(defun generate-toc (doc)
  (let ((tree-builder (make-instance 'tree-builder)))
    (labels ((traverse (sub-list)
	       (cond
		 ((not (listp sub-list))
		  nil)
		 ((and (toc-heading-p sub-list) (has-toc-headings (rest (rest sub-list))))
		  (let ((node (list 'toc-container :name (get-heading-name sub-list))))
		    (open-node tree-builder node)
		    (dolist (item (rest (rest sub-list)))
		      (traverse item) result)
		    (close-node tree-builder)))
		 ((toc-heading-p sub-list)
		  (let ((node (list 'toc-item :name (get-heading-name sub-list))))
		    (add-node tree-builder node)
		    (dolist (item (rest (rest sub-list)))
		      (traverse item) result)))
		 (t
		  (dolist (item sub-list)
		    (traverse item))))))
      (traverse doc)
      (get-tree tree-builder))))

;; TODO Rewrite toc symbol
(defun set-toc (doc)
  (declare (ignore doc))
  nil
  )



;;
;; HTML Generation
;;

;; TODO Remove this function and add HTML generation to doc-to-html-internal
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
	   (toc-to-html-impl (toc-level sub-list)
	     (cond
	       ((not (listp sub-list))
		nil)
	       ((and (toc-heading-p sub-list) (sub-toc-p (rest (rest sub-list))))
		;; Current element is a toc-heading and contains sub toc-headings
		;; <li>...<ul>...</ul></li>
		(format
		 output-stream "<li~a><a href=\"#~a\">~a</a><ul~a>"
		 (if (funcall *get-toc-item-class* toc-level)
		     (format nil " class=\"~a\"" (funcall *get-toc-item-class* toc-level)) "")
		 (get-heading-id sub-list)
		 (get-heading-name sub-list)
		 (if (funcall *get-toc-container-class* toc-level)
		     (format nil " class=\"~a\"" (funcall *get-toc-container-class* toc-level)) ""))
		(dolist (item (rest (rest sub-list)))
		  (toc-to-html-impl (+ 1 toc-level) item))
		(format output-stream "</ul></li>"))
	       ((toc-heading-p sub-list)
		;; Current element is a toc-heading that has no sub toc-headings
		;; <li>...</li>
		(format
		 output-stream "<li~a><a href=\"#~a\">~a</a></li>"
		 (if (funcall *get-toc-item-class* toc-level)
		     (format nil " class=\"~a\"" (funcall *get-toc-item-class* toc-level)) "")
		 (get-heading-id sub-list)
		 (get-heading-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (toc-to-html-impl (+ 1 toc-level) item)))
	       (t
		(dolist (item sub-list)
		  (toc-to-html-impl toc-level item))))))
    (format output-stream "<ul~a>"
	    (if (funcall *get-toc-container-class* 0)
		(format nil " class=\"~a\"" (funcall *get-toc-container-class* 0)) ""))
    (toc-to-html-impl 1 doc)
    (format output-stream "</ul>")))

(defun doc-to-html-internal (output-stream doc)
  "Traverse doc and generate HTML"
  (labels ((doc-to-html-impl (heading-level sub-list)
	     (cond
	       ((not (listp sub-list))
		(format output-stream "~a" sub-list))
	       ((toc-p sub-list)
		(toc-to-html output-stream doc))
	       ((semantic-p sub-list)
		(format output-stream "<~a>" (get-semantic-name sub-list))
		(dolist (item (rest (rest sub-list)))
		  (doc-to-html-impl heading-level item))
		(format output-stream "</~a>" (get-semantic-name sub-list)))
	       ((heading-p sub-list)
		(format
		 output-stream "<h~a~a~a>~a</h~a>"
		 (+ 1 heading-level) ;; TODO Take care of maximum HTML heading level
		 (if (get-heading-id sub-list)
		     (format nil " id=\"~a\"" (get-heading-id sub-list)) "")
		 (if (funcall *get-heading-class* heading-level)
		     (format nil " class=\"~a\"" (funcall *get-heading-class* heading-level)) "")
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


;;
;; Test
;;

(defun get-test-doc-1 ()
  `((heading (:name "1" :toc t))
    "Text"
    (heading (:name "2" :toc t))
    "Text"
    (heading (:name "3" :toc t)
	     "Text"
	     (heading (:name "3.1" :toc t))
	     "Text"
	     (heading (:name "3.2" :toc t)))
    (heading (:name "4" :toc t))))

(defun test-1 ()
  (let ((doc (get-test-doc-1)))
    (let ((id-enriched-doc (set-toc-heading-ids doc)))
      (format t "~%Added Ids: ~a~%" id-enriched-doc)
      (let ((parsed-toc (generate-toc id-enriched-doc)))
	(format t "~%Parsed Toc: ~a~%" parsed-toc))
      ;; TODO Rewrote TOC
      )))

;;(test-1)

