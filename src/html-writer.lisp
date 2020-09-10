(in-package :cl-readme)

;;
;; Rewriting
;;


(defun set-toc-ids (doc)
  "Assign ids to toc-headings. Returns a new documentation tree."
  (let ((counter 0) (builder (make-instance 'cl-readme-dsl:tree-builder)))
    (labels ((set-id (properties)
	       (let ((l (copy-list properties)))
		 (setf counter (+ 1 counter))
		 (setf (getf l :id) (format nil "~a-~a" (getf l :name) counter))
		 l)))
      (cl-readme-dsl:walk-tree
       doc
       :open-element
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (if (getf element-properties :toc)
	     (cl-readme-dsl:open-element builder element-symbol (set-id element-properties))
	     (cl-readme-dsl:open-element builder element-symbol element-properties))
	 nil)
       :close-element
       (lambda(context) (declare (ignore context)) (cl-readme-dsl:close-element builder))
       :text
       (lambda(str) (cl-readme-dsl:add-text builder str)))
      (cl-readme-dsl:get-tree builder))))

;;
;;
;;

(defun has-toc-elements (doc)
  "Brute force implementation to check if a list contains toc-headings"
  (let ((found nil))
    (cl-readme-dsl:walk-tree
     doc
     :open-element
     (lambda(element-symbol element-properties content)
       (declare (ignore element-symbol content))
       (if (getf element-properties :toc)
	   (setf found t)))
     :close-element
     (lambda(context) (declare (ignore context)) nil)
     :text
     (lambda(str) (declare (ignore str)) nil))
    found))

(defun extract-toc (doc)
  (let ((tree-builder (make-instance 'cl-readme-dsl:tree-builder)))
    (cl-readme-dsl:open-element tree-builder 'toc-root nil)
    (cl-readme-dsl:walk-tree
     doc
     :open-element
     (lambda(element-symbol element-properties content)
       (declare (ignore element-symbol))
       (if (getf element-properties :toc)
	   (progn
	     (if (has-toc-elements content)
		 (progn
		   (cl-readme-dsl:open-element tree-builder 'toc-container element-properties)
		   t)
		 (progn
		   (cl-readme-dsl:open-element tree-builder 'toc-item element-properties)
		   t)))
	   (progn
	     nil)))
     :close-element
     (lambda(context)
       (if context
	   (cl-readme-dsl:close-element tree-builder)))
     :text
     (lambda(str)
       (declare (ignore str))
       nil))
    (cl-readme-dsl:close-element tree-builder)
    (let ((tree (cl-readme-dsl:get-tree tree-builder)))
      tree)))


;;
;; API
;;

(defun doc-to-html (output-stream doc)
  "Convert documentation to HTML"
  (declare (ignore output-stream))
  ;;(doc-to-html-internal output-stream (set-toc-heading-ids doc)))
  (let ((doc-1 (set-toc-ids doc)))
    (format t "~%===================== Id-Enriched =======================~%")
    (format t "~%~a~%" doc-1)
    (format t "~%===================== Id-Enriched =======================~%")
    (let ((toc (extract-toc doc-1)))
      (format t "~%===================== Extracted Toc =======================~%")
      (format t "~%~a~%" toc)
      (format t "~%===================== Extracted Toc =======================~%")))

  nil)
