(in-package :cl-html-readme)

;;
;; Rewriting
;;

;; TODO Take care of already set :id property
(defun set-heading-ids (doc)
  "Assign ids to toc-headings. Returns a new documentation tree."
  (let ((counter 0) (tree-builder (make-instance 'cl-html-readme-dsl:tree-builder)))
    (labels ((set-id (properties)
	       (let ((l (copy-list properties)))
		 (setf counter (+ 1 counter))
		 (setf (getf l :id) (format nil "~a-~a" (getf l :name) counter))
		 l)))
      (cl-html-readme-dsl:walk-tree
       doc
       :open-element
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (if (getf element-properties :toc)
	     (cl-html-readme-dsl:open-element tree-builder element-symbol (set-id element-properties))
	     (cl-html-readme-dsl:open-element tree-builder element-symbol element-properties))
	 nil)
       :close-element
       (lambda(context) (declare (ignore context)) (cl-html-readme-dsl:close-element tree-builder))
       :text
       (lambda(str) (cl-html-readme-dsl:add-text tree-builder str)))
      (cl-html-readme-dsl:get-tree tree-builder))))

(defun set-toc (doc)
  "Replace toc element with toc-root"
  (let ((tree-builder (make-instance 'cl-html-readme-dsl:tree-builder)))
    (cl-html-readme-dsl:walk-tree
     doc
     :open-element
     (lambda(element-symbol element-properties content)
       (declare (ignore content))
       (if (cl-html-readme-dsl:toc-p element-symbol)
	   (progn
	     (cl-html-readme-dsl:extract-toc doc tree-builder)
	     :ignore-close-element)
	   (progn
	     (cl-html-readme-dsl:open-element tree-builder element-symbol element-properties)
	     t)))
       :close-element
       (lambda(context)
	 (if (not (eq context :ignore-close-element))
	     (cl-html-readme-dsl:close-element tree-builder)))
       :text
       (lambda(str) (cl-html-readme-dsl:add-text tree-builder str)))
    (cl-html-readme-dsl:get-tree tree-builder)))

(defun set-heading-indentation-levels (doc)
  "Set indentation levels of heading elements."
  (let ((level 0) (tree-builder (make-instance 'cl-html-readme-dsl:tree-builder)))
    (labels ((set-indentation-level (properties)
	       (let ((l (copy-list properties)))
		 (setf (getf l :level) level)
		 l)))
      (cl-html-readme-dsl:walk-tree
       doc
       :open-element
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (if (cl-html-readme-dsl:heading-p element-symbol)
	     (progn
	       (cl-html-readme-dsl:open-element
		tree-builder element-symbol
		(set-indentation-level element-properties))
	       (setf level (+ 1 level))
	       :decrement-level)
	     (progn
	       (cl-html-readme-dsl:open-element tree-builder element-symbol element-properties)
	       nil)))
       :close-element
       (lambda(context)
	 (if (eq context :decrement-level)
	     (setf level (+ -1 level)))
	 (cl-html-readme-dsl:close-element tree-builder))
       :text
       (lambda(str) (cl-html-readme-dsl:add-text tree-builder str)))
      (cl-html-readme-dsl:get-tree tree-builder))))

;;
;; HTML generation
;;

(defun serialize (output-stream doc)
  (labels ((format-class (properties)
	     (if (getf properties :class)
		 (format nil " class=\"~a\" " (getf properties :class))
		 ""))
	   (format-style (properties)
	     (if (getf properties :style)
		 (format nil " style=\"~a\" " (getf properties :style))
		 ""))
	   (format-toc-class (properties)
	     (if (getf properties :toc-class)
		 (format nil " class=\"~a\" " (getf properties :toc-class))
		 ""))
	   (format-toc-style (properties)
	     (if (getf properties :toc-style)
		 (format nil " style=\"~a\" " (getf properties :toc-style))
		 ""))
	   (format-heading (properties)
	     (let ((level (getf properties :level)))
	       (if (<= level 5)
		   (format nil "h~a" (+ 1 level))
		   (format nil "h6")))))
    (cl-html-readme-dsl:walk-tree
     doc
     :open-element
     (lambda(element-symbol element-properties content)
       (declare (ignore content))
       (cond
	 ((cl-html-readme-dsl:heading-p element-symbol)
	  ;; <h{level} id={id} class={class} style={style}> {name} </h{level}>
	  (format
	   output-stream
	   "<~a id=\"~a\" ~a ~a>~a</~a>"
	   (format-heading element-properties)
	   (getf element-properties :id)
	   (format-class element-properties)
	   (format-style element-properties)
	   (getf element-properties :name)
	   (format-heading element-properties))
	  nil)
	 ((cl-html-readme-dsl:semantic-p element-symbol)
	  ;; <{name} class={class} style={style}>...</{name}>
	  (format
	   output-stream
	   "<~a ~a ~a>"
	   (getf element-properties :name)
	   (format-class element-properties)
	   (format-style element-properties))
	  (format nil "</~a>" (getf element-properties :name)))
	 ((cl-html-readme-dsl:toc-root-p element-symbol)
	  ;; <ul class={class} style={style}>...</ul>
	  (format
	   output-stream
	   "<ul ~a ~a>"
	   (format-toc-class element-properties)
	   (format-toc-style element-properties))
	  "</ul>")
	 ((cl-html-readme-dsl:toc-item-p element-symbol)
	  ;; <li class={toc-class} style={toc-style}> <a href=#{id}> {name} </a> </li>
	  (format
	   output-stream
	   "<li ~a ~a> <a href=\"#~a\">~a</a></li>"
	   (format-toc-class element-properties)
	   (format-toc-style element-properties)
	   (getf element-properties :id)
	   (getf element-properties :name))
	  nil)
	 ((cl-html-readme-dsl:toc-container-p element-symbol)
	  ;; <li class={toc-class} style={toc-style}> <a href=#{id}> {name} </a> <ul>...</ul> </li>
	  (format
	   output-stream
	   "<li ~a ~a><a href=\"#~a\">~a</a><ul>"
	   (format-toc-class element-properties)
	   (format-toc-style element-properties)
	   (getf element-properties :id)
	   (getf element-properties :name))
	  "</ul></li>")
	 (t (error (format nil "Dont know how to serialize ~a" element-symbol)))))
     :close-element
     (lambda(context)
       (if context
	   (format output-stream "~a" context)))
     :text
     (lambda(str)
       (format output-stream "~a" str)))
    nil))

;;
;; API
;;

(defun doc-to-html (output-stream doc)
  "Convert documentation to HTML"
  (setf doc (set-heading-ids doc))
  (setf doc (set-toc doc))
  (setf doc (set-heading-indentation-levels doc))
  (serialize output-stream doc)
  nil)
