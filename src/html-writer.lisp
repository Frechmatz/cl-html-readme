(in-package :cl-html-readme)

(defparameter *get-heading-attributes*
  (lambda (properties)
    (declare (ignore properties))
    nil)
    "Get the attributes of a heading form. A heading is rendered as a <code>\"&lt;h1&gt;\"</code> ... <code>\"&lt;h6&gt;\" </code> HTML element. The hook is called with the properties of the DSL <code>heading</code> form.")

(defparameter *get-semantic-attributes*
  (lambda (properties)
    (declare (ignore properties))
    nil)
    "Get the attributes of a semantic form. A semantic form is rendered as a <code>\"&lt;${properties.name}&gt;\"</code> HTML element. The hook is called with the properties of the DSL <code>semantic</code> form.")

(defparameter *get-toc-root-attributes*
  (lambda (properties)
    (declare (ignore properties))
    nil)
  "Get the attributes of the TOC root form. This form is created during the TOC expansion. The TOC root is rendered as a <code>\"&lt;ul&gt;\"</code>
 HTML element. The hook is called with the properties of the DSL <code>toc</code> form.")
					    
(defparameter *get-toc-item-attributes*
  (lambda (properties)
    (declare (ignore properties))
    nil)
    "Get the attributes of a TOC item form. Such forms are created during the TOC expansion. Each entry of the TOC is represented by an item. Items are rendered as <code>\"&lt;li&gt;\"</code> HTML elements. The hook is called with the properties of the DSL <code>toc</code> form.")

(defparameter *get-toc-container-attributes*
  (lambda (properties)
    (declare (ignore properties))
    nil)
    "Get the HTML attributes of a TOC container form. Such forms are created during the TOC expansion. A container is an entry of the TOC that has sub-entries. Containers are rendered as <code>\"&lt;ul&gt;\"</code> HTML elements. The hook is called with the properties of the DSL <code>toc</code> form.")

(defparameter *print-newline*
  (lambda (stream)
    (princ #\Newline stream))
  "Private helper hook for unit tests to get more predictable HTML output.")

;;
;; Attribute rendering helper class
;;

(defclass attribute-renderer ()
  ((attributes :initform nil)))

(defgeneric add-attribute (attribute-renderer indicator value))
(defgeneric add-attributes (attribute-renderer plist))
(defgeneric omit-attribute (attribute-renderer value))
(defgeneric render-attributes (attribute-renderer &key prepend-space &allow-other-keys))

(defmethod omit-attribute ((instance attribute-renderer) value)
  (if (and (stringp value) (< 0 (length value)))
      nil
      t))
  
(defmethod add-attribute ((instance attribute-renderer) indicator value)
  (let ((attribute-name
	  (if (keywordp indicator)
	      (string-downcase (symbol-name indicator))
	      indicator)))
    (with-slots (attributes) instance
      (if (not (omit-attribute instance value))
	  (push (list :name attribute-name :value value) attributes)))
    nil))

(defmethod add-attributes ((instance attribute-renderer) plist)
  (cl-html-readme-plist-util:with-properties
      plist
      (lambda (key value)
	(add-attribute instance (string-downcase (symbol-name key)) value)))
  nil)

(defmethod render-attributes ((instance attribute-renderer) &key prepend-space)
  (let ((first-attr t) (string-output-stream (make-string-output-stream)))
    (with-slots (attributes) instance
      ;; follow insertion order
      (dolist (attribute (reverse attributes))
	(let ((name (getf attribute :name))
	      (value (getf attribute :value)))
	  (if (not first-attr)
	      (format string-output-stream " "))
	  (format
	   string-output-stream
	   "~a=\"~a\"" name value))
	(setf first-attr nil)))
    (let ((rendered (get-output-stream-string string-output-stream)))
      (if (and prepend-space (< 0 (length rendered)))
	  (format nil " ~a" rendered)
	  rendered))))

;;
;; HTML generation
;;

(defun serialize (output-stream doc)
  "Render documentation object. The object is supposed to follow the syntax of cl-html-readme-target-dsl."
  (labels ((newline ()
	     (funcall *print-newline* output-stream))
	   (format-heading (properties)
	     (let ((level (getf properties :indentation-level)))
	       (if (<= level 5)
		   (format nil "h~a" (+ 1 level))
		   (format nil "h6")))))
    (let ((toc-properties nil))
      (cl-html-readme-base-dsl:walk
       (cl-html-readme-target-dsl:instance)
       doc
       :open-form-handler
       (lambda(form-symbol form-properties content)
	 (declare (ignore content))
	 (cond
	   ;;
	   ;; Heading
	   ;;
	   ((string= "HEADING" (string-upcase (symbol-name form-symbol)))
	    ;; <h{level} id={id} {custom-attributes}> {name} </h{level}>
	    (newline)
	    (let ((attribute-renderer (make-instance 'attribute-renderer)))
	      (add-attribute attribute-renderer :id (getf form-properties :id))
	      (add-attributes
	       attribute-renderer
	       (funcall
		*get-heading-attributes*
		form-properties))
	      (format
	       output-stream
	       "<~a~a>~a</~a>"
	       (format-heading form-properties)
	       (render-attributes attribute-renderer :prepend-space t)
	       (getf form-properties :name)
	       (format-heading form-properties))
	      nil))
	   ;;
	   ;; Semantic
	   ;;
	   ((string= "SEMANTIC" (string-upcase (symbol-name form-symbol)))
	    (newline)
	    (let ((attribute-renderer (make-instance 'attribute-renderer)))
	      (add-attributes
	       attribute-renderer
	       (funcall
		*get-semantic-attributes*
		form-properties))
	      ;; <{name {custom-attributes}}>...</{name}>
	      (format
	       output-stream
	       "<~a~a>"
	       (getf form-properties :name)
	       (render-attributes attribute-renderer :prepend-space t))
	      (format nil "</~a>" (getf form-properties :name))))
	   ;;
	   ;; Toc-Root
	   ;;
	   ((string= "TOC-ROOT" (string-upcase (symbol-name form-symbol)))
	    (setf toc-properties form-properties)
	    (newline)
	    (let ((attribute-renderer (make-instance 'attribute-renderer)))
	      (add-attributes
	       attribute-renderer
	       (funcall
		*get-toc-root-attributes*
		toc-properties))
	      ;; <ul {custom-attributes}>...</ul>
	      (format
	       output-stream
	       "<ul~a>"
	       (render-attributes attribute-renderer :prepend-space t)))
	      "</ul>")
	   ;;
	   ;; Toc-Item
	   ;;
	   ((string= "TOC-ITEM" (string-upcase (symbol-name form-symbol)))
	    ;; <li {custom-attributes}><a href=#{id}> {name} </a> </li>
	    (newline)
	    (let ((attribute-renderer (make-instance 'attribute-renderer)))
	      (add-attributes
	       attribute-renderer
	       (funcall
		*get-toc-item-attributes*
		toc-properties))
	      (format
	       output-stream
	       "<li~a><a href=\"#~a\">~a</a></li>"
	       (render-attributes attribute-renderer :prepend-space t)
	       (getf form-properties :id)
	       (getf form-properties :name))
	      nil))
	   ;;
	   ;; Toc-Container
	   ;;
	   ((string= "TOC-CONTAINER" (string-upcase (symbol-name form-symbol)))
	    ;; <li {custom-attributes}> <a href=#{id}> {name} </a>
	    ;; <ul {custom-attributes}>...</ul>
	    ;; </li>
	    (newline)
	    (let ((li-attribute-renderer (make-instance 'attribute-renderer))
		  (ul-attribute-renderer (make-instance 'attribute-renderer))		  )
	      (add-attributes
	       li-attribute-renderer
	       (funcall
		*get-toc-item-attributes*
		toc-properties))
	      (add-attributes
	       ul-attribute-renderer
	       (funcall
		*get-toc-container-attributes*
		toc-properties))
	      (format
	       output-stream
	       "<li~a><a href=\"#~a\">~a</a><ul~a>"
	       (render-attributes li-attribute-renderer :prepend-space t)
	       (getf form-properties :id)
	       (getf form-properties :name)
	       (render-attributes ul-attribute-renderer :prepend-space t))
	      "</ul></li>"))
	   (t (error (format nil "Dont know how to serialize ~a" form-symbol)))))
       :close-form-handler
       (lambda(context)
	 (if context
	     (format output-stream "~a" context)))
       :text-handler
       (lambda(str)
	 (format output-stream "~a" str)))
      nil)))


;;
;; API
;;

(defun doc-to-html (output-stream documentation)
  "Renders a documentation object to HTML. The function has the following parameters:
   <ul>
       <li>output-stream nil or a stream into which the resulting HTML is written.</li>
       <li>documentation A documentation object following the syntax as defined by the package of the cl-html-readme-dsl.</li>
   </ul>"
  (let ((compiled (cl-html-readme-dsl-compiler:compile-documentation documentation)))
    (if output-stream
	(progn
	  (serialize output-stream compiled))
	(progn
	  (let ((string-output-stream (make-string-output-stream)))
	    (serialize string-output-stream compiled)
	    (get-output-stream-string string-output-stream))))))

