(in-package :cl-html-readme)

;;
;; Renderer hooks
;;

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

;;
;; Unit tests hook
;;

(defparameter *print-newline*
  (lambda (stream)
    (princ #\Newline stream))
  "Private helper hook for unit tests to get more predictable HTML output.")

;;
;; HTML attributes rendering helper class
;;

(defclass attribute-renderer ()
  ((attributes :initform nil :documentation "A property list in reverse insertion order")))

(defgeneric add-attribute (attribute-renderer indicator value))
(defgeneric render-attributes (attribute-renderer &key prepend-space &allow-other-keys))

(defmethod add-attribute ((instance attribute-renderer) indicator value)
  (assert (keywordp indicator))
  (if (not (and (stringp value) (< 0 (length value))))
      (format
       t
       "~%Skipping HTML attribute '~a' because its value is nil or not a string or an empty string. ~a"
       indicator value)
      (with-slots (attributes) instance
	(progn
	  (push value attributes)
	  (push indicator attributes))))
  nil)

(defmethod render-attributes ((instance attribute-renderer) &key prepend-space)
  (let ((processed-keys nil) (processed-attributes nil))
    ;; Filter away duplicates (last one wins)
    (cl-html-readme-plist-util:with-properties
	(slot-value instance 'attributes)
      (lambda (key value)
	(if (not (find key processed-keys))
	    (progn
	      (push key processed-keys)
	      (push value processed-attributes)
	      (push key processed-attributes)))))
    (let ((first-attr t) (string-output-stream (make-string-output-stream)))
      (cl-html-readme-plist-util:with-properties
	  ;; Sort properties in order to have a stable (predictable) rendering output
	  (cl-html-readme-plist-util:sort-by-key processed-attributes)
	(lambda (key value)
	  (if (not first-attr)
	      (format string-output-stream " "))
	  (format
	   string-output-stream
	   "~a=\"~a\"" (string-downcase (symbol-name key)) value)
	  (setf first-attr nil)))
      (let ((rendered (get-output-stream-string string-output-stream)))
	(if (and prepend-space (< 0 (length rendered)))
	    (format nil " ~a" rendered)
	    rendered)))))

;;
;; Helper function to call a renderer hook and add attributes to renderer
;;

(defun add-custom-attributes (attribute-renderer fn form-properties)
  (let ((attributes (funcall fn form-properties)))
    (cl-html-readme-plist-util:with-properties
      attributes
      (lambda (key value)
	(if (not (keywordp key))
	    (error 'simple-error
		   :format-control
		   "HTML attribute indicator must be a keyword. Indicator: '~a' Value: '~a')"
		   :format-arguments (list key value)))
	(add-attribute attribute-renderer key value)))))

;;
;; HTML generation
;;

(defun serialize (output-stream doc)
  "Render documentation object that follows cl-html-readme-target-dsl::dsl."
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
	      (let ((id (getf form-properties :id)))
		(if id (add-attribute attribute-renderer :id id)))
	      (add-custom-attributes
	       attribute-renderer
	       *get-heading-attributes*
	       form-properties)
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
	      (add-custom-attributes
	       attribute-renderer
	       *get-semantic-attributes*
	       form-properties)
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
	      (add-custom-attributes
	       attribute-renderer
	       *get-toc-root-attributes*
	       toc-properties)
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
	      (add-custom-attributes
	       attribute-renderer
	       *get-toc-item-attributes*
	       toc-properties)
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
	      (add-custom-attributes
	       li-attribute-renderer
	       *get-toc-item-attributes*
	       toc-properties)
	      (add-custom-attributes
	       ul-attribute-renderer
	       *get-toc-container-attributes*
	       toc-properties)
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
       <li>documentation A documentation object following cl-html-readme-dsl::dsl.</li>
   </ul>"
  (let ((compiled (cl-html-readme-dsl-compiler:compile-documentation documentation)))
    (if output-stream
	(progn
	  (serialize output-stream compiled))
	(progn
	  (let ((string-output-stream (make-string-output-stream)))
	    (serialize string-output-stream compiled)
	    (get-output-stream-string string-output-stream))))))

