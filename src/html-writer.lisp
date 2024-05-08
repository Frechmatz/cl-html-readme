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
;; HTML generation
;;

(defun format-extra-attributes-impl (attrs)
  (let ((string-output-stream (make-string-output-stream))
	(reading-key t)
	(cur-key nil)
	(cur-value nil))
    (dolist (item attrs)
      (if reading-key
	  (setf cur-key item)
	  (setf cur-value item))
      (if (and (not reading-key) (stringp cur-value) (< 0 (length cur-value)))
	  (format
	   string-output-stream
	   " ~a=\"~a\""
	   (string-downcase (symbol-name cur-key))
	   cur-value))
      (setf reading-key (not reading-key)))
    (get-output-stream-string string-output-stream)))

(defun serialize (output-stream doc)
  "Render documentation object. The object is supposed to follow the syntax of cl-html-readme-target-dsl."
  (labels ((newline ()
	     (funcall *print-newline* output-stream))
	   (format-id (properties)
	     (if (getf properties :id)
		 (format nil " id=\"~a\"" (getf properties :id)) ""))
	   (format-heading (properties)
	     (let ((level (getf properties :indentation-level)))
	       (if (<= level 5)
		   (format nil "h~a" (+ 1 level))
		   (format nil "h6"))))
	   (format-extra-attributes (fn properties)
	     (format-extra-attributes-impl
	      (funcall fn properties))))
    (let ((toc-properties nil))
      (cl-html-readme-base-dsl:walk-tree-ng
       (cl-html-readme-target-dsl:instance)
       doc
       :open-form-handler
       (lambda(form-symbol form-properties content)
	 (declare (ignore content))
	 (cond
	   ;;
	   ;; Heading
	   ;;
	   ((cl-html-readme-base-dsl:equal-symbol form-symbol 'heading)
	    ;; <h{level} id={id} {render-hook}> {name} </h{level}>
	    (newline)
	    (format
	     output-stream
	     "<~a~a~a>~a</~a>"
	     (format-heading form-properties)
	     (format-id form-properties)
	     (format-extra-attributes *get-heading-attributes* form-properties)
	     (getf form-properties :name)
	     (format-heading form-properties))
	    nil)
	   ;;
	   ;; Semantic
	   ;;
	   ((cl-html-readme-base-dsl:equal-symbol 'semantic form-symbol)
	    (newline)
	    ;; <{name {render-hook}}>...</{name}>
	    (format
	     output-stream
	     "<~a~a>"
	     (getf form-properties :name)
	     (format-extra-attributes *get-semantic-attributes* form-properties))
	    (format nil "</~a>" (getf form-properties :name)))
	   ;;
	   ;; Toc-Root
	   ;;
	   ((cl-html-readme-base-dsl:equal-symbol form-symbol 'toc-root)
	    (setf toc-properties form-properties)
	    (newline)
	    ;; <ul {render-hook}>...</ul>
	    (format
	     output-stream
	     "<ul~a>"
	     (format-extra-attributes *get-toc-root-attributes* toc-properties))
	    "</ul>")
	   ;;
	   ;; Toc-Item
	   ;;
	   ((cl-html-readme-base-dsl:equal-symbol form-symbol 'toc-item)
	    ;; <li {render-hook}><a href=#{id}> {name} </a> </li>
	    (newline)
	    (format
	     output-stream
	     "<li~a><a href=\"#~a\">~a</a></li>"
	     (format-extra-attributes *get-toc-item-attributes* toc-properties)
	     (getf form-properties :id)
	     (getf form-properties :name))
	    nil)
	   ;;
	   ;; Toc-Container
	   ;;
	   ((cl-html-readme-base-dsl:equal-symbol form-symbol 'toc-container)
	    ;; <li {render-hook}> <a href=#{id}> {name} </a>
	    ;; <ul {render-hook}>...</ul>
	    ;; </li>
	    (newline)
	    (format
	     output-stream
	     "<li~a><a href=\"#~a\">~a</a><ul~a>"
	     (format-extra-attributes *get-toc-item-attributes* toc-properties)
	     (getf form-properties :id)
	     (getf form-properties :name)
	     (format-extra-attributes *get-toc-container-attributes* toc-properties))
	    "</ul></li>")
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

