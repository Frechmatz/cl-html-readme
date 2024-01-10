(in-package :cl-html-readme)

(defparameter *get-heading-attributes*
  (lambda (properties)
    (declare (ignore properties))
    nil)
    "Get the attributes of a heading form. A heading is rendered as a \"&lt;h1&gt;\" ... \"&lt;h6&gt;\" HTML element. The hook is called with the properties of the DSL <code>heading</code> form.")

(defparameter *get-semantic-attributes*
  (lambda (properties)
    (declare (ignore properties))
    nil)
    "Get the attributes of a semantic form. A semantic form is rendered as a \"&lt;${properties.name}&gt;\" HTML element. The hook is called with the properties of the DSL <code>semantic</code> form.")

(defparameter *get-toc-root-attributes*
  (lambda (properties)
    (declare (ignore properties))
    nil)
  "Get the attributes of the TOC root form. This form is created during the TOC expansion. The TOC root is rendered as a \"&lt;ul&gt;\"
 HTML element. The hook is called with the properties of the DSL <code>toc</code> form.")
					    
(defparameter *get-toc-item-attributes*
  (lambda (properties)
    (declare (ignore properties))
    nil)
    "Get the attributes of a TOC item form. Such forms are created during the TOC expansion. An item is an entry of the TOC that does not have sub-entries. Items are rendered as \"&lt;li&gt;\" HTML elements. The hook is called with the properties of the DSL <code>toc</code> form.")

(defparameter *get-toc-container-attributes*
  (lambda (properties)
    (declare (ignore properties))
    nil)
    "Get the HTML attributes of a TOC container form. Such forms are created during the TOC expansion. A container is an entry of the TOC that has sub-entries. Containers are rendered as \"&lt;ul&gt;\" HTML elements. The hook is called with the properties of the DSL <code>toc</code> form.")

(defun set-heading-ids (doc)
  "Assign ids to toc-headings. Returns a new documentation object."
  (let ((id-store nil) (tree-builder (cl-html-readme-dsl::make-tree-builder)))
    (labels ((make-id (name &key (counter 0))
	       (let ((id (if (eq 0 counter) name (format nil "~a-~a" name counter))))
		 (if (find id id-store :test #'string=)
		     (make-id name :counter (+ 1 counter))
		     (progn
		       (push id id-store)
		       id))))
	     (set-id (properties)
	       (let ((l (copy-list properties)))
		 (setf (getf l :id) (make-id (getf l :name)))
		 l)))
      (cl-html-readme-dsl::walk-tree
       doc
       :open-element
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (if (getf element-properties :toc)
	     (cl-html-readme-dsl::open-element tree-builder element-symbol (set-id element-properties))
	     (cl-html-readme-dsl::open-element tree-builder element-symbol element-properties))
	 nil)
       :close-element
       (lambda(context) (declare (ignore context)) (cl-html-readme-dsl::close-element tree-builder))
       :text
       (lambda(str) (cl-html-readme-dsl::add-text tree-builder str)))
      (cl-html-readme-dsl::get-tree tree-builder))))

(defun set-heading-indentation-levels (doc)
  "Set indentation levels of heading elements. Returns a new documentation object."
  (let ((level 0) (tree-builder (cl-html-readme-dsl::make-tree-builder)))
    (labels ((set-indentation-level (properties)
	       (let ((l (copy-list properties)))
		 (setf (getf l :level) level)
		 l)))
      (cl-html-readme-dsl::walk-tree
       doc
       :open-element
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (if (cl-html-readme-dsl::heading-p element-symbol)
	     (progn
	       (cl-html-readme-dsl::open-element
		tree-builder element-symbol
		(set-indentation-level element-properties))
	       (setf level (+ 1 level))
	       :decrement-level)
	     (progn
	       (cl-html-readme-dsl::open-element tree-builder element-symbol element-properties)
	       nil)))
       :close-element
       (lambda(context)
	 (if (eq context :decrement-level)
	     (setf level (+ -1 level)))
	 (cl-html-readme-dsl::close-element tree-builder))
       :text
       (lambda(str) (cl-html-readme-dsl::add-text tree-builder str)))
      (cl-html-readme-dsl::get-tree tree-builder))))

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
  (labels ((newline ()
	     (princ #\Newline output-stream))
	   (format-id (properties)
	     (if (getf properties :id)
		 (format nil " id=\"~a\"" (getf properties :id)) ""))
	   (format-heading (properties)
	     (let ((level (getf properties :level)))
	       (if (<= level 5)
		   (format nil "h~a" (+ 1 level))
		   (format nil "h6"))))
	   (format-extra-attributes (fn properties)
	     (format-extra-attributes-impl
	      (funcall fn properties))))
    (let ((toc-properties nil))
      (cl-html-readme-dsl::walk-tree
       doc
       :open-element
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (cond
	   ((cl-html-readme-dsl::heading-p element-symbol)
	    ;; <h{level} id={id} {render-hook}> {name} </h{level}>
	    (newline)
	    (format
	     output-stream
	     "<~a~a~a>~a</~a>"
	     (format-heading element-properties)
	     (format-id element-properties)
	     (format-extra-attributes *get-heading-attributes* element-properties)
	     (getf element-properties :name)
	     (format-heading element-properties))
	    nil)
	   ((cl-html-readme-dsl::semantic-p element-symbol)
	    (newline)
	    ;; <{name}>...</{name}>
	    (format
	     output-stream
	     "<~a>"
	     (getf element-properties :name))
	    (format nil "</~a>" (getf element-properties :name)))
	   ((cl-html-readme-dsl::toc-root-p element-symbol)
	    (newline)
	    ;; <ul>...</ul>
	    (format
	     output-stream
	     "<ul>")
	    "</ul>")
	   ((cl-html-readme-dsl::toc-item-p element-symbol)
	    ;; <li><a href=#{id}> {name} </a> </li>
	    (newline)
	    (format
	     output-stream
	     "<li><a href=\"#~a\">~a</a></li>"
	     (getf element-properties :id)
	     (getf element-properties :name))
	    nil)
	   ((cl-html-readme-dsl::toc-container-p element-symbol)
	    ;; <li> <a href=#{id}> {name} </a>
	    ;; <ul>...</ul>
	    ;; </li>
	    (newline)
	    (format
	     output-stream
	     "<li><a href=\"#~a\">~a</a><ul>"
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
      nil)))


;;
;; API
;;

(defun doc-to-html (output-stream documentation)
  "Renders a documentation object to HTML. The function has the following parameters:
   <ul>
       <li>output-stream nil or a stream into which the resulting HTML is written.</li>
       <li>documentation A documentation object following the syntax of the DSL.</li>
   </ul>"
  (cl-html-readme-dsl::validate documentation)
  (setf documentation (set-heading-ids documentation))
  (setf documentation (cl-html-readme-dsl::expand-toc documentation))
  (setf documentation (set-heading-indentation-levels documentation))
  (if output-stream
      (serialize output-stream documentation)
      (let ((string-output-stream (make-string-output-stream)))
	(serialize string-output-stream documentation)
	(get-output-stream-string string-output-stream))))

