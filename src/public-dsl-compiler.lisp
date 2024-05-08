(in-package :cl-html-readme-public-dsl-compiler)

;;
;; TOC Processing
;;

(defun get-toc-headings (doc)
  "Returns a documentation object representing the toc heading tree"
  (flet ((is-toc-heading (form-symbol form-properties)
	   (and (cl-html-readme-base-dsl:equal-symbol form-symbol 'heading)
		(getf form-properties :toc))))
    (let ((tree-builder (cl-html-readme-base-dsl:make-builder (cl-html-readme-base-dsl:instance))))
      (cl-html-readme-base-dsl:walk-tree-ng
       (cl-html-readme-base-dsl:instance)
       doc
       :open-form-handler
       (lambda(form-symbol form-properties content)
	 (declare (ignore content))
	 (if (is-toc-heading form-symbol form-properties)
	     (progn
	       (cl-html-readme-base-dsl:open-form
		tree-builder
		form-symbol
		form-properties)
	       t)
	     nil))
       :close-form-handler
       (lambda(context)
	 (if context
	     (cl-html-readme-base-dsl:close-form tree-builder)))
       :text-handler
       (lambda(str)
	 (declare (ignore str))
	 nil))
      (cl-html-readme-base-dsl:get-tree tree-builder))))

(defun write-toc (doc toc-properties tree-builder)
  "Extracts toc and writes toc-root, toc-container, toc-item forms into the builder.
  - toc-properties: The properties of the corresponding toc-form"
  (flet ((remove-toc-property (properties)
	   (cl-html-readme-plist-util:filter-property-list-entries
	    properties
	    (lambda(keyword) (not (eq keyword :toc))))))
    (let ((toc-headings (get-toc-headings doc)))
      (if toc-headings
	  (progn
	    ;; Render toc-root
	    (cl-html-readme-base-dsl:open-form
	     tree-builder
	     'toc-root
	     toc-properties)
	    ;; Render toc content
	    (cl-html-readme-base-dsl:walk-tree-ng
	     (cl-html-readme-base-dsl:instance)
	     toc-headings
	     :text-handler (lambda(str) (declare (ignore str)) nil)
	     :open-form-handler
	     (lambda(form-symbol form-properties content)
	       (declare (ignore form-symbol))
	       (if (not content)
		   (progn
		     ;; Heading does not have sub-headings. Render a plain toc-item.
		     (cl-html-readme-base-dsl:open-form
		      tree-builder
		      'toc-item
		      (remove-toc-property
		       (concatenate
			'list
			toc-properties
			form-properties)))
		     nil)
		   (progn
		     ;; Heading has sub-headings. Render a toc-container.
		     (cl-html-readme-base-dsl:open-form
		      tree-builder
		      'toc-container
		      (remove-toc-property
		       (concatenate
			'list
			form-properties
			toc-properties)))
		     nil)))
	     :close-form-handler
	     (lambda(context)
	       (declare (ignore context))
	       (cl-html-readme-base-dsl:close-form tree-builder)))
	    ;; Close toc-root
	    (cl-html-readme-base-dsl:close-form tree-builder))))))

(defun clean-headings (doc)
  "<p>Helper function to cleanup heading properties</p><ul>
   <li>Remove ids of heading forms that are not marked as toc relevant<li>
   <li>Remove toc indicator from all headings</li>
   <ul>"
  (let ((tree-builder (cl-html-readme-base-dsl:make-builder (cl-html-readme-base-dsl:instance))))
    (cl-html-readme-base-dsl:walk-tree-ng
     (cl-html-readme-base-dsl:instance)
     doc
     :open-form-handler
     (lambda(form-symbol form-properties content)
       (declare (ignore content))
       (if (not (cl-html-readme-base-dsl:equal-symbol form-symbol 'heading))
	   ;; Not a heading element => Pass through
	   (cl-html-readme-base-dsl:open-form
	    tree-builder
	    form-symbol
	    form-properties)
	   (progn
	     (let ((is-toc (getf form-properties :toc)))
	       ;; Heading element that is not toc relevant => Remove :id property
	       (cl-html-readme-base-dsl:open-form
		tree-builder
		form-symbol
		(cl-html-readme-plist-util:filter-property-list-entries
		 form-properties
		 (lambda (keyword)
		   (if (eq :toc keyword)
		       ;; Remove toc indicator
		       nil
		       ;; Remove id if not toc relevant
		       (if (and (not is-toc) (eq :id keyword)) nil t)))))))))
     :close-form-handler
     (lambda(context) (declare (ignore context)) (cl-html-readme-base-dsl:close-form tree-builder))
     :text-handler
     (lambda(str) (cl-html-readme-base-dsl:add-text tree-builder str)))
    (cl-html-readme-base-dsl:get-tree tree-builder)))

(defun set-heading-ids (doc)
  "Helper function which assigns an id to all heading elements. Returns a new documentation object."
  (let ((id-store nil)
	(tree-builder (cl-html-readme-base-dsl:make-builder (cl-html-readme-base-dsl:instance))))
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
      (cl-html-readme-base-dsl:walk-tree-ng
       (cl-html-readme-base-dsl:instance)
       doc
       :open-form-handler
       (lambda(form-symbol form-properties content)
	 (declare (ignore content))
	 (if (cl-html-readme-base-dsl:equal-symbol form-symbol 'heading)
	     (cl-html-readme-base-dsl:open-form tree-builder form-symbol (set-id form-properties))
	     (cl-html-readme-base-dsl:open-form tree-builder form-symbol form-properties))
	 nil)
       :close-form-handler
       (lambda(context) (declare (ignore context)) (cl-html-readme-base-dsl:close-form tree-builder))
       :text-handler
       (lambda(str) (cl-html-readme-base-dsl:add-text tree-builder str)))
      (cl-html-readme-base-dsl:get-tree tree-builder))))

(defun expand-toc (doc)
  "Replace toc form with toc-root. Returns a new documentation object."
  (let ((enriched-doc (set-heading-ids doc))
	(tree-builder (cl-html-readme-base-dsl:make-builder (cl-html-readme-base-dsl:instance))))
    (cl-html-readme-base-dsl:walk-tree-ng
     (cl-html-readme-base-dsl:instance)
     enriched-doc
     :open-form-handler
     (lambda(form-symbol form-properties content)
       (declare (ignore content))
       (if (cl-html-readme-base-dsl:equal-symbol form-symbol 'toc)
	   (progn
	     (write-toc enriched-doc form-properties tree-builder)
	     :ignore-close-form)
	   (progn
	     (cl-html-readme-base-dsl:open-form tree-builder form-symbol form-properties)
	     t)))
     :close-form-handler
     (lambda(context)
       (if (not (eq context :ignore-close-form))
	   (cl-html-readme-base-dsl:close-form tree-builder)))
     :text-handler
     (lambda(str) (cl-html-readme-base-dsl:add-text tree-builder str)))
    (clean-headings (cl-html-readme-base-dsl:get-tree tree-builder))))

;;
;; Heading-Indentation
;;

(defun set-heading-indentation-levels (doc)
  "Set indentation levels of heading forms. Returns a new documentation object."
  (let ((level 0)
	(tree-builder (cl-html-readme-base-dsl:make-builder (cl-html-readme-base-dsl:instance))))
    (labels ((set-indentation-level (properties)
	       (let ((l (copy-list properties)))
		 (setf (getf l :indentation-level) level)
		 l)))
      (cl-html-readme-base-dsl:walk-tree-ng
       (cl-html-readme-base-dsl:instance)
       doc
       :open-form-handler
       (lambda(form-symbol form-properties content)
	 (declare (ignore content))
	 (if (cl-html-readme-base-dsl:equal-symbol form-symbol 'heading)
	     (progn
	       (cl-html-readme-base-dsl:open-form
		tree-builder form-symbol
		(set-indentation-level form-properties))
	       (setf level (+ 1 level))
	       :decrement-level)
	     (progn
	       (cl-html-readme-base-dsl:open-form tree-builder form-symbol form-properties)
	       nil)))
       :close-form-handler
       (lambda(context)
	 (if (eq context :decrement-level)
	     (setf level (+ -1 level)))
	 (cl-html-readme-base-dsl:close-form tree-builder))
       :text-handler
       (lambda(str) (cl-html-readme-base-dsl:add-text tree-builder str)))
      (cl-html-readme-base-dsl:get-tree tree-builder))))

;;
;; Compilation of DSL to intermediate DSL
;;

(defun compile-documentation (documentation)
  "Compile a documentation object that follows the syntax of the DSL to
   the intermediate DSL represention. The intermediate representation is parsed by
   the HTML backend to generate to final HTML output."
  ;; Validate against dsl
  (cl-html-readme-base-dsl:validate-documentation (cl-html-readme-dsl:instance) documentation)
  ;; Compile to intermediate-dsl. Temporary documentation objects may not validate
  (setf documentation (expand-toc documentation))
  (setf documentation (set-heading-indentation-levels documentation))
  ;; Validate against intermediate-dsl
  (cl-html-readme-base-dsl:validate-documentation (cl-html-readme-intermediate-dsl:instance) documentation)
  documentation)

