(in-package :cl-html-readme-public-dsl)


(defparameter *dsl-forms*
  '((:name "SEMANTIC" :mandatory-properties (:name))
    (:name "HEADING" :mandatory-properties (:name))
    (:name "TOC" :mandatory-properties ())
;;    (:name "TOC-ROOT" :mandatory-properties ())
;;    (:name "TOC-ITEM" :mandatory-properties (:name))
;;    (:name "TOC-CONTAINER" :mandatory-properties (:name)))
  ))

(defun get-dsl-form (form-symbol)
  (if (not (symbolp form-symbol))
      nil
      (let ((name (symbol-name form-symbol)))
	(find-if (lambda(e) (string= name (getf e :name))) *dsl-forms*))))

(defun semantic-p (element)
  (and (symbolp element) (string= "SEMANTIC" (symbol-name element))))

(defun heading-p (element)
  (and (symbolp element) (string= "HEADING" (symbol-name element))))

(defun toc-p (element)
  (and (symbolp element) (string= "TOC" (symbol-name element))))

;;(defun toc-heading-p (properties)
;;  (getf properties :toc))


;;
;; Property list tooling
;;

(defun get-property-list-keys (plist)
  "Get the keys of a property list"
  (let ((keys nil) (push-key t))
    (dolist (item plist)
      (if push-key
	  (push item keys))
      (setf push-key (not push-key)))
    keys))

(defun filter-property-list-entries (plist &key key-blacklist)
  "Filter entries of a property list"
  (let ((keys (get-property-list-keys plist))
	(result nil))
    (dolist (key keys)
      (if (not (find key key-blacklist))
	  (progn
	    (push (getf plist key) result)
	    (push key result))))
    result))

;;
;;
;,

(defun validate-form (form-symbol form-properties)
  (let ((form-definition (get-dsl-form form-symbol)))
    (if (not form-definition)
      (error
       'cl-html-readme-dsl::dsl-syntax-error
       :format-control "Not a public DSL special form: ~a"
       :format-arguments (list form-symbol)))
    (dolist (key (getf form-definition :mandatory-properties))
      (if (not (getf form-properties key))
	  (error
	   'cl-html-readme-dsl::dsl-syntax-error
	   :format-control "Mandatory property ~a missing for form ~a"
	   :format-arguments (list key form-symbol))))
  nil))

(defun validate (doc)
  "Validate a documentation object"
  (cl-html-readme-dsl::walk-tree
   doc
   :close-element (lambda(context) (declare (ignore context)) nil)
   :open-element (lambda(form-symbol form-properties content)
		   (declare (ignore content))
		   (validate-form form-symbol form-properties))
   :text (lambda(text)
	   (declare (ignore text))
	   nil)))

(defun make-tree-builder ()
  (make-instance 'cl-html-readme-dsl::tree-builder-v1))

;;
;; TOC
;;

(defun get-toc-headings (doc)
  "Returns a documentation object representing the toc heading tree"
  (flet ((is-toc-heading (element-symbol element-properties)
	   (and (heading-p element-symbol) (getf element-properties :toc))))
    (let ((tree-builder (make-tree-builder)))
      (cl-html-readme-dsl::walk-tree
       doc
       :open-element
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (if (is-toc-heading element-symbol element-properties)
	     (progn
	       (cl-html-readme-dsl::open-element
		tree-builder
		element-symbol
		element-properties)
	       t)
	     nil))
       :close-element
       (lambda(context)
	 (if context
	     (cl-html-readme-dsl::close-element tree-builder)))
       :text
       (lambda(str)
	 (declare (ignore str))
	 nil))
      (cl-html-readme-dsl::get-tree tree-builder))))

(defun write-toc (doc toc-properties tree-builder)
  "Extracts toc and writes toc-root, toc-container, toc-item elements into the builder.
  - toc-properties: The properties of the corresponding toc-form"
  (flet ((remove-toc-property (properties)
	   (filter-property-list-entries properties :key-blacklist (list :toc))))
    (let ((toc-headings (get-toc-headings doc)))
      (if toc-headings
	  (progn
	    ;; Render toc-root
	    (cl-html-readme-dsl::open-element
	     tree-builder
	     'toc-root
	     toc-properties)
	    ;; Render toc content
	    (cl-html-readme-dsl::walk-tree
	     toc-headings
	     :text (lambda(str) (declare (ignore str)) nil)
	     :open-element
	     (lambda(element-symbol element-properties content)
	       (declare (ignore element-symbol))
	       (if (not content)
		   (progn
		     ;; Heading does not have sub-headings. Render a plain toc-item.
		     (cl-html-readme-dsl::open-element
		      tree-builder
		      'toc-item
		      (remove-toc-property
		       (concatenate
			'list
			toc-properties
			element-properties)))
		     nil)
		   (progn
		     ;; Heading has sub-headings. Render a toc-container.
		     (cl-html-readme-dsl::open-element
		      tree-builder
		      'toc-container
		      (remove-toc-property
		       (concatenate
			'list
			element-properties
			toc-properties)))
		     nil)))
	     :close-element
	     (lambda(context)
	       (declare (ignore context))
	       (cl-html-readme-dsl::close-element tree-builder)))
	    ;; Close toc-root
	    (cl-html-readme-dsl::close-element tree-builder))))))

(defun expand-toc (doc)
  "Replace toc element with toc-root. Returns a new documentation object."
  (let ((tree-builder (cl-html-readme-intermediate-dsl::make-tree-builder)))
    (cl-html-readme-dsl::walk-tree
     doc
     :open-element
     (lambda(element-symbol element-properties content)
       (declare (ignore content))
       (if (toc-p element-symbol)
	   (progn
	     (write-toc doc element-properties tree-builder)
	     :ignore-close-element)
	   (progn
	     (cl-html-readme-dsl::open-element tree-builder element-symbol element-properties)
	     t)))
       :close-element
       (lambda(context)
	 (if (not (eq context :ignore-close-element))
	     (cl-html-readme-dsl::close-element tree-builder)))
       :text
       (lambda(str) (cl-html-readme-dsl::add-text tree-builder str)))
    (cl-html-readme-dsl::get-tree tree-builder)))


;;
;;
;;

(defun set-heading-ids (doc)
  "Assign ids to toc-headings. Returns a new documentation object."
  (let ((id-store nil) (tree-builder (make-tree-builder)))
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


;;
;;
;;

(defun set-heading-indentation-levels (doc)
  "Set indentation levels of heading elements. Returns a new documentation object."
  (let ((level 0) (tree-builder (cl-html-readme-intermediate-dsl:make-tree-builder)))
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
;;
;;

(defun compile-documentation (documentation)
  (validate documentation)
  (setf documentation (set-heading-ids documentation))
  (setf documentation (expand-toc documentation))
  (setf documentation (set-heading-indentation-levels documentation))
  documentation)


  
