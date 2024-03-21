(in-package :cl-html-readme-public-dsl)

;;
;; Public DSL of cl-html-readme:
;;
;; <documentation> ::= ({ <string> | <semantic> | <heading> | <toc> | <toc-root> })
;;
;; <semantic> ::= (semantic <semantic-properties> { <string> | <heading> | <toc> | <toc-root> })
;;
;; <heading> ::= (heading <heading-properties> { <string> | <heading> | <toc> | <toc-root> })
;;
;; <toc> ::= (toc <toc-properties>) ;; High level representation of <toc-root> element
;;
;; <semantic-properties> ::= (:name <string> {:<keyword> <value>})
;;
;; <heading-properties> ::= (:name <string> [:toc t | nil] {:<keyword> <value>})
;;
;; <toc-properties> ::= ({:<keyword> <value>})
;;
;; <string> ::= A string literal
;;

(defparameter *dsl-forms*
  '((:name "SEMANTIC" :mandatory-properties (:name))
    (:name "HEADING" :mandatory-properties (:name))
    (:name "TOC" :mandatory-properties ())))

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
;; Validation

(defun validate-form (form-symbol form-properties)
  (let ((form-definition (get-dsl-form form-symbol)))
    (if (not form-definition)
	(progn
	  (format
	   t
	   "~%cl-html-readme-public-dsl: Unknown DSL form '~a'~%"
	   form-symbol)
	  (error
	   'cl-html-readme:syntax-error
	   :format-control
	   "cl-html-readme-public-dsl: Unknown DSL form '~a'"
	   :format-arguments (list form-symbol))))
    (dolist (key (getf form-definition :mandatory-properties))
      (if (not (getf form-properties key))
	  (error
	   'cl-html-readme:syntax-error
	   :format-control "Mandatory property ~a missing for form ~a"
	   :format-arguments (list key form-symbol))))
    nil))

;;
;; Tree-Walker
;;

(defun make-tree-walker (&key open-form-handler close-form-handler text-handler)
  (make-instance
   'cl-html-readme-dsl::tree-walker-lambda
   :open-form-handler
   (lambda (form-symbol form-properties content)
     ;; TODO Concept how to minimize costly validations
     (validate-form form-symbol form-properties)
     (funcall open-form-handler form-symbol form-properties content))
   :close-form-handler close-form-handler
   :text-handler text-handler))

(defun walk-tree (documentation &key open-form-handler close-form-handler text-handler)
  "Traverse a documentation object"
  (let ((walker
	  (make-tree-walker
	   :open-form-handler open-form-handler
	   :close-form-handler close-form-handler
	   :text-handler text-handler)))
    (cl-html-readme-dsl::walk-tree walker documentation)))

;;
;; Tree-Builder
;;

(defclass tree-builder (cl-html-readme-dsl::tree-builder-v1) ())

(defmethod cl-html-readme-dsl::open-element
    ((instance tree-builder) form-symbol form-properties)
  (validate-form form-symbol form-properties)
  (call-next-method))

(defun make-tree-builder ()
  (let ((builder (make-instance 'tree-builder)))
    builder))

;;
;; TOC-Processing
;;

(defun get-toc-headings (doc)
  "Returns a documentation object representing the toc heading tree"
  (flet ((is-toc-heading (element-symbol element-properties)
	   (and (heading-p element-symbol) (getf element-properties :toc))))
    (let ((tree-builder (make-tree-builder)))
      (walk-tree
       doc
       :open-form-handler
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
       :close-form-handler
       (lambda(context)
	 (if context
	     (cl-html-readme-dsl::close-element tree-builder)))
       :text-handler
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
	    (walk-tree
	     toc-headings
	     :text-handler (lambda(str) (declare (ignore str)) nil)
	     :open-form-handler
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
	     :close-form-handler
	     (lambda(context)
	       (declare (ignore context))
	       (cl-html-readme-dsl::close-element tree-builder)))
	    ;; Close toc-root
	    (cl-html-readme-dsl::close-element tree-builder))))))

(defun expand-toc (doc)
  "Replace toc element with toc-root. Returns a new documentation object."
  (let ((tree-builder (cl-html-readme-intermediate-dsl::make-tree-builder)))
    (walk-tree
     doc
     :open-form-handler
     (lambda(element-symbol element-properties content)
       (declare (ignore content))
       (if (toc-p element-symbol)
	   (progn
	     (write-toc doc element-properties tree-builder)
	     :ignore-close-element)
	   (progn
	     (cl-html-readme-dsl::open-element tree-builder element-symbol element-properties)
	     t)))
     :close-form-handler
     (lambda(context)
       (if (not (eq context :ignore-close-element))
	   (cl-html-readme-dsl::close-element tree-builder)))
     :text-handler
     (lambda(str) (cl-html-readme-dsl::add-text tree-builder str)))
    (cl-html-readme-dsl::get-tree tree-builder)))

;;
;; Heading-Ids
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
      (walk-tree
       doc
       :open-form-handler
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (if (getf element-properties :toc)
	     (cl-html-readme-dsl::open-element tree-builder element-symbol (set-id element-properties))
	     (cl-html-readme-dsl::open-element tree-builder element-symbol element-properties))
	 nil)
       :close-form-handler
       (lambda(context) (declare (ignore context)) (cl-html-readme-dsl::close-element tree-builder))
       :text-handler
       (lambda(str) (cl-html-readme-dsl::add-text tree-builder str)))
      (cl-html-readme-dsl::get-tree tree-builder))))


;;
;; Heading-Indentation
;;

(defun set-heading-indentation-levels (doc)
  "Set indentation levels of heading elements. Returns a new documentation object."
  (let ((level 0) (tree-builder (cl-html-readme-intermediate-dsl:make-tree-builder)))
    (labels ((set-indentation-level (properties)
	       (let ((l (copy-list properties)))
		 (setf (getf l :level) level)
		 l)))
      (cl-html-readme-intermediate-dsl:walk-tree
       doc
       :open-form-handler
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (if (heading-p element-symbol)
	     (progn
	       (cl-html-readme-dsl::open-element
		tree-builder element-symbol
		(set-indentation-level element-properties))
	       (setf level (+ 1 level))
	       :decrement-level)
	     (progn
	       (cl-html-readme-dsl::open-element tree-builder element-symbol element-properties)
	       nil)))
       :close-form-handler
       (lambda(context)
	 (if (eq context :decrement-level)
	     (setf level (+ -1 level)))
	 (cl-html-readme-dsl::close-element tree-builder))
       :text-handler
       (lambda(str) (cl-html-readme-dsl::add-text tree-builder str)))
      (cl-html-readme-dsl::get-tree tree-builder))))

;;
;; Compilation of public DSL to intermediate DSL
;;

(defun compile-documentation (documentation)
  (setf documentation (set-heading-ids documentation))
  (setf documentation (expand-toc documentation))
  (setf documentation (set-heading-indentation-levels documentation))
  documentation)

