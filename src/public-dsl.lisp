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
;; <toc> ::= (toc <toc-properties>)
;;
;; <semantic-properties> ::= (:name <string> [:app <object>])
;;
;; <heading-properties> ::= (:name <string> [:toc t | nil] [:app <object>])
;;
;; <toc-properties> ::= ([:app <object>])
;;
;; <string> ::= A string literal
;;

(defparameter *dsl-forms*
  '((:name "SEMANTIC"
     :mandatory-properties (:name)
     :optional-properties (:app))
    (:name "HEADING"
     :mandatory-properties (:name)
     :optional-properties (:app))
    (:name "TOC"
     :mandatory-properties ()
     :optional-properties (:app))))

(defun get-dsl-form (form-symbol)
  (if (not (symbolp form-symbol))
      nil
      (let ((name (symbol-name form-symbol)))
	(find-if (lambda(e) (string= name (getf e :name))) *dsl-forms*))))

(defun is-special-form (form-symbol expected-form-symbol)
  (let ((form-definition (get-dsl-form expected-form-symbol)))
    (if (not form-definition)
	nil
	(string= (symbol-name form-symbol) (getf form-definition :name)))))


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

(defun walk-tree (documentation &key open-form-handler close-form-handler text-handler)
  "Validating traversal of a documentation object"
  (let ((validating-open-form-handler
	  (if open-form-handler
	      (lambda (form-symbol form-properties content)
		(validate-form form-symbol form-properties)
		(funcall open-form-handler form-symbol form-properties content))
	      (lambda (form-symbol form-properties content)
		(declare (ignore content))
		(validate-form form-symbol form-properties)
		nil))))
    (let ((walker
	    (make-instance
	     'cl-html-readme-dsl:default-tree-walker
	     :open-form-handler validating-open-form-handler
	     :close-form-handler close-form-handler
	     :text-handler text-handler)))
      (cl-html-readme-dsl:walk-tree walker documentation))))

;;
;; Tree-Builder
;;

(defclass tree-builder (cl-html-readme-dsl:default-tree-builder) ())

(defmethod cl-html-readme-dsl:open-form
    ((instance tree-builder) form-symbol form-properties)
  (validate-form form-symbol form-properties)
  (call-next-method))

(defun make-tree-builder ()
  "Instantiates a validating tree-builder"
  (let ((builder (make-instance 'tree-builder)))
    builder))

;;
;; Documentation validation
;;

(defun validate (documentation)
  "Validate a documentation object against the public DSL."
  (walk-tree
   documentation
   :open-form-handler nil
   :close-form-handler nil
   :text-handler nil))

;;
;; Compilation helper functions
;;

(defun make-non-validating-tree-builder ()
  "Internal helper function to instantiate a tree builder that follows the syntax as defined
   by cl-html-readme-dsl but does not apply any additional validations.
   During the compilation from public to intermediate, temporary objects
   are created that neither follow the syntax of the public DSL nor the syntax of
   the intermediate DSL."
  (make-instance 'cl-html-readme-dsl:default-tree-builder))

(defun walk-non-validating-tree (documentation &key open-form-handler close-form-handler text-handler)
  "Internal helper function to traverse a tree that follows the syntax as defined by
   cl-html-readme-dsl but does not apply any additional validations.
   During the compilation from public to intermediate, temporary objects
   are created and traversed that neither follow the syntax of the public DSL nor the syntax of
   the intermediate DSL."
  (let ((walker
	  (make-instance
	   'cl-html-readme-dsl:default-tree-walker
	   :open-form-handler open-form-handler
	   :close-form-handler close-form-handler
	   :text-handler text-handler)))
    (cl-html-readme-dsl:walk-tree walker documentation)))

;;
;; TOC-Processing
;;

(defun get-toc-headings (doc)
  "Returns a documentation object representing the toc heading tree"
  (flet ((is-toc-heading (form-symbol form-properties)
	   (and (is-special-form form-symbol 'heading) (getf form-properties :toc))))
    (let ((tree-builder (make-non-validating-tree-builder)))
      (walk-non-validating-tree
       doc
       :open-form-handler
       (lambda(form-symbol form-properties content)
	 (declare (ignore content))
	 (if (is-toc-heading form-symbol form-properties)
	     (progn
	       (cl-html-readme-dsl:open-form
		tree-builder
		form-symbol
		form-properties)
	       t)
	     nil))
       :close-form-handler
       (lambda(context)
	 (if context
	     (cl-html-readme-dsl:close-form tree-builder)))
       :text-handler
       (lambda(str)
	 (declare (ignore str))
	 nil))
      (cl-html-readme-dsl:get-tree tree-builder))))

(defun write-toc (doc toc-properties tree-builder)
  "Extracts toc and writes toc-root, toc-container, toc-item forms into the builder.
  - toc-properties: The properties of the corresponding toc-form"
  (flet ((remove-toc-property (properties)
	   (filter-property-list-entries properties :key-blacklist (list :toc))))
    (let ((toc-headings (get-toc-headings doc)))
      (if toc-headings
	  (progn
	    ;; Render toc-root
	    (cl-html-readme-dsl:open-form
	     tree-builder
	     'toc-root
	     toc-properties)
	    ;; Render toc content
	    (walk-non-validating-tree
	     toc-headings
	     :text-handler (lambda(str) (declare (ignore str)) nil)
	     :open-form-handler
	     (lambda(form-symbol form-properties content)
	       (declare (ignore form-symbol))
	       (if (not content)
		   (progn
		     ;; Heading does not have sub-headings. Render a plain toc-item.
		     (cl-html-readme-dsl:open-form
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
		     (cl-html-readme-dsl:open-form
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
	       (cl-html-readme-dsl:close-form tree-builder)))
	    ;; Close toc-root
	    (cl-html-readme-dsl:close-form tree-builder))))))

(defun expand-toc (doc)
  "Replace toc form with toc-root. Returns a new documentation object."
  (let ((tree-builder (make-non-validating-tree-builder)))
    (walk-non-validating-tree
     doc
     :open-form-handler
     (lambda(form-symbol form-properties content)
       (declare (ignore content))
       (if (is-special-form form-symbol 'toc)
	   (progn
	     (write-toc doc form-properties tree-builder)
	     :ignore-close-form)
	   (progn
	     (cl-html-readme-dsl:open-form tree-builder form-symbol form-properties)
	     t)))
     :close-form-handler
     (lambda(context)
       (if (not (eq context :ignore-close-form))
	   (cl-html-readme-dsl:close-form tree-builder)))
     :text-handler
     (lambda(str) (cl-html-readme-dsl:add-text tree-builder str)))
    (cl-html-readme-dsl:get-tree tree-builder)))

;;
;; Heading-Ids
;;

(defun set-heading-ids (doc)
  "Assign ids to toc-headings. Returns a new documentation object."
  (let ((id-store nil)
	(tree-builder (make-non-validating-tree-builder)))
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
      (walk-non-validating-tree
       doc
       :open-form-handler
       (lambda(form-symbol form-properties content)
	 (declare (ignore content))
	 (if (getf form-properties :toc)
	     (cl-html-readme-dsl:open-form tree-builder form-symbol (set-id form-properties))
	     (cl-html-readme-dsl:open-form tree-builder form-symbol form-properties))
	 nil)
       :close-form-handler
       (lambda(context) (declare (ignore context)) (cl-html-readme-dsl:close-form tree-builder))
       :text-handler
       (lambda(str) (cl-html-readme-dsl:add-text tree-builder str)))
      (cl-html-readme-dsl:get-tree tree-builder))))


;;
;; Heading-Indentation
;;

(defun set-heading-indentation-levels (doc)
  "Set indentation levels of heading forms. Returns a new documentation object."
  (let ((level 0)
	(tree-builder (make-non-validating-tree-builder)))
    (labels ((set-indentation-level (properties)
	       (let ((l (copy-list properties)))
		 (setf (getf l :indentation-level) level)
		 l)))
      (walk-non-validating-tree
       doc
       :open-form-handler
       (lambda(form-symbol form-properties content)
	 (declare (ignore content))
	 (if (is-special-form form-symbol 'heading)
	     (progn
	       (cl-html-readme-dsl:open-form
		tree-builder form-symbol
		(set-indentation-level form-properties))
	       (setf level (+ 1 level))
	       :decrement-level)
	     (progn
	       (cl-html-readme-dsl:open-form tree-builder form-symbol form-properties)
	       nil)))
       :close-form-handler
       (lambda(context)
	 (if (eq context :decrement-level)
	     (setf level (+ -1 level)))
	 (cl-html-readme-dsl:close-form tree-builder))
       :text-handler
       (lambda(str) (cl-html-readme-dsl:add-text tree-builder str)))
      (cl-html-readme-dsl:get-tree tree-builder))))

;;
;; Compilation of public DSL to intermediate DSL
;;

(defun compile-documentation (documentation)
  "Compile a documentation object that follows the syntax of the public DSL to
   the intermediate DSL represention. The intermediate representation is parsed by
   the HTML backend to generate to final HTML output."
  (validate documentation)
  ;; Compile to intermediate representation
  (setf documentation (set-heading-ids documentation))
  (setf documentation (expand-toc documentation))
  (setf documentation (set-heading-indentation-levels documentation))
  ;; During the compilation phases temporary representations of the
  ;; final documentation object are created that do not necessarily follow the syntax
  ;; of the intermediate DSL. Therefore before returning the documentation oject
  ;; validate it to catch any compilation related issues.
  (cl-html-readme-intermediate-dsl:validate documentation)
  documentation)

