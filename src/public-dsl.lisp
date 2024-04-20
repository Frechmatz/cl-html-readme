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

(defclass dsl (cl-html-readme-dsl-util:specialized-dsl) ())

(defmethod cl-html-readme-dsl-util:signal-syntax-error
    ((instance dsl) format-control format-arguments)
  (apply #'format t (concatenate 'string "~%" format-control "~%") format-arguments)
  (error
   'cl-html-readme:syntax-error
   :format-control format-control
   :format-arguments format-arguments))

(defparameter *dsl-definition*
  (progn
    (let ((instance (make-instance 'dsl)))
      ;; semantic
      (cl-html-readme-dsl-util:register-special-form
       instance 'semantic (list :name) (list :app))
      ;; heading
      (cl-html-readme-dsl-util:register-special-form
       instance 'heading (list :name) (list :app :toc))
      ;; toc
      (cl-html-readme-dsl-util:register-special-form
       instance 'toc nil (list :app))
      instance)))

(defun is-special-form (form-symbol expected-form-symbol)
  (cl-html-readme-dsl-util:is-special-form *dsl-definition* form-symbol expected-form-symbol))

(defun validate-form (form-symbol form-properties)
  (cl-html-readme-dsl-util:validate-special-form *dsl-definition* form-symbol form-properties))

(defun is-supported-special-form-property (form-symbol property-keyword)
  (cl-html-readme-dsl-util:is-supported-special-form-property
   *dsl-definition*
   form-symbol
   property-keyword))

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
  (format t "~%Validating against public DSL...")
  (walk-tree
   documentation
   :open-form-handler nil
   :close-form-handler nil
   :text-handler nil)
  (format t "~%Validation against public DSL has succeeded"))

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
	   (cl-html-readme-plist-util:filter-property-list-entries
	    properties
	    (lambda(keyword) (not (eq keyword :toc))))))
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

;; TODO Validate that each heading element has an :id property
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
  "Assign an id to all heading elements. Returns a new documentation object."
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
	 (if (is-special-form form-symbol 'heading)
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
	       (format t "~%set-heading-indentation-levels::Found heading")
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
;;
;;

(defun clean-heading-ids (doc)
  "Remove ids of heading forms that are not marked as toc relevant"
  (let ((tree-builder (make-non-validating-tree-builder)))
    (walk-non-validating-tree
     doc
     :open-form-handler
     (lambda(form-symbol form-properties content)
       (declare (ignore content))
       (if (not (is-special-form form-symbol 'heading))
	   ;; Not a heading element => Pass through
	   (cl-html-readme-dsl:open-form
	    tree-builder
	    form-symbol
	    form-properties)
	   (progn
	     (if (cl-html-readme-plist-util:has-property form-properties :toc)
		 (progn
		   ;; Heading element which is toc relevant => Pass through
		   (cl-html-readme-dsl:open-form
		    tree-builder
		    form-symbol
		    form-properties))
		 (progn
		   ;; Heading element that is not toc relevant => Remove :id property
		   (cl-html-readme-dsl:open-form
		    tree-builder
		    form-symbol
		    (cl-html-readme-plist-util:filter-property-list-entries
		     form-properties
		     (lambda (keyword)
		       (not (eq :id keyword))))))))))
     :close-form-handler
     (lambda(context) (declare (ignore context)) (cl-html-readme-dsl:close-form tree-builder))
     :text-handler
     (lambda(str) (cl-html-readme-dsl:add-text tree-builder str)))
    (cl-html-readme-dsl:get-tree tree-builder)))


;;
;; Final form property clean up and validation
;;

(defun clean-and-validate (doc)
  "Walk tree, and clean up
   <ul>
     <li>Remove all properties that are not supported by dsl-intermediate</li>
     <li>Remove ids of headings that are not marked as toc relevant</li>
   </ul>"
  (let ((tree-builder (cl-html-readme-intermediate-dsl:make-tree-builder)))
    (walk-non-validating-tree
     doc
     :open-form-handler
     (lambda(form-symbol form-properties content)
       (declare (ignore content))
       (cl-html-readme-dsl:open-form
	tree-builder
	form-symbol
	(cl-html-readme-plist-util:filter-property-list-entries
	 form-properties
	 (lambda (keyword)
	   (cl-html-readme-intermediate-dsl:is-supported-special-form-property
	    form-symbol
	    keyword))))
       nil)
     :close-form-handler
     (lambda(context) (declare (ignore context)) (cl-html-readme-dsl:close-form tree-builder))
     :text-handler
     (lambda(str) (cl-html-readme-dsl:add-text tree-builder str)))
    (cl-html-readme-dsl:get-tree tree-builder)))

;;
;; Compilation of public DSL to intermediate DSL
;;

(defun compile-documentation (documentation)
  "Compile a documentation object that follows the syntax of the public DSL to
   the intermediate DSL represention. The intermediate representation is parsed by
   the HTML backend to generate to final HTML output."
  (format t "~%Compiling public-dsl to intermediate-dsl...")
  (validate documentation)
  ;; Compile (to a non-validating temporary representation)
  (setf documentation (set-heading-ids documentation))
  (setf documentation (expand-toc documentation))
  (setf documentation (set-heading-indentation-levels documentation))
  (setf documentation (clean-heading-ids documentation))
  ;; Final clean-up and validation
  (format t "~%Cleaning up properties and validating against intermediate DSL...")
  (setf documentation (clean-and-validate documentation))
  (format t "~%Compilation to intermediate-dsl and validation has succeeded~%")
  documentation)


