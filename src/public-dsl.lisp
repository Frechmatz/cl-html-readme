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

(defun semantic-p (form-symbol)
  (and (symbolp form-symbol) (string= "SEMANTIC" (symbol-name form-symbol))))

(defun heading-p (form-symbol)
  (and (symbolp form-symbol) (string= "HEADING" (symbol-name form-symbol))))

(defun toc-p (form-symbol)
  (and (symbolp form-symbol) (string= "TOC" (symbol-name form-symbol))))

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

(defclass tree-walker-lambda (cl-html-readme-dsl:default-tree-walker)
  ((open-form-handler :initarg :open-form-handler)
   (close-form-handler :initarg :close-form-handler)
   (text-handler :initarg :text-handler))
  (:documentation "DSL traverser with lambda callbacks."))

(defmethod cl-html-readme-dsl:on-open-form ((instance tree-walker-lambda) form-symbol form-properties content)
  (funcall (slot-value instance 'open-form-handler) form-symbol form-properties content))

(defmethod cl-html-readme-dsl:on-close-form ((instance tree-walker-lambda) context)
  (funcall (slot-value instance 'close-form-handler) context))

(defmethod cl-html-readme-dsl:on-text ((instance tree-walker-lambda) text)
  (funcall (slot-value instance 'text-handler) text))

(defun make-tree-walker (&key open-form-handler close-form-handler text-handler)
  (make-instance
   'tree-walker-lambda
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
    (cl-html-readme-dsl:walk-tree walker documentation)))

;;
;; Tree-Builder
;;

(defclass tree-builder (cl-html-readme-dsl:default-tree-builder) ())

(defmethod cl-html-readme-dsl:open-form
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
  (flet ((is-toc-heading (form-symbol form-properties)
	   (and (heading-p form-symbol) (getf form-properties :toc))))
    (let ((tree-builder (make-tree-builder)))
      (walk-tree
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
	    (walk-tree
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
  (let ((tree-builder (cl-html-readme-intermediate-dsl:make-tree-builder)))
    (walk-tree
     doc
     :open-form-handler
     (lambda(form-symbol form-properties content)
       (declare (ignore content))
       (if (toc-p form-symbol)
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
  (let ((level 0) (tree-builder (cl-html-readme-intermediate-dsl:make-tree-builder)))
    (labels ((set-indentation-level (properties)
	       (let ((l (copy-list properties)))
		 (setf (getf l :level) level)
		 l)))
      (cl-html-readme-intermediate-dsl:walk-tree
       doc
       :open-form-handler
       (lambda(form-symbol form-properties content)
	 (declare (ignore content))
	 (if (heading-p form-symbol)
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
  (setf documentation (set-heading-ids documentation))
  (setf documentation (expand-toc documentation))
  (setf documentation (set-heading-indentation-levels documentation))
  documentation)

