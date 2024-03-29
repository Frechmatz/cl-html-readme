(in-package :cl-html-readme-dsl)

;;
;; DSL of cl-html-readme
;;

;;
;; DSL definition:
;;
;; <documentation> ::= ({ <string> | <semantic> | <heading> | <toc> | <toc-root> })
;;
;; <semantic> ::= (semantic <semantic-properties> { <string> | <heading> | <toc> | <toc-root> })
;;
;; <heading> ::= (heading <heading-properties> { <string> | <heading> | <toc> | <toc-root> })
;;
;; <toc> ::= (toc <toc-properties>) ;; High level representation of <toc-root> element
;;
;; <toc-root> ::= (toc-root <toc-root-properties> { <toc-item> | <toc-container> })
;;
;; <toc-item> ::= (toc-item <toc-item-properties>)
;;
;; <toc-container> ::= (toc-container <toc-container-properties> { <toc-item> | <toc-container> })
;;
;; <semantic-properties> ::= (:name <string> {:<keyword> <value>})
;;
;; <heading-properties> ::= (:name <string> [:toc t | nil] {:<keyword> <value>})
;;
;; <toc-properties> ::= ({:<keyword> <value>})
;;
;; <toc-item-properties> ::= (:name <string> {:<keyword> <value>})
;;
;; <toc-container-properties> ::= (:name <string> {:<keyword> <value>})
;;
;; <toc-root-properties> ::= ({:<keyword> <value>})
;;
;; <string> ::= A string literal
;;


(defparameter *dsl-elements*
  '((:name "SEMANTIC" :mandatory-properties (:name))
    (:name "HEADING" :mandatory-properties (:name))
    (:name "TOC" :mandatory-properties ())
    (:name "TOC-ROOT" :mandatory-properties ())
    (:name "TOC-ITEM" :mandatory-properties (:name))
    (:name "TOC-CONTAINER" :mandatory-properties (:name))))

(defun get-dsl-element (element)
  (if (not (symbolp element))
      nil
      (let ((name (symbol-name element)))
	(find-if (lambda(e) (string= name (getf e :name))) *dsl-elements*))))

(defun semantic-p (element)
  (and (symbolp element) (string= "SEMANTIC" (symbol-name element))))

(defun heading-p (element)
  (and (symbolp element) (string= "HEADING" (symbol-name element))))

(defun toc-p (element)
  (and (symbolp element) (string= "TOC" (symbol-name element))))

(defun toc-root-p (element)
  (and (symbolp element) (string= "TOC-ROOT" (symbol-name element))))

(defun toc-item-p (element)
  (and (symbolp element) (string= "TOC-ITEM" (symbol-name element))))

(defun toc-container-p (element)
  (and (symbolp element) (string= "TOC-CONTAINER" (symbol-name element))))

(defun toc-heading-p (properties)
  (getf properties :toc))

;;
;;
;;

(define-condition dsl-syntax-error (simple-error)())

(defun validate-element (element properties)
  (let ((form-definition (get-dsl-element element)))
    (if (not form-definition)
      (error
       'dsl-syntax-error
       :format-control "Not a DSL special form: ~a"
       :format-arguments (list element)))
    (dolist (key (getf form-definition :mandatory-properties))
      (if (not (getf properties key))
	  (error
	   'dsl-syntax-error
	   :format-control "Mandatory property ~a missing for form ~a"
	   :format-arguments (list key element))))
  nil))

(defun validate-text (text)
  (if (not (stringp text))
      (error
       'dsl-syntax-error
       :format-control "Text must be a string: ~a"
       :format-arguments (list text))))

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
;; DSL-Tree Walker
;;

(defun walk-tree (documentation &key open-element close-element text)
  "DSL tree traversal. The function assumes that the documentation object conforms to
the syntax of the DSL. No validation is applied. The function has the following arguments:
   <ul>
   <li>documentation An instance of <documentation>./li>
   <li>:open-element A function that is called when a DSL element is opened.
     <p>(lambda(element-symbol element-properties content))</p>
     Returns a context that is passed to :close-element.</li>
   <li>:close-element A function that is called when a previously opened DSL element closes.
     <p>(lambda(context)) Context value as returned by open-element.</p></li>
   <li>:text A function that is called for each text node.
     <p>(lambda(str))</p></li>
   </ul>"
  (labels ((walk-tree-impl (l)
	     (if (not (listp l))
		 (progn
		   (funcall text l))
		 (progn
		   (let* ((element-symbol (first l))
			  (element-properties (second l)))
		     (let* ((content (rest (rest l)))
			    (context (funcall
				      open-element
				      element-symbol
				      element-properties
				      content)))
		       (dolist (item content)
			 (walk-tree-impl item))
		       (funcall close-element context)))))))
    (dolist (item documentation)
      (walk-tree-impl item))
    nil))

;;
;; DSL-Tree builder
;;

(define-condition dsl-tree-builder-error (simple-error)())

(defclass tree-builder ()
  ())

(defgeneric open-element (tree-builder element-symbol element-properties))
(defgeneric close-element (tree-builder))
(defgeneric add-text (tree-builder text))
(defgeneric get-tree (tree-builder))

;;
;; Version 1 of tree-builder
;;

(defclass tree-builder-v1 (tree-builder)
  ((node-stack :initform nil)
   (root-node :initform nil)))

(defclass dsl-element-node ()
  ((element-symbol :initarg :element-symbol)
   (element-properties :initarg :element-properties)
   (content :initform (list))))

(defun push-content (dsl-element-node item)
  (assert (typep dsl-element-node 'dsl-element-node))
  (let ((l (slot-value dsl-element-node 'content)))
    (setf (slot-value dsl-element-node 'content) (push item l)))
  nil)
    
(defun push-stack (tree-builder-v1 item)
  (assert (typep item 'dsl-element-node))
  (let ((l (slot-value tree-builder-v1 'node-stack)))
    (setf (slot-value tree-builder-v1 'node-stack) (push item l))))

(defun pop-stack (tree-builder-v1)
  (let ((stack (slot-value tree-builder-v1 'node-stack)))
    (if (not (< 1 (length stack)))
	(error
	 'dsl-tree-builder-error
	 :format-control "Stack underflow. Unbalanced open/close-element calls."
	 :format-arguments nil))
    (let ((r (rest stack)))
      (setf (slot-value tree-builder-v1 'node-stack) r))))

(defclass dsl-text-node ()
  ((text :initarg :text)))

(defmethod initialize-instance :after ((instance tree-builder-v1) &rest init-args)
  (declare (ignore init-args))
  (let ((node (make-instance 'dsl-element-node :element-symbol 'root :element-properties nil)))
    (setf (slot-value instance 'root-node) node)
    (setf (slot-value instance 'node-stack) (list node))))

(defmethod open-element ((instance tree-builder-v1) element-symbol element-properties)
  (validate-element element-symbol element-properties)
  (let ((node (make-instance
	       'dsl-element-node
	       :element-symbol element-symbol
	       :element-properties element-properties))
	(stack-pointer (first (slot-value instance 'node-stack))))
    (push-content stack-pointer node)
    (push-stack instance node))
  nil)

(defmethod close-element ((instance tree-builder-v1))
  (pop-stack instance)
  nil)

(defmethod add-text ((instance tree-builder-v1) text)
  (validate-text text)
  (let ((node (make-instance 'dsl-text-node :text text))
	(stack-pointer (first (slot-value instance 'node-stack))))
    (push-content stack-pointer node))
  nil)

(defmethod get-tree ((instance tree-builder-v1))
  "Generate resulting tree"
  (if (not (eq 1 (length (slot-value instance 'node-stack))))
      (error
       'dsl-tree-builder-error
       :format-control "Pending open elements on stack"
       :format-arguments (list)))
  (labels ((process-node (node)
	     (if (typep node 'dsl-text-node)
		 (slot-value node 'text)
		 (progn
		   (assert (typep node 'dsl-element-node))
		   (let ((element nil))
		     (dolist (content-node (slot-value node 'content))
		       (push (process-node content-node) element))
		     (push (slot-value node 'element-properties) element)
		     (push (slot-value node 'element-symbol) element)
		     element)))))
    (let ((tree nil) (root-node (slot-value instance 'root-node)))
      (assert (typep root-node 'dsl-element-node))
      (let ((content-nodes (slot-value root-node 'content)))
	(dolist (node content-nodes)
	  (push (process-node node) tree)))
      tree)))

;;
;;
;;

(defun make-tree-builder ()
  (make-instance 'tree-builder-v1))

;;
;; TOC
;;

(defun get-toc-headings (doc)
  "Returns a documentation object representing the toc heading tree"
  (flet ((is-toc-heading (element-symbol element-properties)
	   (and (heading-p element-symbol) (getf element-properties :toc))))
    (let ((tree-builder (make-tree-builder)))
      (walk-tree
       doc
       :open-element
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (if (is-toc-heading element-symbol element-properties)
	     (progn
	       (open-element
		tree-builder
		element-symbol
		element-properties)
	       t)
	     nil))
       :close-element
       (lambda(context)
	 (if context
	     (close-element tree-builder)))
       :text
       (lambda(str)
	 (declare (ignore str))
	 nil))
      (get-tree tree-builder))))

(defun write-toc (doc toc-properties tree-builder)
  "Extracts toc and writes toc-root, toc-container, toc-item elements into the builder.
  - toc-properties: The properties of the corresponding toc-form"
  (flet ((remove-toc-property (properties)
	   (filter-property-list-entries properties :key-blacklist (list :toc))))
    (let ((toc-headings (get-toc-headings doc)))
      (if toc-headings
	  (progn
	    ;; Render toc-root
	    (open-element
	     tree-builder
	     'toc-root
	     toc-properties)
	    ;; Render toc content
	    (walk-tree
	     toc-headings
	     :text (lambda(str) (declare (ignore str)) nil)
	     :open-element
	     (lambda(element-symbol element-properties content)
	       (declare (ignore element-symbol))
	       (if (not content)
		   (progn
		     ;; Heading does not have sub-headings. Render a plain toc-item.
		     (open-element
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
		     (open-element
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
	       (close-element tree-builder)))
	    ;; Close toc-root
	    (close-element tree-builder))))))

(defun expand-toc (doc)
  "Replace toc element with toc-root. Returns a new documentation object."
  (let ((tree-builder (make-tree-builder)))
    (walk-tree
     doc
     :open-element
     (lambda(element-symbol element-properties content)
       (declare (ignore content))
       (if (toc-p element-symbol)
	   (progn
	     (write-toc doc element-properties tree-builder)
	     :ignore-close-element)
	   (progn
	     (open-element tree-builder element-symbol element-properties)
	     t)))
       :close-element
       (lambda(context)
	 (if (not (eq context :ignore-close-element))
	     (close-element tree-builder)))
       :text
       (lambda(str) (add-text tree-builder str)))
    (get-tree tree-builder)))

;;
;;
;;

(defun validate (doc)
  "Validate a documentation object"
  (walk-tree
   doc
   :close-element (lambda(context) (declare (ignore context)) nil)
   :open-element (lambda(element-symbol element-properties content)
		   (declare (ignore content))
		   (validate-element element-symbol element-properties))
   :text (lambda(text) (validate-text text))))
