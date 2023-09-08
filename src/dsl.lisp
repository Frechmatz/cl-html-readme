(in-package :cl-html-readme-dsl)

;;
;; DSL of cl-html-readme
;;

;;
;; DSL definition:
;;
;; <documentation> ::=
;;   ({ <string> | <semantic> | <heading> | <toc> | <toc-root> })
;;
;; <semantic> ::=
;;   (semantic (<semantic-properties>) { <string> | <heading> | <toc> | <toc-root> })
;;
;; <heading> ::=
;;   (heading (<heading-properties>) { <string> | <heading> | <toc> | <toc-root> })
;;
;; <toc> ::=
;;   (toc (<toc-properties>)) ;; High level representation of <toc-root> element.
;;
;; <toc-root> ::=
;;   (toc-root (<toc-root-properties>) { <toc-item> | <toc-container> })
;;
;; <toc-item> ::=
;;   (toc-item (<toc-item-properties>))
;;
;; <toc-container> ::=
;;   (toc-container (<toc-container-properties>) { <toc-item> | <toc-container> })
;;
;; <semantic-properties> ::=
;;   :name <string> [:class <string>] [:style <string>]
;;
;; <heading-properties> ::=
;;   :name <string> [:id <string>] [:toc t | nil]
;;   [:class <string>] [:style <string>]
;;
;; <toc-properties> ::=
;;   [:root-class <string>] [:root-style <string>]
;;   [:container-class <string>] [:container-style <string>]
;;   [:item-class <string>] [:item-style <string>]
;;
;; <toc-item-properties> ::=
;;   :name <string> :id <string>
;;   [:class <string>] [:style <string>]
;;
;; <toc-container-properties> ::=
;;   :name <string> :id <string>
;;   [:class <string>] [:style <string>]
;;   [:container-class <string>] [:container-style <string>]
;;
;; <toc-root-properties> ::=
;;   [:class <string>] [:style <string>]
;;
;; <string> ::= A string literal
;;


(defparameter *dsl-elements*
  '((:name "SEMANTIC" :semantic-p t :mandatory-properties (:name))
    (:name "HEADING" :heading-p t :mandatory-properties (:name))
    (:name "TOC" :toc-p t :mandatory-properties ())
    (:name "TOC-ROOT" :toc-root-p t :mandatory-properties ())
    (:name "TOC-ITEM" :toc-item-p t :mandatory-properties (:name :id))
    (:name "TOC-CONTAINER" :toc-container-p t :mandatory-properties (:name :id))))

(defun get-dsl-element (element)
  (if (not (symbolp element))
      nil
      (let ((name (symbol-name element)))
	(find-if (lambda(e) (string= name (getf e :name))) *dsl-elements*))))

(defun semantic-p (element)
  (getf (get-dsl-element element) :semantic-p))

(defun heading-p (element)
  (getf (get-dsl-element element) :heading-p))

(defun toc-p (element)
  (getf (get-dsl-element element) :toc-p))

(defun toc-root-p (element)
  (getf (get-dsl-element element) :toc-root-p))

(defun toc-item-p (element)
  (getf (get-dsl-element element) :toc-item-p))

(defun toc-container-p (element)
  (getf (get-dsl-element element) :toc-container-p))

(defun toc-heading-p (properties)
  (getf properties :toc))

(defun validate-properties (element properties)
  (declare (ignore element properties))
  nil)

;;
;; DSL-Tree Walker
;;

(define-condition dsl-syntax-error (simple-error)())


;; TODO: Validation of tree
(defun walk-tree (documentation &key open-element close-element text)
  "Walk a DSL tree. The function has the following arguments:
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
  ;;(declare (optimize (debug 3) (speed 0) (space 0)))
  (labels ((walk-tree-impl (l)
	     ;;(declare (optimize (debug 3) (speed 0) (space 0)))
	     (if (not (listp l))
		 (progn
		   (if (not (stringp l))
		       (error
			'dsl-syntax-error
			:format-control "Not a string: ~a"
			:format-arguments (list l)))
		   (funcall text l))
		 (progn
		   (let* ((element-symbol (first l))
			  (element-properties (second l)) 
			  (dsl-element (get-dsl-element element-symbol)))
		     (if (not dsl-element)
			 (error
			  'dsl-syntax-error
			  :format-control "Not a DSL element: ~a "
			  :format-arguments (list element-symbol)))
		     (validate-properties dsl-element element-properties)
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

(defclass tree-builder ()
  ((node-stack :initform nil)
   (root-node :initform nil)))

(defgeneric open-element (tree-builder element-symbol element-properties))
(defgeneric close-element (tree-builder))
(defgeneric add-text (tree-builder text))
(defgeneric get-tree (tree-builder))

(defclass dsl-element-node ()
  ((element-symbol :initarg :element-symbol)
   (element-properties :initarg :element-properties)
   (content :initform (list))))

(defun push-content (dsl-element-node item)
  (assert (typep dsl-element-node 'dsl-element-node))
  (let ((l (slot-value dsl-element-node 'content)))
    (setf (slot-value dsl-element-node 'content) (push item l)))
  nil)
    
(defun push-stack (tree-builder item)
  (assert (typep item 'dsl-element-node))
  (let ((l (slot-value tree-builder 'node-stack)))
    (setf (slot-value tree-builder 'node-stack) (push item l))))

(defun pop-stack (tree-builder)
  (let ((stack (slot-value tree-builder 'node-stack)))
    (if (not (< 1 (length stack)))
	(error (format nil "Stack underflow. Probably unbalanced open/close-element calls.")))
    (let ((r (rest stack)))
      (setf (slot-value tree-builder 'node-stack) r))))

(defclass dsl-text-node ()
  ((text :initarg :text)))

(defmethod initialize-instance :after ((instance tree-builder) &rest init-args)
  (declare (ignore init-args))
  (let ((node (make-instance 'dsl-element-node :element-symbol 'root :element-properties nil)))
    (setf (slot-value instance 'root-node) node)
    (setf (slot-value instance 'node-stack) (list node))))

(defmethod open-element ((instance tree-builder) element-symbol element-properties)
  (if (not (get-dsl-element element-symbol))
      (error (format nil "Not a DSL element: ~a" element-symbol)))
  (let ((node (make-instance
	       'dsl-element-node
	       :element-symbol element-symbol
	       :element-properties element-properties))
	(stack-pointer (first (slot-value instance 'node-stack))))
    (push-content stack-pointer node)
    (push-stack instance node))
  nil)

(defmethod close-element ((instance tree-builder))
  (pop-stack instance)
  nil)

(defmethod add-text ((instance tree-builder) text)
  (if (not (stringp text))
      (error "Text must be a string"))
  (let ((node (make-instance 'dsl-text-node :text text))
	(stack-pointer (first (slot-value instance 'node-stack))))
    (push-content stack-pointer node))
  nil)

(defmethod get-tree ((instance tree-builder))
  "Generate resulting tree"
  (if (not (eq 1 (length (slot-value instance 'node-stack))))
      (error "Unbalanced tree"))
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
;; TOC extraction
;;

(defun extract-toc (doc tree-builder)
  "Extracts toc and pushes toc-root, toc-container, toc-item elements into the builder."
  (flet ((get-toc-properties ()
	   "Lookup toc element and return its properties."
	   (let ((toc-properties nil))
	     (cl-html-readme-dsl:walk-tree
	      doc
	      :open-element
	      (lambda(element-symbol element-properties content)
		(declare (ignore content))
		(if (toc-p element-symbol)
		    (setf toc-properties element-properties)))
	      :close-element (lambda(context) (declare (ignore context)) nil)
	      :text (lambda(str) (declare (ignore str)) nil))
	     toc-properties))
	 (has-toc-elements (doc)
	   "Recursive descent to check if doc contains toc relevant elements"
	   (let ((found nil))
	     (cl-html-readme-dsl:walk-tree
	      doc
	      :open-element
	      (lambda(element-symbol element-properties content)
		(declare (ignore element-symbol content))
		(if (getf element-properties :toc)
		    (setf found t)))
	      :close-element
	      (lambda(context) (declare (ignore context)) nil)
	      :text
	      (lambda(str) (declare (ignore str)) nil))
	     found))
	 (remove-toc-property (properties)
	   (setf properties (copy-list properties))
	   (setf (getf properties :toc) nil)
	   properties))
    (let ((toc-root-pending t) (toc-properties (get-toc-properties)))
      (cl-html-readme-dsl:walk-tree
       doc
       :open-element
       (lambda(element-symbol element-properties content)
	 (declare (ignore element-symbol))
	 (if (getf element-properties :toc)
	     (progn
	       (if toc-root-pending
		   (progn
		     (cl-html-readme-dsl:open-element
		      tree-builder
		      'toc-root
		      (list
		       :class (getf toc-properties :root-class)
		       :style (getf toc-properties :root-style)))
		     (setf toc-root-pending nil)))
	       (if (has-toc-elements content)
		   (progn
		     (cl-html-readme-dsl:open-element
		      tree-builder
		      'toc-container
		      (concatenate
		       'list
		       (list
			:class (getf toc-properties :item-class)
			:style (getf toc-properties :item-style)
			:container-class (getf toc-properties :container-class)
			:container-style (getf toc-properties :container-style))
		       (remove-toc-property element-properties)))
		      t)
		   (progn
		     (cl-html-readme-dsl:open-element
		      tree-builder
		      'toc-item
		      (concatenate
		       'list
		       (list
			:class (getf toc-properties :item-class)
			:style (getf toc-properties :item-style))
		       (remove-toc-property element-properties)))
		     t)))
	     (progn
	       nil)))
       :close-element
       (lambda(context)
	 (if context
	     (cl-html-readme-dsl:close-element tree-builder)))
       :text
       (lambda(str)
	 (declare (ignore str))
	 nil))
      (if (not toc-root-pending)
	  (cl-html-readme-dsl:close-element tree-builder))))
  nil)

(defun extract-toc-headings (doc)
  "Returns a documentation object representing the toc heading tree"
  (flet ((is-toc-heading (element-symbol element-properties)
	   (and (heading-p element-symbol) (getf element-properties :toc))))
    (let ((tree-builder (make-instance 'tree-builder)))
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


