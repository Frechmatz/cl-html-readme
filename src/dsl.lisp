(in-package :cl-html-readme-dsl)

;;
;; DSL-Tree Walker
;;

(defclass tree-walker ()
  ((open-form-handler
    :initarg :open-form-handler
    :documentation
    "Handler that is called when a DSL special form is opened.
     (lambda (form-symbol form-properties content))
    Returns a context that is passed to on-close-form")
   (close-form-handler
    :initarg :close-form-handler
    :documentation
    "Handler that is called when a previously opened DSL special form is closed.
     (lambda (context))")
   (text-handler
    :initarg :text-handler
    :documentation
    "Handler that is called for plain string occurences outside of DSL special form properties.
    (lambda (context))"))
  (:documentation "DSL traverser. All handlers are optional."))

(defgeneric walk-tree (tree-walker tree)
  (:documentation
   "Traverse tree and invoke handlers."))

(defmethod initialize-instance :after ((instance tree-walker) &key)
  "Initialize nil callback handlers with default implementations."
  (with-slots (open-form-handler close-form-handler text-handler) instance
    (if (not open-form-handler)
	(setf open-form-handler
	      (lambda (form-symbol form-properties content)
		(declare (ignore form-symbol form-properties content)))))
    (if (not close-form-handler)
	(setf close-form-handler
	      (lambda (context)
		(declare (ignore context))
		nil)))
    (if (not text-handler)
	(setf text-handler
	      (lambda (text)
		(declare (ignore text))
		nil)))))

;;
;; Default implementation of tree-walker
;;

(defclass default-tree-walker (tree-walker) ()
  (:documentation "An implementation of the tree-walker class."))

(defmethod walk-tree ((instance default-tree-walker) tree)
  (with-slots (open-form-handler close-form-handler text-handler) instance
    (labels ((walk-tree-impl (l)
	       (if (not (listp l))
		   (progn
		     (if (not (stringp l))
			 (error
			  'cl-html-readme:syntax-error
			  :format-control "Item must be a string: ~a"
			  :format-arguments (list l)))
		     (funcall text-handler l))
		   (progn
		     (let* ((form-symbol (first l))
			    (form-properties (second l)))
		       (let* ((content (rest (rest l)))
			      (context (funcall open-form-handler
					form-symbol
					form-properties
					content)))
			 (dolist (item content)
			   (walk-tree-impl item))
			 (funcall close-form-handler context)))))))
      (dolist (item tree)
	(walk-tree-impl item))
      nil)))

;;
;; DSL-Tree builder
;;

(defclass tree-builder () ())

(defgeneric open-form (tree-builder form-symbol form-properties)
  (:documentation "Open a DSL special form."))

(defgeneric close-form (tree-builder)
  (:documentation "Close a DSL special form."))

(defgeneric add-text (tree-builder text)
  (:documentation "Add a plain string."))

(defgeneric get-tree (tree-builder)
  (:documentation "Get the resulting tree."))

;;
;; Default implementation of tree-builder
;;

(defclass default-tree-builder (tree-builder)
  ((node-stack :initform nil)
   (root-node :initform nil))
  (:documentation "An implementation of the tree-builder class."))

(defclass dsl-form-node ()
  ((form-symbol :initarg :form-symbol)
   (form-properties :initarg :form-properties)
   (content :initform (list))))

(defun push-content (dsl-form-node item)
  (assert (typep dsl-form-node 'dsl-form-node))
  (let ((l (slot-value dsl-form-node 'content)))
    (setf (slot-value dsl-form-node 'content) (push item l)))
  nil)
    
(defun push-stack (default-tree-builder item)
  (assert (typep item 'dsl-form-node))
  (let ((l (slot-value default-tree-builder 'node-stack)))
    (setf (slot-value default-tree-builder 'node-stack) (push item l))))

(defun pop-stack (default-tree-builder)
  (let ((stack (slot-value default-tree-builder 'node-stack)))
    (if (not (< 1 (length stack)))
	(error
	 'cl-html-readme:unbalanced-tree-error
	 :format-control "Stack underflow. Unbalanced open/close-form calls."
	 :format-arguments nil))
    (let ((r (rest stack)))
      (setf (slot-value default-tree-builder 'node-stack) r))))

(defclass dsl-text-node ()
  ((text :initarg :text)))

(defmethod initialize-instance :after ((instance default-tree-builder) &rest init-args)
  (declare (ignore init-args))
  (let ((node (make-instance 'dsl-form-node :form-symbol 'root :form-properties nil)))
    (setf (slot-value instance 'root-node) node)
    (setf (slot-value instance 'node-stack) (list node))))

(defmethod open-form ((instance default-tree-builder) form-symbol form-properties)
  (let ((node (make-instance
	       'dsl-form-node
	       :form-symbol form-symbol
	       :form-properties form-properties))
	(stack-pointer (first (slot-value instance 'node-stack))))
    (push-content stack-pointer node)
    (push-stack instance node))
  nil)

(defmethod close-form ((instance default-tree-builder))
  (pop-stack instance)
  nil)

(defmethod add-text ((instance default-tree-builder) text)
  (if (not (stringp text))
      (error
       'cl-html-readme:syntax-error
       :format-control "Text must be a string: ~a"
       :format-arguments (list text)))
  (let ((node (make-instance 'dsl-text-node :text text))
	(stack-pointer (first (slot-value instance 'node-stack))))
    (push-content stack-pointer node))
  nil)

(defmethod get-tree ((instance default-tree-builder))
  "Generate resulting tree"
  (if (not (eq 1 (length (slot-value instance 'node-stack))))
      (error
       'cl-html-readme:unbalanced-tree-error
       :format-control "Pending open forms on stack"
       :format-arguments (list)))
  (labels ((process-node (node)
	     (if (typep node 'dsl-text-node)
		 (slot-value node 'text)
		 (progn
		   (assert (typep node 'dsl-form-node))
		   (let ((element nil))
		     (dolist (content-node (slot-value node 'content))
		       (push (process-node content-node) element))
		     (push (slot-value node 'form-properties) element)
		     (push (slot-value node 'form-symbol) element)
		     element)))))
    (let ((tree nil) (root-node (slot-value instance 'root-node)))
      (assert (typep root-node 'dsl-form-node))
      (let ((content-nodes (slot-value root-node 'content)))
	(dolist (node content-nodes)
	  (push (process-node node) tree)))
      tree)))

