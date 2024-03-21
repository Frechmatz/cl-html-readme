(in-package :cl-html-readme-dsl)

;;
;; DSL-Tree Walker
;;

(defclass tree-walker () ()
  (:documentation "DSL traverser."))

(defgeneric on-open-form (tree-walker form-symbol form-properties content)
  (:documentation
   "Called when a DSL special form is opened.
    Returns a context that is passed to on-close-form"))

(defgeneric on-close-form (tree-walker context)
  (:documentation
   "Called when a previously opened DSL special form is closed."))

(defgeneric on-text (tree-walker text)
  (:documentation
   "Called for each text form."))

(defun walk-tree (instance tree) 
  "DSL tree traversal. The function has the following arguments:
   <ul>
   <li>instance An instance of tree-walker./li>
   <li>tree An object following the syntax of the DSL./li>
   </ul>"
  (labels ((walk-tree-impl (l)
	     (if (not (listp l))
		 (progn
		   (if (not (stringp l))
		       (error
			'cl-html-readme:syntax-error
			:format-control "Item must be a string: ~a"
			:format-arguments (list l)))
		   (on-text instance l))
		 (progn
		   (let* ((form-symbol (first l))
			  (form-properties (second l)))
		     (let* ((content (rest (rest l)))
			    (context (on-open-form
				      instance 
				      form-symbol
				      form-properties
				      content)))
		       (dolist (item content)
			 (walk-tree-impl item))
		       (on-close-form instance context)))))))
    (dolist (item tree)
      (walk-tree-impl item))
    nil))

;;
;; Tree-Walker implementation with lambda handlers
;;

(defclass tree-walker-lambda (tree-walker)
  ((open-form-handler :initarg :open-form-handler)
   (close-form-handler :initarg :close-form-handler)
   (text-handler :initarg :text-handler))
  (:documentation "DSL traverser with lambda callbacks."))

(defmethod on-open-form ((instance tree-walker-lambda) form-symbol form-properties content)
  (funcall (slot-value instance 'open-form-handler) form-symbol form-properties content))

(defmethod on-close-form ((instance tree-walker-lambda) context)
  (funcall (slot-value instance 'close-form-handler) context))

(defmethod on-text ((instance tree-walker-lambda) text)
  (funcall (slot-value instance 'text-handler) text))

;;
;; DSL-Tree builder
;;

(define-condition dsl-tree-builder-error (simple-error)())

(defclass tree-builder () ())

(defgeneric open-form (tree-builder form-symbol form-properties))
(defgeneric close-form (tree-builder))
(defgeneric add-text (tree-builder text))
(defgeneric get-tree (tree-builder))

;;
;; Version 1 of tree-builder
;;

(defclass tree-builder-v1 (tree-builder)
  ((node-stack :initform nil)
   (root-node :initform nil)))

(defclass dsl-form-node ()
  ((form-symbol :initarg :form-symbol)
   (form-properties :initarg :form-properties)
   (content :initform (list))))

(defun push-content (dsl-form-node item)
  (assert (typep dsl-form-node 'dsl-form-node))
  (let ((l (slot-value dsl-form-node 'content)))
    (setf (slot-value dsl-form-node 'content) (push item l)))
  nil)
    
(defun push-stack (tree-builder-v1 item)
  (assert (typep item 'dsl-form-node))
  (let ((l (slot-value tree-builder-v1 'node-stack)))
    (setf (slot-value tree-builder-v1 'node-stack) (push item l))))

(defun pop-stack (tree-builder-v1)
  (let ((stack (slot-value tree-builder-v1 'node-stack)))
    (if (not (< 1 (length stack)))
	(error
	 'dsl-tree-builder-error
	 :format-control "Stack underflow. Unbalanced open/close-form calls."
	 :format-arguments nil))
    (let ((r (rest stack)))
      (setf (slot-value tree-builder-v1 'node-stack) r))))

(defclass dsl-text-node ()
  ((text :initarg :text)))

(defmethod initialize-instance :after ((instance tree-builder-v1) &rest init-args)
  (declare (ignore init-args))
  (let ((node (make-instance 'dsl-form-node :form-symbol 'root :form-properties nil)))
    (setf (slot-value instance 'root-node) node)
    (setf (slot-value instance 'node-stack) (list node))))

(defmethod open-form ((instance tree-builder-v1) form-symbol form-properties)
  (let ((node (make-instance
	       'dsl-form-node
	       :form-symbol form-symbol
	       :form-properties form-properties))
	(stack-pointer (first (slot-value instance 'node-stack))))
    (push-content stack-pointer node)
    (push-stack instance node))
  nil)

(defmethod close-form ((instance tree-builder-v1))
  (pop-stack instance)
  nil)

(defmethod add-text ((instance tree-builder-v1) text)
  (if (not (stringp text))
      (error
       'cl-html-readme:syntax-error
       :format-control "Text must be a string: ~a"
       :format-arguments (list text)))
  (let ((node (make-instance 'dsl-text-node :text text))
	(stack-pointer (first (slot-value instance 'node-stack))))
    (push-content stack-pointer node))
  nil)

(defmethod get-tree ((instance tree-builder-v1))
  "Generate resulting tree"
  (if (not (eq 1 (length (slot-value instance 'node-stack))))
      (error
       'dsl-tree-builder-error
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

