(in-package :cl-html-readme-dsl)

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
		   (if (not (stringp l))
		       (error
			'cl-html-readme:syntax-error
			:format-control "Item must be a string: ~a"
			:format-arguments (list l)))
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
  ((pre-open-element-handler
    :initform (lambda(element-symbol element-properties)
		(declare (ignore element-symbol element-properties))
		nil))))

(defgeneric open-element (tree-builder element-symbol element-properties))
(defgeneric close-element (tree-builder))
(defgeneric add-text (tree-builder text))
(defgeneric get-tree (tree-builder))

(defun set-pre-open-element-handler (builder handler)
  (setf (slot-value builder 'pre-open-element-handler) handler))


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
  (funcall (slot-value instance 'pre-open-element-handler) element-symbol element-properties)
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

(defun make-tree-builder ()
  (make-instance 'tree-builder-v1))

