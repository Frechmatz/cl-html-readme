(in-package :cl-html-readme-base-dsl)

;;
;; Shared Low-Level DSL
;;
;; <documentation> ::= ({ <string> | <special-form> })
;;
;; <special-form> ::= (symbol <form-properties> { <string> | <special-form> })
;;
;; <form-properties> ::= A property list
;;
;; <string> ::= A string literal
;;


(define-condition syntax-error (simple-error)()
  (:documentation "Signalled when a documentation object does not conform to the DSL specification,    e.g. undefined DSL special forms, missing mandatory DSL special form properties,
   unsupported DSL special form properties."))

(define-condition unbalanced-tree-error (simple-error)()
  (:documentation "Signalled when a documentation object to be programmatically build has opened a DSL special form but not closed it or a DSL special form is being closed but has not been opened."))


;;
;; Validation util
;;

(defclass default-validation-util (cl-html-readme-validation:validation-util)
  ())

(defmethod cl-html-readme-validation:reject ((instance default-validation-util) format-control format-arguments)
  (let ((error (make-instance
		'syntax-error
		:format-control format-control
		:format-arguments format-arguments)))
    (format t "~%Error: ~a~%" error)
    (error error)))

(defparameter *default-validation-util* (make-instance 'default-validation-util))

;;
;; Pass-Through property validator
;;

(defclass all-good-property-validator (cl-html-readme-validation:validator)
  ()
  (:documentation "A validator that does not apply any checks"))

(defmethod cl-html-readme-validation:validate ((instance all-good-property-validator) validation-util object)
  (declare (ignore validation-util object))
  nil)

(defparameter *default-property-validator* (make-instance 'all-good-property-validator))

;;
;; 
;;

(defun get-symbol-name (form-symbol)
  (string-upcase (symbol-name form-symbol)))

;;
;;
;;

(defclass dsl ()
  ()
  (:documentation "DSL"))

(defgeneric get-special-form-validator (dsl form-name)
  (:documentation "Returns an instance of property-validator or nil of the form-symbol
   is not supported by the DSL. The function has the following parameters:
   <ul><li>form-name Name of the special form converted to upper-case</li></ul>"))

(defgeneric make-validation-util (dsl)
  (:documentation "Create an instance of validation-util."))

(defgeneric walk (dsl documentation
			  &key open-form-handler close-form-handler text-handler
			  &allow-other-keys)
  (:documentation "Traversal of a documentation object. The function has the following parameters:
   <ul>
   <li>open-form-handler: A function that is called when a DSL special form is opened. Can be nil.
     <code>(lambda (form-symbol form-properties content))</code>
    Returns a context that is passed to on-close-form</li>
   <li>close-form-handler: A function that is called when a previously opened DSL special
    form is closed. Can be nil. <code>(lambda (context))</code></li>
   <li>text-handler: A function that is called for plain string occurences outside of
    DSL special form properties. Can be nil. <code>(lambda (text))</code></li>
  </ul>"))

(defgeneric make-builder (dsl)
  (:documentation "Returns a tree builder instance"))

(defmethod get-special-form-validator ((instance dsl) form-name)
  (declare (ignore form-name))
  *default-property-validator*)

(defmethod make-validation-util ((instance dsl))
  *default-validation-util*)

(defun equal-symbol (x y)
  "Returns t if both symbols have the same name according to the symbol-to-name conversion
   internally applied by DSL."
  (string= (get-symbol-name x) (get-symbol-name y)))

(defun validate-documentation (dsl documentation)
  "Validate a documentation object against the DSL."
  (walk
   dsl
   documentation
   :open-form-handler nil
   :close-form-handler nil
   :text-handler nil))

;;
;; Internal validation functions
;;

(defun signal-fatal-error (format-control format-arguments)
  (let ((error (make-instance
		'syntax-error
		:format-control format-control
		:format-arguments format-arguments)))
    (format t "~%Error: ~a~%" error)
    (error error)))

(defun validate-special-form (dsl form-symbol form-properties)
  "Check if the given form-symbol is a symbol and that the form-properties
   are a list. If these conditions are met then get and invoke
   a property validator."
  (if (not (symbolp form-symbol))
      (signal-fatal-error
	"FATAL ERROR: Not a symbol: ~a"
	(list form-symbol)))
  (if (not (listp form-properties))
      (signal-fatal-error
	"FATAL ERROR: Properties of special form '~a' are not a list: ~a"
	(list form-symbol form-properties)))
  (let ((validator (get-special-form-validator dsl form-symbol)))
    (if (not validator)
	(signal-fatal-error
	 "FATAL ERROR: Unsupported special form '~a'"
	 (list form-symbol))))
  (cl-html-readme-validation:validate
   (get-special-form-validator dsl (get-symbol-name form-symbol))
   (make-validation-util dsl)
   form-properties))

(defun validate-text-node (dsl text)
  "Validate if the given text node is a string"
  (declare (ignore dsl))
  (if (not (stringp text))
      (signal-fatal-error
	"FATAL ERROR: Text node must be a string: ~a"
	(list text))))

;;
;; Tree traversal NG
;;

(defmethod walk
    ((instance dsl) documentation
     &key open-form-handler close-form-handler text-handler &allow-other-keys)
  (labels
      ((walk-tree-impl (l)
	 (if (not (listp l))
	     (progn
	       (validate-text-node instance l)
	       (if text-handler (funcall text-handler l)))
	     (progn
	       (let ((form-symbol (first l))
		     (form-properties (second l))
		     (content (rest (rest l))))
		 (validate-special-form
		  instance
		  form-symbol
		  form-properties)
		 (let ((context
			 (if open-form-handler
			     (funcall open-form-handler
				      form-symbol
				      form-properties
				      content)
			     nil)))
		   (dolist (item content)
		     (walk-tree-impl item))
		   (if close-form-handler
		       (funcall close-form-handler context))))))))
    (dolist (item documentation)
      (walk-tree-impl item))
    nil))

;;
;; Tree Builder
;;

(defclass tree-builder () ())

(defgeneric open-form (tree-builder form-symbol form-properties)
  (:documentation "Open a DSL special form."))

(defgeneric close-form (tree-builder)
  (:documentation "Close a DSL special form."))

(defgeneric add-text (tree-builder text)
  (:documentation "Add a plain string."))

(defgeneric to-tree (tree-builder)
  (:documentation "Get the resulting tree."))

;;
;; Default implementation of tree-builder
;;

(defclass default-tree-builder (tree-builder)
  ((dsl :initarg :dsl :documentation "An instance of class Dsl")
   (node-stack :initform nil)
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
	 'unbalanced-tree-error
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
  (validate-special-form (slot-value instance 'dsl) form-symbol form-properties)
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
  (validate-text-node (slot-value instance 'dsl) text)
  (if (not (stringp text))
      (error
       'syntax-error
       :format-control "Text must be a string: ~a"
       :format-arguments (list text)))
  (let ((node (make-instance 'dsl-text-node :text text))
	(stack-pointer (first (slot-value instance 'node-stack))))
    (push-content stack-pointer node))
  nil)

(defmethod to-tree ((instance default-tree-builder))
  "Generate resulting tree"
  (if (not (eq 1 (length (slot-value instance 'node-stack))))
      (error
       'unbalanced-tree-error
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

;;
;;
;;

(defmethod make-builder ((instance dsl))
  (make-instance 'default-tree-builder :dsl instance))

;;
;;
;;

(defparameter *instance* (make-instance 'dsl))

(defun instance()
  *instance*)

