(in-package :cl-html-readme-dsl)

;;
;; Intermediate DSL definition:
;;
;; <documentation> ::= ({ <string> | <semantic> | <heading> | <toc> | <toc-root> })
;;
;; <semantic> ::= (semantic <semantic-properties> { <string> | <heading> | <toc> | <toc-root> })
;;
;; <heading> ::= (heading <heading-properties> { <string> | <heading> | <toc> | <toc-root> })
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
;; <toc-item-properties> ::= (:name <string> {:<keyword> <value>})
;;
;; <toc-container-properties> ::= (:name <string> {:<keyword> <value>})
;;
;; <toc-root-properties> ::= ({:<keyword> <value>})
;;
;; <string> ::= A string literal
;;

#|
;; TODO Move to dsl-backend
(defparameter *dsl-elements*
  '((:name "SEMANTIC" :mandatory-properties (:name))
    (:name "HEADING" :mandatory-properties (:name))
;;    (:name "TOC" :mandatory-properties ())
    (:name "TOC-ROOT" :mandatory-properties ())
    (:name "TOC-ITEM" :mandatory-properties (:name))
    (:name "TOC-CONTAINER" :mandatory-properties (:name))))
|#


;;
;; TODO Separate backend/frontend
;;

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
;;  (format t "~%########### Walk-Tree ################ ~%")
  (labels ((walk-tree-impl (l)
		   ;; (format 
		   ;;  t
		   ;;  "~%########### Walk-Tree-Impl called with  ~a ################ ~%" l)
	     (if (not (listp l))
		 (progn
;;		   (format
;;		    t
;;		    "~%########### Walk-Tree-Impl: Not a list. calling text handler ~a ################ ~%" l)
		   (funcall text l))
		 (progn
;;		   (format
;;		    t
;;		    "~%########### Walk-Tree-Impl: Item is a list. Processing form ~a ################ ~%" l)
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
;;      (format
 ;;      t
 ;;      "~%########### Walk-Tree: Processing item ~a ################ ~%" item)
      
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
  ;; Intended for internal use. Accepts anything.
  ;;(validate-element element-symbol element-properties)
  ;; TODO writer-factory in dsl-frontend, dsl-backend with validation handler
;;  (format t "~%############## Open element called with ~a ~%" element-symbol)
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
       'dsl-syntax-error
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

;;
;;
;;

(defun make-tree-builder ()
  (make-instance 'tree-builder-v1))

