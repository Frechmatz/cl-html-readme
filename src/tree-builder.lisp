(in-package :cl-readme)

;;
;; Simple and inefficient tooling
;; class for generation of list trees.
;;

(defclass tree-builder ()
  ((stack :initform nil)
   (tree :initform nil)))

(defgeneric open-node (tree-builder node))
(defgeneric close-node (tree-builder))
(defgeneric add-node (tree-builder node))
(defgeneric get-tree (tree-builder))

(defmethod open-node ((instance tree-builder) node)
  (let ((stack-pointer (first (slot-value instance 'stack)))
	(node-impl (list :container-node t :node node :child-nodes nil)))
    (if stack-pointer
	(progn
	  (setf (getf stack-pointer :child-nodes)
		(push node-impl (getf stack-pointer :child-nodes)))
	  (push node-impl (slot-value instance 'stack)))
	(progn
	  (push node-impl (slot-value instance 'tree))
	  (push node-impl (slot-value instance 'stack)))))
  nil)

(defmethod close-node ((instance tree-builder))
  (pop (slot-value instance 'stack))
  nil)

(defmethod add-node ((instance tree-builder) node)
  (let ((stack-pointer (first (slot-value instance 'stack)))
	(node-impl (list :leaf-node t :node node)))
    (if stack-pointer
	(setf (getf stack-pointer :child-nodes) (push node-impl (getf stack-pointer :child-nodes)))
	(push node-impl (slot-value instance 'tree))))
  nil)

;; Rewrite tree
(defmethod get-tree ((instance tree-builder))
  (labels ((process-node (node-impl)
	     (cond
	       ((getf node-impl :leaf-node)
		(getf node-impl :node))
	       ((getf node-impl :container-node)
		(let ((node (getf node-impl :node))
		      (child-nodes nil))
		  (dolist (child-node-impl (getf node-impl :child-nodes))
		    (push (process-node child-node-impl) child-nodes))
		  (list node child-nodes)))
	       (t
		(error (format nil "Dont know how to handle node ~a" node-impl))))))
    (let ((tree nil))
      (dolist (node-impl (slot-value instance 'tree))
	(push (process-node node-impl) tree))
      tree)))
