(in-package :cl-html-readme-test)

(defun get-property-list-keys (plist)
  "Get the keys of a property list"
  (let ((keys nil) (push-key t))
    (dolist (item plist)
      (if push-key
	  (push item keys))
      (setf push-key (not push-key)))
    keys))

(defun doc-to-string (doc &key (string-enclosure-character "'"))
  "Deterministic stringification of a documentation object.
   Does not apply validation.
   Assumes that public and intermediate documentation represention share the
   same structure as defined by cl-html-readme-dsl.
   Assumes that the tree walker has been tested."
  (labels
      ((walk-tree (tree &key open-form-handler close-form-handler text-handler)
	 "Non-Validating tree traversal"
	 (let ((walker 
		 (make-instance
		  'cl-html-readme-dsl:default-tree-walker
		  :open-form-handler open-form-handler
		  :close-form-handler close-form-handler
		  :text-handler text-handler)))
	   (cl-html-readme-dsl:walk-tree walker tree)))
       (format-item (item)
	 "Format an item. Formats a list as nil when it is empty and t when it is not empty"
	 (cond
	   ((keywordp item)
	    (format nil ":~a" (string-downcase (symbol-name item))))
	   ((stringp item)
	    (format nil "~a~a~a" string-enclosure-character item string-enclosure-character))
	   ((numberp item)
	    (format nil "~a" item))
	   ((symbolp item)
	    (format nil "~a" (string-downcase (symbol-name item))))
	   ((not item)
	    (format nil "nil"))
	   (t
	    (format nil "t"))))
       (get-plist-keys-sorted (plist)
	 (sort
	  (get-property-list-keys plist)
	  (lambda (a b)
	    (string-lessp
	     (format-item a)
	     (format-item b))))))
    (let ((buffer (make-string-output-stream)))
      (labels
	  ((make-space-printer ()
	     (let ((first-space t))
	       (lambda ()
		 (if (not first-space)
		     (format buffer " "))
		 (setf first-space nil))))
	   (print-plist-content (plist)
	     (let ((print-space (make-space-printer)))
	       (dolist (key (get-plist-keys-sorted plist))
		 (funcall print-space)
		 (format buffer "~a" (format-item key))
		 (funcall print-space)
		 (format buffer "~a" (format-item (getf plist key))))))
	   (print-doc-content ()
	     (let ((print-space (make-space-printer)))
	       (walk-tree
		doc
		:open-form-handler
		(lambda(form-symbol form-properties content)
		  (declare (ignore content))
		  (funcall print-space)
		  (format buffer "(~a (" (format-item form-symbol))
		  (print-plist-content form-properties)
		  (format buffer ")"))
		:close-form-handler
		(lambda(context)
		  (declare (ignore context))
		  (format buffer ")"))
		:text-handler
		(lambda(str)
		  (funcall print-space)
		  (format buffer "~a" (format-item str)))))))
	(handler-case
	    (progn
	      (print-doc-content)
	      (format nil "(~a)" (get-output-stream-string buffer)))
	  (error (err)
	    (progn
	      (format t "~%Error while stringifying doc object:~%~a~%Doc:~%~a~%" err doc)
	      (error err))))))))

(defun doc-to-html (doc)
  "Render to HTML. Omit newlines."
  (let ((cl-html-readme::*print-newline*
	  (lambda (stream)
	    (declare (ignore stream))
	    nil)))
    (cl-html-readme:doc-to-html nil doc)))
