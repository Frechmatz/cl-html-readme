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
  "Deterministic stringification of an object following the syntax of the DSL.
   Uses the DSL tree walker, which is supposed to have been tested."
  (labels
      ((format-item (item)
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
	       (cl-html-readme-dsl::walk-tree
		doc
		:open-element
		(lambda(element-symbol element-properties content)
		  (declare (ignore content))
		  (funcall print-space)
		  (format buffer "(~a (" (format-item element-symbol))
		  (print-plist-content element-properties)
		  (format buffer ")"))
		:close-element
		(lambda(context)
		  (declare (ignore context))
		  (format buffer ")"))
		:text
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
