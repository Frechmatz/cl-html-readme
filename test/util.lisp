(in-package :cl-html-readme-test)

(defun doc-to-string (doc &key (string-enclosure-character "'"))
  "Deterministic stringification of a documentation object.
   Does not apply validation.
   Assumes that public and target documentation represention share the
   same structure as defined by cl-html-readme-base-dsl.
   Assumes that the tree walker has been tested."
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
	    (format nil "t")))))
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
	       (cl-html-readme-plist-util:with-properties
		   (cl-html-readme-plist-util:sort-by-key plist)
		    (lambda (key value)
		      (funcall print-space)
		      (format buffer "~a" (format-item key))
		      (funcall print-space)
		      (format buffer "~a" (format-item value))))))
	   (print-doc-content ()
	     (let ((print-space (make-space-printer)))
	       (cl-html-readme-base-dsl:walk
		(cl-html-readme-base-dsl:instance)
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

(defun get-app-property (properties app-property)
  (let ((app-properties (getf properties :app)))
    (if app-properties
	(getf app-properties app-property)
	nil)))

