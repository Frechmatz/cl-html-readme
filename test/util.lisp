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
	 (cond
	   ((listp item)
	    (error "Document stringification: List not supported"))
	   ((keywordp item)
	    (format nil ":~a" (string-downcase (symbol-name item))))
	   ((stringp item)
	    (format nil "~a~a~a" string-enclosure-character item string-enclosure-character))
	   ((numberp item)
	    (format nil "~a" item))
	   ((symbolp item)
	    (format nil "~a" (string-downcase (symbol-name item))))
	   ((not item)
	    (format nil "NIL"))
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
	     (let ((first-item t))
	       (lambda ()
		 (if (not first-item)
		     (format buffer " "))
		 (setf first-item nil))))
	   (format-plist-content (plist)
	     (let ((space (make-space-printer)))
	       (dolist (key (get-plist-keys-sorted plist))
		 (funcall space)
		 (format buffer "~a" (format-item key))
		 (funcall space)
		 (format buffer "~a" (format-item (getf plist key)))))))
	(let ((space (make-space-printer)))
	    (cl-html-readme-dsl:walk-tree
	     doc
	     :open-element
	     (lambda(element-symbol element-properties content)
	       (declare (ignore content))
	       (funcall space)
	       (format buffer "(~a (" (format-item element-symbol))
	       (format-plist-content element-properties)
	       (format buffer ")"))
	     :close-element
	     (lambda(context)
	       (declare (ignore context))
	       (format buffer ")"))
	     :text
	     (lambda(str)
	       (funcall space)
	       (format buffer "~a" (format-item str))))
	    (format nil "(~a)" (get-output-stream-string buffer)))))))


;;
;; Tests
;;
#|
(defun dsl-to-string-examples ()
  (let ((examples
	  (list
	   (list
	    :name "A string"
	    :doc (list "Text"))
	   (list
	    :name "Two strings"
	    :doc (list "Text 1" "Text 2"))
	   (list
	    :name "A string and a form"
	    :doc `("Intro" (heading (:toc t :name "Heading 1"))))
	   (list
	    :name "Form properties to be sorted alphabetically"
	    :doc `((heading (:d "D" :a "A" :z "Z"))))
	   (list
	    :name "Form property values types to be recognized"
	    :doc `("Intro" (heading (:string "D" :number 123456 :keyword :a-keyword :boolean t))))
	   (list
	    :name "Nested forms"
	    :doc `((heading (:name "H1") (heading (:name "H1.1")))))
	   )))
    (format t "~%~%Stringifying examples")
    (dolist (example examples)
      (format t "~%~%Formatting example: ~a" (getf example :name))
      (format t "~%Output: >>~a<<" (doc-to-string (getf example :doc))))
    (format t "~%DONE~%")))

;;(dsl-to-string-examples)
|#
