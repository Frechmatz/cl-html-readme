(in-package :cl-html-readme-test)

(defun dsl-to-string (doc)
  "Deterministic stringification of an object following the syntax of the documentation DSL.
   Uses the DSL tree walker, which is supposed to be tested.
   In no means a generic list serializer."
  (labels
      ((format-item (item)
	 (cond
	   ((keywordp item)
	    (format nil ":~a" (string-downcase (symbol-name item))))
	   ((stringp item)
	    (format nil "\"~a\"" item))
	   ((numberp item)
	    (format nil "~a" item))
	   ((listp item)
	    (error "DSL-Stringification: List not supported"))
	   ((symbolp item)
	    (format nil "~a" (string-downcase (symbol-name item))))
	   ((not item)
	    (format nil "NIL"))
	   (t
	    (format nil "t"))))
       (get-plist-keys (plist)
	 (let ((keys nil) (push-key t))
	   (dolist (item plist)
	     (if push-key
		 (push item keys))
	     (setf push-key (not push-key)))
	   keys))
       (get-plist-keys-sorted (plist)
	 (sort
	  (get-plist-keys plist)
	  (lambda (a b)
	    (string-lessp
	     (format-item a)
	     (format-item b))))))
    (let ((buffer (make-string-output-stream)))
      (cl-html-readme-dsl:walk-tree
       doc
       :open-element
       (lambda(element-symbol element-properties content)
	 (declare (ignore content))
	 (format buffer " (~a (" (format-item element-symbol))
	 (let ((first-keyword t))
	   (dolist (key (get-plist-keys-sorted element-properties))
	     (if (not first-keyword)
		 (format buffer " "))
	     (setf first-keyword nil)
	     (format buffer "~a" (format-item key))
	     (format buffer " ~a" (format-item (getf element-properties key)))))
	 (format buffer ")"))
       :close-element
       (lambda(context)
	 (declare (ignore context))
	 (format buffer ")"))
       :text
       (lambda(str)
	 (format buffer "~a" (format-item str))))
      (get-output-stream-string buffer))))



;;
;; Test stuff
;;

(defun dsl-to-string-examples ()
  (let ((examples
	  (list
	   ;; Not supported yet by tree-walker: plain string
	   ;; (list
	   ;;   :name "Plain string"
	   ;;   :doc "Ollimaus")

	   (list
	    :name "A list containing a single string"
	    :doc (list "Ollimaus"))

	   ;; Not supported yet by tree-walker: Doc object is DSL special form
	   ;; (list
	   ;;  :name "A heading"
	   ;;  :doc `(heading (:toc t :name "Heading 1")))
	   
	   (list
	    :name "A string and then a DSL special form"
	    :doc `("Intro" (heading (:toc t :name "Heading 1"))))

	   (list
	    :name "A string and then a DSL special form (alphabetically sorted properties)"
	    :doc `("Intro" (heading (:d "D" :a "A" :z "Z"))))

	   (list
	    :name "A string and then a DSL special form (property type handling)"
	    :doc `("Intro" (heading (:string "D" :number 123456 :keyword :a-keyword :boolean t))))
	   
	   ;; Not supported yet by tree-walker: Doc object is DSL special form
	   ;; (list
	   ;;  :name "A heading with embedded content"
	   ;;  :doc `(heading (:toc t :name "Heading 1") "CONTENT"))

	   ;; Not supported yet by tree-walker: Doc object is DSL special form
	   ;; (list
	   ;;  :name "A heading with follow-up content"
	   ;;  :doc `((heading (:toc t :name "Heading 1")) "CONTENT"))

	   ;; Not supported yet by tree-walker: Doc object is DSL special form
	   ;; (list
	   ;;  :name "Nested headings"
	   ;;  :doc `((heading (:toc t :name "Heading 1")
	   ;; 		    (heading (:name "Heading 1.1)") "CONTENT"))))

	   ;; Not supported yet by tree-walker: Doc object is DSL special form
	   ;; (list
	   ;;  :name "A heading with id XXXXX"
	   ;;  :doc `(heading (:toc t :id "XXXXX")))

	   )))
    (format t "~%~%Stringifying examples")
    (dolist (example examples)
      (format t "~%~%Formatting example: ~a" (getf example :name))
      (format t "~%Output:~a" (dsl-to-string (getf example :doc))))
    (format t "~%DONE~%")))

    
;;(dsl-to-string-examples)

  
