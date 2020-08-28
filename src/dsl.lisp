(in-package :cl-readme)

;;
;; <documentation> ::= ({ <string> | <semantic> | <heading> | <toc> })
;; <semantic>      ::= (semantic <properties> { <string> | <heading> | <toc> })
;; <heading>       ::= (heading <properties> { <string> | <heading> | <toc> })
;; <toc>           ::= (toc <properties>)
;; <properties>    ::= A property list
;; <string>        ::= A string literal
;; <toc-root>      ::= (toc-root <properties> { <toc-item> | <toc-container> })
;; <toc-item>      ::= (toc-item <properties>)
;; <toc-container> ::= (toc-container <properties> { <toc-item> | <toc-container> })
;;

(defparameter *dsl-elements*
  '((:name "SEMANTIC" :semantic-p t :mandatory-properties (:name))
    (:name "HEADING" :heading-p t :mandatory-properties (:name))
    (:name "TOC" :toc-p t :mandatory-properties ())
    (:name "TOC-ROOT" :toc-root-p t :mandatory-properties ())
    (:name "TOC-ITEM" :toc-item-p t :mandatory-properties (:name :id))
    (:name "TOC-CONTAINER" :toc-container-p t :mandatory-properties (:name :id))))

(defun get-dsl-element (element)
  (if (not (symbolp element))
      nil
      (let ((name (symbol-name element)))
	(find-if (lambda(e) (string= name (getf e :name))) *dsl-elements*))))

(defun semantic-p (element)
  (getf (get-dsl-element element) :semantic-p))

(defun heading-p (element)
  (getf (get-dsl-element element) :heading-p))

(defun toc-p (element)
  (getf (get-dsl-element element) :toc-p))

(defun toc-root-p (element)
  (getf (get-dsl-element element) :toc-root-p))

(defun toc-item-p (element)
  (getf (get-dsl-element element) :toc-item-p))

(defun toc-container-p (element)
  (getf (get-dsl-element element) :toc-container-p))

(defun toc-heading-p (properties)
  (getf properties :toc))

(defun validate-properties (element properties)
  (declare (ignore element properties))
  nil)

;;
;; Tree Walker
;;

(defun walk-tree (documentation &key open-element close-element text)
  (labels ((walk-tree-impl (l)
	     ;;(format t "~%walk-tree-impl called with ~a" l)
	     ;; TODO Überlegen, ob ich l gegen nil testen muss
	     (if (not (listp l))
		 (progn
		   (if (not (stringp l))
		       (error "Syntax error. Object ~a must be a string" l))
		   (funcall text l))
		 (progn
		   (let* ((element-symbol (first l))
			  (element-properties (second l)) 
			  (dsl-element (get-dsl-element element-symbol)))
		     (if (not dsl-element)
			 (error "Syntax error. Object ~a does not represent a DSL element"
				element-symbol))
		     (validate-properties dsl-element element-properties)
		     (let ((context (funcall open-element element-symbol element-properties)))
		       (dolist (item (rest (rest l)))
			 (walk-tree-impl item))
		       (funcall close-element element-symbol element-properties context)))))))
    (dolist (item documentation)
      (walk-tree-impl item))
    nil))

;;
;; Test stuff
;;

(defun test-walk-tree (documentation)
  (format t "~%~%test-walk-tree called with ~a~%" documentation)
  (walk-tree
   documentation 
   :open-element (lambda(element element-properties)
		   (format t "~%open-element: ~a ~a" element element-properties)
		   (format nil "close-context-~a" element))
   :close-element (lambda(element element-properties context)
		    (format t "~%close-element: ~a ~a ~a" element element-properties context))
   :text (lambda(str) (format t "~%text: ~a" str))))

(defun test-1 ()
  (test-walk-tree '("OLLI" "MAUSI" "BAUCHI")))

;;(test-1)


(defun test-2 ()
  (test-walk-tree '("OLLI" (heading (:name "MAUSI") "BAUCHI"))))

;;(test-2)

(defun test-3 ()
  (test-walk-tree '((heading (:name "OLLI") "MAUSI" "BAUCHI"))))

;;(test-3)

(defun test-4 ()
  (test-walk-tree '((heading (:name "OLLI") (heading (:name "MAUSI") "BAUCHI")))))

;;(test-4)


(defun test-5-no-dsl-symbol ()
  (test-walk-tree '((unknown (:name "OLLI") "MAUSI" "BAUCHI"))))

;;(test-5-no-dsl-symbol)

(defun test-6-nested-string ()
  (test-walk-tree '("OLLI" ("MAUSI" "BAUCHI"))))

;;(test-6-nested-string)

