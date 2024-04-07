(in-package :cl-html-readme-intermediate-dsl)

;;
;; Intermediate DSL of cl-html-readme
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
;; <semantic-properties> ::= (:name <string> [:app <object>])
;;
;; <heading-properties> ::= (:name <string> :indentation-level <integer> [:toc t | nil] [:app <object>])
;;
;; <toc-item-properties> ::= (:name <string> [:app <object>])
;;
;; <toc-container-properties> ::= (:name <string> [:app <object>])
;;
;; <toc-root-properties> ::= ([:app <object>])
;;
;; <string> ::= A string literal
;;

(defparameter *dsl-forms*
  '((:name "SEMANTIC"
     :mandatory-properties (:name)
          :optional-properties (:app))
    (:name "HEADING"
     :mandatory-properties (:name :indentation-level)
     :optional-properties (:app))
    (:name "TOC-ROOT"
     :mandatory-properties ()
     :optional-properties (:app))
    (:name "TOC-ITEM"
     :mandatory-properties (:name)
     :optional-properties (:app))
    (:name "TOC-CONTAINER"
     :mandatory-properties (:name)
     :optional-properties (:app))))

(defun get-dsl-form (form-symbol)
  (if (not (symbolp form-symbol))
      nil
      (let ((name (symbol-name form-symbol)))
	(find-if (lambda(e) (string= name (getf e :name))) *dsl-forms*))))

(defun is-special-form (form-symbol expected-form-symbol)
  (let ((form-definition (get-dsl-form expected-form-symbol)))
    (if (not form-definition)
	nil
	(string= (symbol-name form-symbol) (getf form-definition :name)))))

;;
;; Validation
;;

(defun validate-form (form-symbol form-properties)
  (let ((form-definition (get-dsl-form form-symbol)))
    (if (not form-definition)
	(progn
	  (format
	   t
	   "~%cl-html-readme-intermediate-dsl: Unknown DSL form '~a'~%"
	   form-symbol)
	  (error
	   'cl-html-readme:syntax-error
	   :format-control
	   "cl-html-readme-intermediate-dsl: Unknown DSL form '~a'"
	   :format-arguments (list form-symbol))))
    (dolist (key (getf form-definition :mandatory-properties))
      (if (not (getf form-properties key))
	  (progn
	    (format
	     t
	     "~%cl-html-readme-intermediate-dsl::validate-form failed. Missing mandatory property ~a~% in properties of form ~a"
	     key form-symbol)
	    (error
	     'cl-html-readme:syntax-error
	     :format-control "Mandatory property ~a missing for form ~a"
	     :format-arguments (list key form-symbol)))))
    nil))

;;
;; Tree-Walker
;;

(defun walk-tree (documentation &key open-form-handler close-form-handler text-handler)
  "Validating traversal of a documentation object"
  (let ((validating-open-form-handler
	  (if open-form-handler
	      (lambda (form-symbol form-properties content)
		(validate-form form-symbol form-properties)
		(funcall open-form-handler form-symbol form-properties content))
	      (lambda (form-symbol form-properties content)
		(declare (ignore content))
		(validate-form form-symbol form-properties)
		nil))))
    (let ((walker
	    (make-instance
	     'cl-html-readme-dsl:default-tree-walker
	     :open-form-handler validating-open-form-handler
	     :close-form-handler close-form-handler
	     :text-handler text-handler)))
      (cl-html-readme-dsl:walk-tree walker documentation))))

;;
;; Tree-Builder
;;

(defclass tree-builder (cl-html-readme-dsl:default-tree-builder) ())

(defmethod cl-html-readme-dsl:open-form
    ((instance tree-builder) form-symbol form-properties)
  (validate-form form-symbol form-properties)
  (call-next-method))

(defun make-tree-builder ()
  "Instantiates a validating tree-builder"
  (let ((builder (make-instance 'tree-builder)))
    builder))

;;
;; Documentation validation
;;

(defun validate (documentation)
  "Validate a documentation object against the intermediate DSL."
  (walk-tree
   documentation
   :open-form-handler nil
   :close-form-handler nil
   :text-handler nil))
