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

(defparameter *dsl-definition*
  (progn
    (let ((instance (make-instance 'cl-html-readme-specialized-dsl:specialized-dsl)))
      (cl-html-readme-specialized-dsl:register-special-form
       instance 'semantic (list :name) (list :app))
      (cl-html-readme-specialized-dsl:register-special-form
       instance 'heading (list :name :indentation-level) (list :app))
      (cl-html-readme-specialized-dsl:register-special-form
       instance 'toc-root nil (list :app))
      (cl-html-readme-specialized-dsl:register-special-form
       instance 'toc-item (list :name) (list :app))
      (cl-html-readme-specialized-dsl:register-special-form
       instance 'toc-container (list :name) (list :app))
      instance)))

(defun is-special-form (form-symbol expected-form-symbol)
  (cl-html-readme-specialized-dsl:is-special-form *dsl-definition* form-symbol expected-form-symbol))

(defun validate-form (form-symbol form-properties)
  (cl-html-readme-specialized-dsl:validate-special-form *dsl-definition* form-symbol form-properties))

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
