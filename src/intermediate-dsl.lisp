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

(defparameter *dsl-forms*
  '((:name "SEMANTIC" :mandatory-properties (:name))
    (:name "HEADING" :mandatory-properties (:name))
    (:name "TOC-ROOT" :mandatory-properties ())
    (:name "TOC-ITEM" :mandatory-properties (:name))
    (:name "TOC-CONTAINER" :mandatory-properties (:name))))

(defun get-dsl-form (form-symbol)
  (if (not (symbolp form-symbol))
      nil
      (let ((name (symbol-name form-symbol)))
	(find-if (lambda(e) (string= name (getf e :name))) *dsl-forms*))))

(defun semantic-p (form-symbol)
  (and (symbolp form-symbol) (string= "SEMANTIC" (symbol-name form-symbol))))

(defun heading-p (form-symbol)
  (and (symbolp form-symbol) (string= "HEADING" (symbol-name form-symbol))))

(defun toc-root-p (form-symbol)
  (and (symbolp form-symbol) (string= "TOC-ROOT" (symbol-name form-symbol))))

(defun toc-item-p (form-symbol)
  (and (symbolp form-symbol) (string= "TOC-ITEM" (symbol-name form-symbol))))

(defun toc-container-p (form-symbol)
  (and (symbolp form-symbol) (string= "TOC-CONTAINER" (symbol-name form-symbol))))

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
	     'cl-html-readme-dsl::dsl-syntax-error
	     :format-control "Mandatory property ~a missing for form ~a"
	     :format-arguments (list key form-symbol)))))
    nil))

;;
;; Tree-Walker
;;

(defun make-tree-walker (&key open-form-handler close-form-handler text-handler)
  (make-instance
   'cl-html-readme-dsl::tree-walker-lambda
   :open-form-handler
   (lambda (form-symbol form-properties content)
     ;; TODO Concept how to minimize costly validations
     (validate-form form-symbol form-properties)
     (funcall open-form-handler form-symbol form-properties content))
   :close-form-handler close-form-handler
   :text-handler text-handler))

(defun walk-tree (documentation &key open-form-handler close-form-handler text-handler)
  "Traverse a documentation object"
  (let ((walker
	  (make-tree-walker
	   :open-form-handler open-form-handler
	   :close-form-handler close-form-handler
	   :text-handler text-handler)))
    (cl-html-readme-dsl::walk-tree walker documentation)))

;;
;; Tree-Builder
;;

(defclass tree-builder (cl-html-readme-dsl::tree-builder-v1) ())

(defmethod cl-html-readme-dsl::open-element
    ((instance tree-builder) form-symbol form-properties)
  (validate-form form-symbol form-properties)
  (call-next-method))

(defun make-tree-builder ()
  (let ((builder (make-instance 'tree-builder)))
    builder))

