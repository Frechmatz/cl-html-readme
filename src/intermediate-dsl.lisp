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

(defun semantic-p (element)
  (and (symbolp element) (string= "SEMANTIC" (symbol-name element))))

(defun heading-p (element)
  (and (symbolp element) (string= "HEADING" (symbol-name element))))

(defun toc-root-p (element)
  (and (symbolp element) (string= "TOC-ROOT" (symbol-name element))))

(defun toc-item-p (element)
  (and (symbolp element) (string= "TOC-ITEM" (symbol-name element))))

(defun toc-container-p (element)
  (and (symbolp element) (string= "TOC-CONTAINER" (symbol-name element))))

;;
;; Validation
;;

(defun validate-form (form-symbol form-properties)
  (let ((form-definition (get-dsl-form form-symbol)))
    (if (not form-definition)
	(progn
	  (format
	   t
	   "~%cl-html-readme-intermediate-dsl::validate-form failed ~a ~a~%"
	   form-symbol form-properties)
	  (error
	   'cl-html-readme-dsl::dsl-syntax-error
	   :format-control "cl-html-readme-public-dsl::validate-form failed: ~a ~a"
	   :format-arguments (list form-symbol form-properties))))
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

(defun validate (doc)
  "Validate a documentation object"
  (cl-html-readme-dsl::walk-tree
   doc
   :close-element (lambda(context) (declare (ignore context)) nil)
   :open-element (lambda(form-symbol form-properties content)
		   (declare (ignore content))
		   (validate-form form-symbol form-properties))
   :text (lambda(text)
	   (declare (ignore text))
	   nil)))

;;
;; Tree-Builder
;;

(defclass tree-builder (cl-html-readme-dsl::tree-builder-v1) ())

(defmethod cl-html-readme-dsl::validate-element
    ((instance tree-builder)
     form-symbol form-properties)
  (validate-form form-symbol form-properties))

(defun make-tree-builder ()
  (let ((builder (make-instance 'tree-builder)))
    builder))
