(in-package :cl-html-readme-intermediate-dsl)

(defparameter *dsl-forms*
  '((:name "SEMANTIC" :mandatory-properties (:name))
    (:name "HEADING" :mandatory-properties (:name))
    (:name "TOC" :mandatory-properties ())
    (:name "TOC-ROOT" :mandatory-properties ())
    (:name "TOC-ITEM" :mandatory-properties (:name))
    (:name "TOC-CONTAINER" :mandatory-properties (:name))))

(defun get-dsl-form (form-symbol)
  (if (not (symbolp form-symbol))
      nil
      (let ((name (symbol-name form-symbol)))
	(find-if (lambda(e) (string= name (getf e :name))) *dsl-forms*))))

(defun validate-form (form-symbol form-properties)
  (let ((form-definition (get-dsl-form form-symbol)))
    (if (not form-definition)
      (error
       'cl-html-readme-dsl::dsl-syntax-error
       :format-control "Not a public DSL special form: ~a"
       :format-arguments (list form-symbol)))
    (dolist (key (getf form-definition :mandatory-properties))
      (if (not (getf form-properties key))
	  (error
	   'cl-html-readme-dsl::dsl-syntax-error
	   :format-control "Mandatory property ~a missing for form ~a"
	   :format-arguments (list key form-symbol))))
  nil))

(defun validate-text (text)
  (if (not (stringp text))
      (error
       'cl-html-readme-dsl::dsl-syntax-error
       :format-control "Text must be a string: ~a"
       :format-arguments (list text))))

(defun validate (doc)
  "Validate a documentation object"
  (cl-html-readme-dsl::walk-tree
   doc
   :close-element (lambda(context) (declare (ignore context)) nil)
   :open-element (lambda(form-symbol form-properties content)
		   (declare (ignore content))
		   (validate-form form-symbol form-properties))
   :text (lambda(text) (validate-text text))))

(defun make-tree-builder ()
  (make-instance 'cl-html-readme-dsl::tree-builder-v1))
