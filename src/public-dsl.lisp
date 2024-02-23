(in-package :cl-html-readme-public-dsl)

(defun validate (doc)
  "Validate a documentation object"
  (cl-html-readme-dsl::walk-tree
   doc
   :close-element (lambda(context) (declare (ignore context)) nil)
   :open-element (lambda(element-symbol element-properties content)
		   (declare (ignore content))
		   (cl-html-readme-dsl::validate-element element-symbol element-properties))
   :text (lambda(text) (cl-html-readme-dsl::validate-text text))))
