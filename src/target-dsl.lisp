(in-package :cl-html-readme-target-dsl)

;;
;; Target DSL of cl-html-readme representing the format of the documentation that is passed
;; to the HTML rendering backend.
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


(defclass dsl (cl-html-readme-base-dsl:dsl) ())

(defparameter *semantic-validator*
  (make-instance
   'cl-html-readme-validation:property-list-validator
   :name "cl-html-readme-target-dsl:*semantic-validator*"
   :properties '((:indicator :name :mandatory :t)
		 (:indicator :app))))

(defparameter *heading-validator*
  (make-instance
   'cl-html-readme-validation:property-list-validator
   :name "cl-html-readme-target-dsl:*heading-validator*"
   :properties '((:indicator :name :mandatory :t)
		 (:indicator :indentation-level :mandatory :t)
		 (:indicator :id)
		 (:indicator :app))))
   
(defparameter *toc-root-validator*
  (make-instance
   'cl-html-readme-validation:property-list-validator
   :name "cl-html-readme-target-dsl:*toc-root-validator*"
   :properties '((:indicator :app))))

(defparameter *toc-container-validator*
  (make-instance
   'cl-html-readme-validation:property-list-validator
   :name "cl-html-readme-target-dsl:*toc-container-validator*"
   :properties '((:indicator :name :mandatory :t)
		 (:indicator :id :mandatory :t)
		 (:indicator :app))))

(defparameter *toc-item-validator*
  (make-instance
   'cl-html-readme-validation:property-list-validator
   :name "cl-html-readme-target-dsl:*toc-item-validator*"
   :properties '((:indicator :name :mandatory :t)
		 (:indicator :id :mandatory :t)
		 (:indicator :app))))

(defmethod cl-html-readme-base-dsl:get-special-form-validator
    ((instance dsl) form-symbol)
  (let ((form-name (string-upcase (symbol-name form-symbol))))
    (cond
      ((string= "SEMANTIC" form-name)
       *semantic-validator*)
      ((string= "HEADING" form-name)
       *heading-validator*)
      ((string= "TOC-ROOT" form-name)
       *toc-root-validator*)
      ((string= "TOC-CONTAINER" form-name)
       *toc-container-validator*)
      ((string= "TOC-ITEM" form-name)
       *toc-item-validator*)
      (t nil))))

(defparameter *dsl* (make-instance 'dsl))

(defun instance ()
  *dsl*)

