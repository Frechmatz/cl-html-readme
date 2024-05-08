(in-package :cl-html-readme-public-dsl)

;;
;; Public DSL of cl-html-readme:
;;
;; <documentation> ::= ({ <string> | <semantic> | <heading> | <toc> | <toc-root> })
;;
;; <semantic> ::= (semantic <semantic-properties> { <string> | <heading> | <toc> | <toc-root> })
;;
;; <heading> ::= (heading <heading-properties> { <string> | <heading> | <toc> | <toc-root> })
;;
;; <toc> ::= (toc <toc-properties>)
;;
;; <semantic-properties> ::= (:name <string> [:app <object>])
;;
;; <heading-properties> ::= (:name <string> [:toc t | nil] [:app <object>])
;;
;; <toc-properties> ::= ([:app <object>])
;;
;; <string> ::= A string literal
;;


(defclass dsl (cl-html-readme-base-dsl:dsl) ())

(defparameter *semantic-validator*
  (make-instance
   'cl-html-readme-base-dsl:default-property-validator
   :name "cl-html-readme-public-dsl:*semantic-validator*"
   :properties '((:indicator :name :mandatory :t)
		 (:indicator :app))))

(defparameter *heading-validator*
  (make-instance
   'cl-html-readme-base-dsl:default-property-validator
   :name "cl-html-readme-public-dsl:*heading-validator*"
   :properties '((:indicator :name :mandatory :t)
		 (:indicator :toc)
		 (:indicator :app))))
   
(defparameter *toc-validator*
  (make-instance
   'cl-html-readme-base-dsl:default-property-validator
   :name "cl-html-readme-public-dsl:*toc-validator*"
   :properties '((:indicator :app))))

(defmethod cl-html-readme-base-dsl:get-special-form-validator
    ((instance dsl) form-name)
  (cond
    ((string= "SEMANTIC" form-name)
     *semantic-validator*)
    ((string= "HEADING" form-name)
     *heading-validator*)
    ((string= "TOC" form-name)
     *toc-validator*)
    (t nil)))

(defparameter *dsl* (make-instance 'dsl))

(defun instance ()
  *dsl*)

