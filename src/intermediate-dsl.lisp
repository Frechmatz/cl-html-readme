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


;;
;; DSL-NG
;;

(defclass dsl-ng (cl-html-readme-dsl:dsl) ())

(defparameter *semantic-validator*
  (make-instance
   'cl-html-readme-dsl:default-property-validator
   :name "cl-html-readme-intermediate-dsl:*semantic-validator*"
   :properties '((:indicator :name :mandatory :t)
		 (:indicator :app))))

(defparameter *heading-validator*
  (make-instance
   'cl-html-readme-dsl:default-property-validator
   :name "cl-html-readme-intermediate-dsl:*heading-validator*"
   :properties '((:indicator :name :mandatory :t)
		 (:indicator :indentation-level :mandatory :t)
		 (:indicator :id)
		 (:indicator :app))))
   
(defparameter *toc-root-validator*
  (make-instance
   'cl-html-readme-dsl:default-property-validator
   :name "cl-html-readme-intermediate-dsl:*toc-root-validator*"
   :properties '((:indicator :app))))

(defparameter *toc-container-validator*
  (make-instance
   'cl-html-readme-dsl:default-property-validator
   :name "cl-html-readme-intermediate-dsl:*toc-container-validator*"
   :properties '((:indicator :name :mandatory :t)
		 (:indicator :id :mandatory :t)
		 (:indicator :app))))

(defparameter *toc-item-validator*
  (make-instance
   'cl-html-readme-dsl:default-property-validator
   :name "cl-html-readme-intermediate-dsl:*toc-item-validator*"
   :properties '((:indicator :name :mandatory :t)
		 (:indicator :id :mandatory :t)
		 (:indicator :app))))

(defmethod cl-html-readme-dsl:get-special-form-validator
    ((instance dsl-ng) form-name)
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
    (t nil)))

(defparameter *dsl-ng* (make-instance 'dsl-ng))

(defun instance ()
  *dsl-ng*)


;;
;;
;;


(defclass dsl (cl-html-readme-dsl-util:specialized-dsl) ())

(defmethod cl-html-readme-dsl-util:signal-syntax-error
    ((instance dsl)
     format-control format-arguments)
  (apply #'format t (concatenate 'string "~%" format-control "~%") format-arguments)
  (error
   'cl-html-readme-dsl:syntax-error
   :format-control format-control
   :format-arguments format-arguments))
  
(defparameter *dsl-definition*
  (progn
    (let ((instance (make-instance 'dsl)))
      ;; semantic
      (cl-html-readme-dsl-util:register-special-form
       instance 'semantic (list :name) (list :app))
      ;; heading
      (cl-html-readme-dsl-util:register-special-form
       instance 'heading (list :name :indentation-level) (list :app :id))
      ;; toc-root
      (cl-html-readme-dsl-util:register-special-form
       instance 'toc-root nil (list :app))
      ;; toc-item
      (cl-html-readme-dsl-util:register-special-form
       instance 'toc-item (list :name :id) (list :app))
      ;; toc-container
      (cl-html-readme-dsl-util:register-special-form
       instance 'toc-container (list :name :id) (list :app))
      instance)))

(defun validate-form (form-symbol form-properties)
  (cl-html-readme-dsl-util:validate-special-form *dsl-definition* form-symbol form-properties))

