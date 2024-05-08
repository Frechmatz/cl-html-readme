(defpackage :cl-html-readme
  (:use :cl)
  (:export
   :*home-directory*
   :*tab-width*
   :*get-heading-attributes*
   :*get-semantic-attributes*
   :*get-toc-root-attributes*
   :*get-toc-item-attributes*
   :*get-toc-container-attributes*
   :doc-to-html
   :read-stream
   :read-file
   :read-string
   :make-path))

(defpackage :cl-html-readme-plist-util
  (:use :cl)
  (:export
   :get-property-list-keys
   :filter-property-list-entries
   :has-property))
  
(defpackage :cl-html-readme-dsl
  (:use :cl)
  (:export
   :syntax-error
   :unbalanced-tree-error
   :validation-util
   :reject
   :property-validator
   :validate
   :default-property-validator
   :dsl
   :get-special-form-validator
   :make-validation-util
   :equal-symbol
   :walk-tree-ng
   :instance
   :validate-documentation
   ;; Builder
   :make-builder
   :open-form
   :close-form
   :add-text
   :get-tree))

(defpackage :cl-html-readme-dsl-util
  (:use :cl)
  (:export
   :specialized-dsl
   :register-special-form
   :validate-special-form
   :signal-syntax-error))

(defpackage :cl-html-readme-public-dsl
  (:use :cl)
  (:export
   :instance
   :compile-documentation))

(defpackage :cl-html-readme-intermediate-dsl
  (:use :cl)
  (:export
   :instance))
