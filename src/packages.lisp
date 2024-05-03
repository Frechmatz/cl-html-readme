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
   :syntax-error
   :unbalanced-tree-error
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
   ;; Next generation
   :syntax-error
   :validation-util
   :reject
   :property-validator
   :validate
   :default-property-validator
   :dsl
   :get-special-form-validator
   :make-validation-util
   :equal-symbol
   ;; Tree-Walker
   :tree-walker
   :default-tree-walker
   :walk-tree
   ;; Tree-Builder
   :tree-builder
   :default-tree-builder
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
   :is-special-form
   :signal-syntax-error))

(defpackage :cl-html-readme-public-dsl
  (:use :cl)
  (:export
   :walk-tree
   :make-tree-builder
   :compile-documentation
   :validate
   :is-special-form))

(defpackage :cl-html-readme-intermediate-dsl
  (:use :cl)
  (:export
   :walk-tree
   :make-tree-builder
   :validate
   :is-special-form))
