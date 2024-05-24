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
   :filter-properties
   :with-properties
   :sort-by-key))

(defpackage :cl-html-readme-validation
  (:use :cl)
  (:export
   :reject
   :validate
   :validator
   :property-list-validator))

(defpackage :cl-html-readme-base-dsl
  (:use :cl)
  (:export
   :dsl
   :instance
   :syntax-error
   :unbalanced-tree-error
   :property-list-validator
   :get-special-form-validator
   :walk
   ;; Builder
   :tree-builder
   :make-builder
   :open-form
   :close-form
   :add-text
   :to-tree))

(defpackage :cl-html-readme-dsl
  (:use :cl)
  (:export
   :instance))

(defpackage :cl-html-readme-target-dsl
  (:use :cl)
  (:export
   :instance))

(defpackage :cl-html-readme-dsl-compiler
  (:use :cl)
  (:export
   :compile-documentation))
