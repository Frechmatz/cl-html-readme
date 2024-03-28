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
   :doc-to-html
   :read-stream
   :read-file
   :read-string
   :make-path))

(defpackage :cl-html-readme-dsl
  (:use :cl)
  (:export
   ;; Tree-Walker
   :tree-walker
   :default-tree-walker
   :on-open-form
   :on-close-form
   :on-text
   :walk-tree
   ;; Tree-Builder
   :dsl-tree-builder-error
   :tree-builder
   :default-tree-builder
   :open-form
   :close-form
   :add-text
   :get-tree))

(defpackage :cl-html-readme-public-dsl
  (:use :cl)
  (:export
   :walk-tree
   :make-tree-builder
   :compile-documentation))

(defpackage :cl-html-readme-intermediate-dsl
  (:use :cl)
  (:export
   :walk-tree
   :make-tree-builder))
