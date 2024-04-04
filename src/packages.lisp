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

(defpackage :cl-html-readme-dsl
  (:use :cl)
  (:export
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
