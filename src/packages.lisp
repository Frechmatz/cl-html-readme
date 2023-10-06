(defpackage :cl-html-readme
  (:use :cl)
  (:export
   :*home-directory*
   :*tab-width*
   :doc-to-html
   :read-file
   :make-path))

(defpackage :cl-html-readme-dsl
  (:use :cl)
  (:export
   :semantic-p
   :heading-p
   :toc-p
   :toc-root-p
   :toc-item-p
   :toc-container-p
   :toc-heading-p
   :dsl-syntax-error
   :walk-tree
   :make-tree-builder
   :open-element
   :close-element
   :add-text
   :get-tree
   :write-toc))

