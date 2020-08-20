(defpackage :cl-readme
  (:use :cl)
  (:export
   :*home-directory*
   :*tab-width*
   :*get-heading-class*
   :*get-toc-container-class*
   :*get-toc-item-class*
   :html-writer
   :doc-to-html
   :read-verbatim
   :read-code
   :current-date
   :make-path
   :open-semantic
   :close-semantic
   :sbcl-make-function-decl ;; DEPRECATED
   :sbcl-make-function-lambda-list-str))

