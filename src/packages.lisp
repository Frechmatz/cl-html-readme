(defpackage :cl-html-readme
  (:use :cl)
  (:export
   :*home-directory*
   :*tab-width*
   :doc-to-html
   :read-stream
   :read-file
   :make-path))

(defpackage :cl-html-readme-dsl
  (:use :cl))
