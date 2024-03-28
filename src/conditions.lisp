(in-package :cl-html-readme)

(define-condition syntax-error (simple-error)()
  (:documentation "Signalled when a documentation object does not conform to the DSL specification, e.g. undefined DSL special forms, missing mandatory DSL special form properties, unsupported DSL special form properties."))

(define-condition unbalanced-tree-error (simple-error)()
  (:documentation "Signalled when a documentation object to be programmatically build has opened a DSL special form but not closed it or a DSL special form is being closed but has not been opened."))


