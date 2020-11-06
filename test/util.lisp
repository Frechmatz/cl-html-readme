(in-package :cl-html-readme-test)

(defun dsl-to-string (dsl)
  "Evaluate DSL and return result as a string."
  (let ((str (make-string-output-stream)))
    (cl-html-readme:doc-to-html str dsl)
    (get-output-stream-string str)))

