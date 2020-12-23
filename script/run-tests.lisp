(load "init-ql")
(asdf:load-system "cl-html-readme/test" :force t)
(in-package :cl-html-readme-test)
(format t "~%Running tests...~%")
(setf lisp-unit:*print-failures* t)
;;(use-debugger)
(run-tests)

