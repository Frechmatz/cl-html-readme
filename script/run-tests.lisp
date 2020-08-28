(load "init-ql")
(asdf:load-system "cl-readme-test" :force t)
(in-package :cl-readme-test)
(format t "~%Running tests...~%")
(setf lisp-unit:*print-failures* t)
;;(use-debugger)
(run-tests)

