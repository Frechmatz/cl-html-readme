(defsystem :cl-readme-test
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-readme"
  :description "Readme generation utilities"
  :long-description "Readme generation utilities"
  :depends-on (:lisp-unit :cl-readme)
  :components ((:module "test"
			:serial t
			:components ((:file "packages")
				     (:file "tree-builder-test")))))

