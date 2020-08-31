(defsystem :cl-readme-test
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-readme"
  :description "Readme generation utilities"
  :long-description "Readme generation utilities"
  :depends-on (:lisp-unit)
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "dsl")
				     (:file "variables")
;;				     (:file "html-writer")
				     (:file "readme-util")))
	       (:module "test"
			:serial t
			:components ((:file "packages")
				     (:file "walk-tree-test")
				     (:file "tree-builder-test")))))

