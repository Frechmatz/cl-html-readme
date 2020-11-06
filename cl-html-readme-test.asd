(defsystem :cl-html-readme-test
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-html-readme"
  :description "Readme generation utilities"
  :long-description "Readme generation utilities"
  :depends-on (:lisp-unit)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "dsl")
			     (:file "variables")
			     (:file "html-writer")
			     (:file "readme-util")))
	       (:module "test"
		:serial t
		:components ((:file "packages")
			     (:file "util")
			     (:file "walk-tree-test")
			     (:file "tree-builder-test")
			     (:file "extract-toc-test")
			     (:file "html-writer-test")))))

