(defsystem :cl-html-readme-test
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-html-readme"
  :description "A HTML Documentation Generator for Common Lisp projects."
  :long-description "A HTML Documentation Generator for Common Lisp projects."
  :depends-on (:lisp-unit)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "dsl")
			     (:file "readme-util")
			     (:file "html-writer")))
	       (:module "test"
		:serial t
		:components ((:file "packages")
			     (:file "util")
			     (:file "walk-tree-test")
			     (:file "tree-builder-test")
			     (:file "extract-toc-test")
			     (:file "html-writer-test")))))

