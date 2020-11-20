(defsystem :cl-html-readme-make-readme
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-html-readme"
  :description "Readme generation utilities"
  :long-description "Readme generation utilities"
  :depends-on (:docparser)
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "dsl")
			     (:file "readme-util")
			     (:file "html-writer")))
	       (:module "make-readme"
		:serial t
		:components ((:file "packages")
			     (:file "make-readme")))))

