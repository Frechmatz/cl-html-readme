(defsystem :cl-readme-make-readme
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-readme"
  :description "Readme generation utilities"
  :long-description "Readme generation utilities"
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "variables")
				     (:file "html-writer")
				     (:file "readme-util")))
	       (:module "make-readme"
			:serial t
			:components ((:file "packages")
				     (:file "make-readme")))))

