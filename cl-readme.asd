(defsystem :cl-readme
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
				     (:file "doc-to-html")
				     (:file "readme-util")))))

