(defsystem :cl-readme
  :serial t
  :version "0.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-readme"
  :description "Readme generation utilities"
  :long-description "Readme generation utilities"
  :depends-on (:cl-synthesizer)
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "readme-util")))))

