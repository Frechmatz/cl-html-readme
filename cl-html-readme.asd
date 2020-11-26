(defsystem :cl-html-readme
  :serial t
  :version "1.0.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-html-readme"
  :description "A HTML Documentation Generator for Common Lisp projects."
  :long-description "A HTML Documentation Generator for Common Lisp projects."
  :components ((:module "src"
			:serial t
			:components ((:file "packages")
				     (:file "dsl")
				     (:file "readme-util")
				     (:file "html-writer")))))

