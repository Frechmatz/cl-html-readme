(defsystem :cl-html-readme
  :serial t
  :version "1.0.1"
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
				     (:file "html-writer"))))
  :in-order-to ((test-op (test-op "cl-html-readme/test"))))

(defsystem :cl-html-readme/test
  :serial t
  :version "1.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-html-readme"
  :description "Test suite for cl-html-readme"
  :long-description "Test suite for cl-html-readme"
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
			     (:file "doc-to-string-test")
			     (:file "tree-builder-test")
			     (:file "extract-toc-test"))))
  :perform (test-op (o c) (symbol-call :lisp-unit '#:run-tests :all :cl-html-readme-test)))

(defsystem :cl-html-readme/doc
  :serial t
  :version "1.0.1"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-html-readme"
  :description "Documentation sources of cl-html-readme"
  :long-description
  "Documentation sources of cl-html-readme. Documentation is created via (cl-html-readme-make-doc::make-doc)"
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
			     (:file "make-doc")))))

