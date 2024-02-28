(defsystem :cl-html-readme
  :serial t
  :version "2.1.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-html-readme"
  :description "A HTML Documentation Generator for Common Lisp projects."
  :long-description "A HTML Documentation Generator for Common Lisp projects."
  :components ((:module "src"
		:serial t
		:components ((:file "packages")
			     (:file "syntax-error")
			     (:file "dsl")
			     (:file "intermediate-dsl")
			     (:file "public-dsl")
			     (:file "readme-util")
			     (:file "html-writer"))))
  :in-order-to ((test-op (test-op "cl-html-readme/test"))))

(defsystem :cl-html-readme/test
  :serial t
  :version "2.1.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-html-readme"
  :description "Test suite of cl-html-readme"
  :long-description "Test suite of cl-html-readme"
  :depends-on (:cl-html-readme :lisp-unit)
  :components ((:module "test"
		:serial t
		:components ((:file "packages")
			     (:file "util")
			     (:file "walk-tree-test")
			     (:file "doc-to-string-test")
			     (:file "validate-test")
			     (:file "tree-builder-test")
			     (:file "extract-toc-test")
			     (:file "html-writer-test"))))
  :perform (test-op (o c) (symbol-call :lisp-unit '#:run-tests :all :cl-html-readme-test)))

(defsystem :cl-html-readme/doc
  :serial t
  :version "2.1.0"
  :licence "MIT"
  :author "Oliver <frechmatz@gmx.de>"
  :maintainer "Oliver <frechmatz@gmx.de>"
  :homepage "https://github.com/Frechmatz/cl-html-readme"
  :description "Documentation of cl-html-readme"
  :long-description "Documentation of cl-html-readme"
  :depends-on (:cl-html-readme :docparser)
  :components ((:module "make-readme/examples"
		:serial t
		:components ((:file "toc")
			     (:file "semantic")
			     (:file "escape")
			     (:file "rendering-hook")))
	       (:module "make-readme"
		:serial t
		:components ((:file "packages")
			     (:file "make-doc")))))

