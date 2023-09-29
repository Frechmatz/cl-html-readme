(in-package :cl-html-readme-test)

(define-test test-tree-builder-1 ()
	     (let ((builder (cl-html-readme-dsl:make-tree-builder)))
	       (cl-html-readme-dsl:add-text builder "1")
	       (cl-html-readme-dsl:add-text builder "2")
	       (let ((tree (cl-html-readme-dsl:get-tree builder))
		     (expected-tree '("1" "2")))
		 (let ((tree-str (doc-to-string tree))
		       (expected-tree-str (doc-to-string expected-tree)))
		   (assert-equal expected-tree-str tree-str)))))

(define-test test-tree-builder-2 ()
	     (let ((builder (cl-html-readme-dsl:make-tree-builder)))
	       (cl-html-readme-dsl:open-element builder 'heading (list :name "heading-name"))
	       (cl-html-readme-dsl:close-element builder)
	       (let ((tree (cl-html-readme-dsl:get-tree builder))
		     (expected-tree '((heading (:name "heading-name")))))
		 (let ((tree-str (doc-to-string tree))
		       (expected-tree-str (doc-to-string expected-tree)))
		   (assert-equal expected-tree-str tree-str)))))

(define-test test-tree-builder-3 ()
	     (let ((builder (cl-html-readme-dsl:make-tree-builder)))
	       (cl-html-readme-dsl:add-text builder "1")
	       (cl-html-readme-dsl:open-element builder 'heading (list :name "heading-name"))
	       (cl-html-readme-dsl:close-element builder)
	       (cl-html-readme-dsl:add-text builder "2")
	       (let ((tree (cl-html-readme-dsl:get-tree builder))
		     (expected-tree '("1" (heading (:name "heading-name")) "2")))
		 (let ((tree-str (doc-to-string tree))
		       (expected-tree-str (doc-to-string expected-tree)))
		   (assert-equal expected-tree-str tree-str)))))

(define-test test-tree-builder-4 ()
	     (let ((builder (cl-html-readme-dsl:make-tree-builder)))
	       (cl-html-readme-dsl:add-text builder "1")
	       (cl-html-readme-dsl:open-element builder 'heading (list :name "heading-name"))
	       (cl-html-readme-dsl:add-text builder "1.1")
	       (cl-html-readme-dsl:close-element builder)
	       (cl-html-readme-dsl:add-text builder "2")
	       (let ((tree (cl-html-readme-dsl:get-tree builder))
		     (expected-tree '("1" (heading (:name "heading-name") "1.1") "2")))
		 (let ((tree-str (doc-to-string tree))
		       (expected-tree-str (doc-to-string expected-tree)))
		   (assert-equal expected-tree-str tree-str)))))

(define-test test-tree-builder-unbalanced-tree ()
	     (let ((builder (cl-html-readme-dsl:make-tree-builder)))
	       (cl-html-readme-dsl:open-element builder 'heading (list :name "heading-name"))
	       (let ((catched-error nil))
		 (handler-case
		     (cl-html-readme-dsl:get-tree builder)
		   (error (err)
		     (setf catched-error err)))
		 (assert-true catched-error))))

(define-test test-tree-builder-invalid-text-1 ()
	     (let ((builder (cl-html-readme-dsl:make-tree-builder)))
	       (let ((catched-error nil))
		 (handler-case
		     (cl-html-readme-dsl:add-text builder :text-1)
		   (error (err)
		     (setf catched-error err)))
		 (assert-true catched-error))))

(define-test test-tree-builder-invalid-text-2 ()
	     (let ((builder (cl-html-readme-dsl:make-tree-builder)))
	       (let ((catched-error nil))
		 (handler-case
		     (cl-html-readme-dsl:add-text builder nil)
		   (error (err)
		     (setf catched-error err)))
		 (assert-true catched-error))))

(define-test test-tree-builder-invalid-element ()
	     (let ((builder (cl-html-readme-dsl:make-tree-builder)))
	       (let ((catched-error nil))
		 (handler-case
		     (cl-html-readme-dsl:open-element builder 'xxxx (:name "name"))
		   (error (err)
		     (setf catched-error err)))
		 (assert-true catched-error))))
