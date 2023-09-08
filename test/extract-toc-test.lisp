(in-package :cl-html-readme-test)

(define-test test-extract-toc-1 ()
	     (let ((tree-builder (make-instance 'cl-html-readme-dsl:tree-builder))
		   (doc '((heading (:name "H1" :toc t))
			  (heading (:name "H2" :toc t))
			  (heading (:name "H3" :toc t)))))
	       (cl-html-readme-dsl:extract-toc doc tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc
			'((toc-root (:class nil :style nil)
			   (toc-item (:class nil :name "H1" :style nil :toc nil))
			   (toc-item (:class nil :name "H2" :style nil :toc nil))
			   (toc-item (:class nil :name "H3" :style nil :toc nil))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

(define-test test-extract-toc-2 ()
	     (let ((tree-builder (make-instance 'cl-html-readme-dsl:tree-builder))
		   (doc '((heading (:name "H1" :toc t))
			  (heading (:name "H2" :toc t)
			   (heading (:name "H2.1" :toc t))
			   (heading (:name "H2.2" :toc t)))
			  (heading (:name "H3" :toc t)))))
	       (cl-html-readme-dsl:extract-toc doc tree-builder)
	       (let ((toc (first (cl-html-readme-dsl:get-tree tree-builder))))
		 ;; p ::= <properties>
		 ;; toc ::= (toc-root p (toc-item p) (toc-container p (toc-item p) (toc-item p))
		 ;; (toc-item p))
		 (assert-true 3 (length toc))
		 (assert-true (cl-html-readme-dsl:toc-root-p (first toc)))
		 (let ((ti-1 (third toc)) (tc-2 (fourth toc)) (ti-3 (fifth toc)))
		   (assert-true (cl-html-readme-dsl:toc-item-p (first ti-1)))
		   (assert-equal "H1" (getf (second ti-1) :name))
		   (assert-true (cl-html-readme-dsl:toc-container-p (first tc-2)))
		   (assert-equal "H2" (getf (second tc-2) :name))
		   (let ((tc-2-ti-1 (third tc-2))
			 (tc-2-ti-2 (fourth tc-2)))
		     (assert-true (cl-html-readme-dsl:toc-item-p (first tc-2-ti-1)))
		     (assert-equal "H2.1" (getf (second tc-2-ti-1) :name))
		     (assert-true (cl-html-readme-dsl:toc-item-p (first tc-2-ti-2)))
		     (assert-equal "H2.2" (getf (second tc-2-ti-2) :name))
		     (assert-true (cl-html-readme-dsl:toc-item-p (first ti-3)))
		     (assert-equal "H3" (getf (second ti-3) :name)))))))

(define-test test-extract-toc-2-2 ()
	     (let ((tree-builder (make-instance 'cl-html-readme-dsl:tree-builder))
		   (doc '((heading (:name "H1" :toc t))
			  (heading (:name "H2" :toc t)
			   (heading (:name "H2.1" :toc t))
			   (heading (:name "H2.2" :toc t)))
			  (heading (:name "H3" :toc t)))))
	       (cl-html-readme-dsl:extract-toc doc tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (toc-str (doc-to-string toc))
		      (expected-toc-str
			(concatenate
			 'string
			 "((toc-root (:class nil :style nil) "
			 "(toc-item (:class nil :name 'H1' :style nil :toc nil)) "
			 "(toc-container (:class nil :container-class nil :container-style nil "
			 ":name 'H2' :style nil :toc nil)"
			 " (toc-item (:class nil :name 'H2.1' :style nil :toc nil))"
			 " (toc-item (:class nil :name 'H2.2' :style nil :toc nil))) "
			 "(toc-item (:class nil :name 'H3' :style nil :toc nil))))")))
		 (assert-equal expected-toc-str toc-str ))))


(define-test test-extract-toc-3 ()
	     (let ((tree-builder (make-instance 'cl-html-readme-dsl:tree-builder))
		   (doc '((heading (:name "XXX")
			   (heading (:name "H1" :toc t))
			   "XXXX"
			   (heading (:name "XX"))
			   "XXXX"
			   (heading (:name "H2" :toc t)
			    (heading (:name "XXX")
				     (heading (:name "H2.1" :toc t))
				     (heading (:name "XXX"))
				     "XXXX"
				     (heading (:name "H2.2" :toc t))))
			   "XXXX"
			   (heading (:name "H3" :toc t))))))
	       (cl-html-readme-dsl:extract-toc doc tree-builder)
	       (let ((toc (first (cl-html-readme-dsl:get-tree tree-builder))))
		 ;; p ::= <properties>
		 ;; toc ::= (toc-root p (toc-item p) (toc-container p (toc-item p) (toc-item p))
		 ;; (toc-item p))
		 (assert-true 3 (length toc))
		 (assert-true (cl-html-readme-dsl:toc-root-p (first toc)))
		 (let ((ti-1 (third toc)) (tc-2 (fourth toc)) (ti-3 (fifth toc)))
		   (assert-true (cl-html-readme-dsl:toc-item-p (first ti-1)))
		   (assert-equal "H1" (getf (second ti-1) :name))
		   (assert-true (cl-html-readme-dsl:toc-container-p (first tc-2)))
		   (assert-equal "H2" (getf (second tc-2) :name))
		   (let ((tc-2-ti-1 (third tc-2))
			 (tc-2-ti-2 (fourth tc-2)))
		     (assert-true (cl-html-readme-dsl:toc-item-p (first tc-2-ti-1)))
		     (assert-equal "H2.1" (getf (second tc-2-ti-1) :name))
		     (assert-true (cl-html-readme-dsl:toc-item-p (first tc-2-ti-2)))
		     (assert-equal "H2.2" (getf (second tc-2-ti-2) :name))
		     (assert-true (cl-html-readme-dsl:toc-item-p (first ti-3)))
		     (assert-equal "H3" (getf (second ti-3) :name)))))))


(define-test test-extract-toc-4 ()
	     (let ((tree-builder (make-instance 'cl-html-readme-dsl:tree-builder))
		   (doc '((heading (:name "H1"))
			  (heading (:name "H2"))
			  (heading (:name "H3")))))
	       (cl-html-readme-dsl:extract-toc doc tree-builder)
	       (let ((toc (first (cl-html-readme-dsl:get-tree tree-builder))))
		 (assert-false toc))))
