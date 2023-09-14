(in-package :cl-html-readme-test)

;;
;; extract-toc-headings
;;

(define-test test-extract-toc-headings-1 ()
  (let ((doc '((heading (:name "H1" :toc t :extra "1"))
	       (heading (:name "H2" :toc t :extra "2"))
	       (heading (:name "H3" :toc t)))))
    (let ((toc (cl-html-readme-dsl::extract-toc-headings doc))
	  (expected-toc
	    '((heading (:name "H1" :extra "1" :toc t))
	      (heading (:name "H2" :extra "2" :toc t))
	      (heading (:name "H3" :toc t)))))
      (let ((toc-str (doc-to-string toc))
	    (expected-toc-str (doc-to-string expected-toc)))
	(assert-equal expected-toc-str toc-str)))))

(define-test test-extract-toc-headings-2 ()
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2"))
	       (heading (:name "H3" :toc t)))))
    (let ((toc (cl-html-readme-dsl::extract-toc-headings doc))
	  (expected-toc
	    '((heading (:name "H1" :toc t))
	      (heading (:name "H3" :toc t)))))
      (let ((toc-str (doc-to-string toc))
	    (expected-toc-str (doc-to-string expected-toc)))
	(assert-equal expected-toc-str toc-str)))))

(define-test test-extract-toc-headings-3 ()
  (let ((doc '((heading (:name "H1" :toc t)
		(heading (:name "H1.1" :toc t))
		(heading (:name "H1.2" :toc t)))
	       (heading (:name "H2" :toc t))
	       (heading (:name "H3" :toc t)
		(heading (:name "H3.1" :toc t))
		(heading (:name "H3.2" :toc t))))))
	       (let ((toc (cl-html-readme-dsl::extract-toc-headings doc))
		      (expected-toc
			'((heading (:name "H1" :toc t)
			   (heading (:name "H1.1" :toc t))
			   (heading (:name "H1.2" :toc t)))
			  (heading (:name "H2" :toc t))
			  (heading (:name "H3" :toc t)
			   (heading (:name "H3.1" :toc t))
			   (heading (:name "H3.2" :toc t))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

(define-test test-extract-toc-headings-4 ()
  (let ((doc '((heading (:name "H1" :toc t)
		(heading (:name "H1.1" :toc t))))))
	       (let ((toc (cl-html-readme-dsl::extract-toc-headings doc))
		      (expected-toc
			'((heading (:name "H1" :toc t)
			   (heading (:name "H1.1" :toc t))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

;;
;; extract-toc
;;

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
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc
			'((toc-root (:class nil :style nil)
			   (toc-item (:class nil :name "H1" :style nil :toc nil))
			   (toc-container (:class nil :container-class nil :container-style nil
					   :name "H2" :style nil :toc nil)
			    (toc-item (:class nil :name "H2.1" :style nil :toc nil))
			    (toc-item (:class nil :name "H2.2" :style nil :toc nil)))
			   (toc-item (:class nil :name "H3" :style nil :toc nil))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

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
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc
			'((toc-root (:class nil :style nil)
			   (toc-item (:class nil :name "H1" :style nil :toc nil))
			   (toc-container (:class nil :container-class nil :container-style nil
					   :name "H2" :style nil :toc nil)
			    (toc-item (:class nil :name "H2.1" :style nil :toc nil))
			    (toc-item (:class nil :name "H2.2" :style nil :toc nil)))
			   (toc-item (:class nil :name "H3" :style nil :toc nil))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

(define-test test-extract-toc-4 ()
	     (let ((tree-builder (make-instance 'cl-html-readme-dsl:tree-builder))
		   (doc '((heading (:name "H1"))
			  (heading (:name "H2"))
			  (heading (:name "H3")))))
	       (cl-html-readme-dsl:extract-toc doc tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc '()))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))
