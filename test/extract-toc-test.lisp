(in-package :cl-html-readme-test)

;;
;; get-toc-headings
;;

(define-test test-get-toc-headings-1 ()
  (let ((doc '((heading (:name "H1" :toc t :extra "1"))
	       (heading (:name "H2" :toc t :extra "2"))
	       (heading (:name "H3" :toc t)))))
    (let ((toc (cl-html-readme-dsl::get-toc-headings doc))
	  (expected-toc
	    '((heading (:name "H1" :extra "1" :toc t))
	      (heading (:name "H2" :extra "2" :toc t))
	      (heading (:name "H3" :toc t)))))
      (let ((toc-str (doc-to-string toc))
	    (expected-toc-str (doc-to-string expected-toc)))
	(assert-equal expected-toc-str toc-str)))))

(define-test test-get-toc-headings-2 ()
  (let ((doc '((heading (:name "H1" :toc t))
	       (heading (:name "H2"))
	       (heading (:name "H3" :toc t)))))
    (let ((toc (cl-html-readme-dsl::get-toc-headings doc))
	  (expected-toc
	    '((heading (:name "H1" :toc t))
	      (heading (:name "H3" :toc t)))))
      (let ((toc-str (doc-to-string toc))
	    (expected-toc-str (doc-to-string expected-toc)))
	(assert-equal expected-toc-str toc-str)))))

(define-test test-get-toc-headings-3 ()
  (let ((doc '((heading (:name "H1" :toc t)
		(heading (:name "H1.1" :toc t))
		(heading (:name "H1.2" :toc t)))
	       (heading (:name "H2" :toc t))
	       (heading (:name "H3" :toc t)
		(heading (:name "H3.1" :toc t))
		(heading (:name "H3.2" :toc t))))))
	       (let ((toc (cl-html-readme-dsl::get-toc-headings doc))
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

(define-test test-get-toc-headings-4 ()
  (let ((doc '((heading (:name "H1" :toc t)
		(heading (:name "H1.1" :toc t))))))
	       (let ((toc (cl-html-readme-dsl::get-toc-headings doc))
		      (expected-toc
			'((heading (:name "H1" :toc t)
			   (heading (:name "H1.1" :toc t))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

;;
;; write-toc
;;

(define-test test-write-toc-1 ()
	     (let ((tree-builder (make-instance 'cl-html-readme-dsl:tree-builder))
		   (doc '((heading (:name "H1" :toc t))
			  (heading (:name "H2" :toc t))
			  (heading (:name "H3" :toc t)))))
	       (cl-html-readme-dsl:write-toc doc nil tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc
			'((toc-root ()
			   (toc-item (:name "H1" :toc nil))
			   (toc-item (:name "H2" :toc nil))
			   (toc-item (:name "H3" :toc nil))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

(define-test test-write-toc-2 ()
	     (let ((tree-builder (make-instance 'cl-html-readme-dsl:tree-builder))
		   (doc '((heading (:name "H1" :toc t))
			  (heading (:name "H2" :toc t)
			   (heading (:name "H2.1" :toc t))
			   (heading (:name "H2.2" :toc t)))
			  (heading (:name "H3" :toc t)))))
	       (cl-html-readme-dsl:write-toc doc nil tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc
			'((toc-root ()
			   (toc-item (:name "H1" :toc nil))
			   (toc-container (:name "H2" :toc nil)
			    (toc-item (:name "H2.1" :toc nil))
			    (toc-item (:name "H2.2" :toc nil)))
			   (toc-item (:name "H3" :toc nil))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

(define-test test-write-toc-3 ()
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
	       (cl-html-readme-dsl:write-toc doc nil tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc
			'((toc-root ()
			   (toc-item (:name "H1" :toc nil))
			   (toc-container (:name "H2" :toc nil)
			    (toc-item (:name "H2.1" :toc nil))
			    (toc-item (:name "H2.2" :toc nil)))
			   (toc-item (:name "H3" :toc nil))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

(define-test test-write-toc-4 ()
	     (let ((tree-builder (make-instance 'cl-html-readme-dsl:tree-builder))
		   (doc '((heading (:name "H1"))
			  (heading (:name "H2"))
			  (heading (:name "H3")))))
	       (cl-html-readme-dsl:write-toc doc nil tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc '()))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

