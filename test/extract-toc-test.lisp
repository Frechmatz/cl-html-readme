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
	     (let ((tree-builder (cl-html-readme-dsl:make-tree-builder))
		   (doc '((heading (:name "H1" :toc t))
			  (heading (:name "H2" :toc t))
			  (heading (:name "H3" :toc t)))))
	       (cl-html-readme-dsl::write-toc doc nil tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc
			'((toc-root ()
			   (toc-item (:name "H1"))
			   (toc-item (:name "H2"))
			   (toc-item (:name "H3"))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

(define-test test-write-toc-2 ()
	     (let ((tree-builder (cl-html-readme-dsl:make-tree-builder))
		   (doc '((heading (:name "H1" :toc t))
			  (heading (:name "H2" :toc t)
			   (heading (:name "H2.1" :toc t))
			   (heading (:name "H2.2" :toc t)))
			  (heading (:name "H3" :toc t)))))
	       (cl-html-readme-dsl::write-toc doc nil tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc
			'((toc-root ()
			   (toc-item (:name "H1"))
			   (toc-container (:name "H2")
			    (toc-item (:name "H2.1"))
			    (toc-item (:name "H2.2")))
			   (toc-item (:name "H3"))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

(define-test test-write-toc-3 ()
	     (let ((tree-builder (cl-html-readme-dsl:make-tree-builder))
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
	       (cl-html-readme-dsl::write-toc doc nil tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc
			'((toc-root ()
			   (toc-item (:name "H1"))
			   (toc-container (:name "H2")
			    (toc-item (:name "H2.1"))
			    (toc-item (:name "H2.2")))
			   (toc-item (:name "H3"))))))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

(define-test test-write-toc-4 ()
	     (let ((tree-builder (cl-html-readme-dsl:make-tree-builder))
		   (doc '((heading (:name "H1"))
			  (heading (:name "H2"))
			  (heading (:name "H3")))))
	       (cl-html-readme-dsl::write-toc doc nil tree-builder)
	       (let* ((toc (cl-html-readme-dsl:get-tree tree-builder))
		      (expected-toc '()))
		 (let ((toc-str (doc-to-string toc))
		       (expected-toc-str (doc-to-string expected-toc)))
		 (assert-equal expected-toc-str toc-str)))))

;;
;; expand-toc
;;

(define-test test-expand-toc-1 ()
  (let ((doc '((heading (:name "H1" :toc t :a 1 :b 2))
	       (heading (:name "H2" :toc t :c 11 :d 12))
	       (toc)
	       (heading (:name "H3" :toc t)))))
    (let ((updated-doc (cl-html-readme-dsl:expand-toc doc))
	  (expected-doc
	    '((heading (:name "H1" :toc t :a 1 :b 2))
	      (heading (:name "H2" :toc t :c 11 :d 12))
	      (toc-root ()
	       (toc-item (:name "H1" :a 1 :b 2))
	       (toc-item (:name "H2" :c 11 :d 12))
	       (toc-item (:name "H3")))
	      (heading (:name "H3" :toc t)))))
      (let ((doc-str (doc-to-string updated-doc))
	    (expected-doc-str (doc-to-string expected-doc)))
	(assert-equal expected-doc-str doc-str)))))

