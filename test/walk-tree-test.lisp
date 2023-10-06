(in-package :cl-html-readme-test)

(defun record-tree-walk (doc)
  "Records handler invocations of the tree-walker and returns the recording."
  (let ((recording nil))
    (cl-html-readme-dsl::walk-tree
     doc
     :open-element
     (lambda(element properties content)
       (push
	(list
	 :action :open-element
	 :form (string-downcase (symbol-name element))
	 :form-properties properties
	 :content content)
	recording)
       (getf properties :cl-html-unit-test-open-element-return-context))
     :close-element
     (lambda(context)
       (push
	(list
	 :action :close-element
	 :context context)
	recording))
     :text
     (lambda(str)
       (push
	(list
	 :action :text
	 :text str)
	recording)))
    (reverse recording)))

(defun assert-recording (recording expected-recording)
  "Compare two recordings"
  (assert-equal (length expected-recording) (length recording))
  (dotimes (i (length recording))
    (let ((recorded-entry (nth i recording))
	  (expected-entry (nth i expected-recording)))
      (let ((action (getf expected-entry :action)))
	(assert-equal action (getf recorded-entry :action))
	(cond
	  ((eq action :text)
	   (assert-equal (getf expected-entry :text) (getf recorded-entry :text)))
	  ((eq action :open-element)
	   (assert-equal (getf expected-entry :form) (getf recorded-entry :form))
	   (let ((recorded-properties (getf recorded-entry :form-properties))
		 (expected-properties (getf expected-entry :form-properties)))
	     (assert-equal (length expected-properties) (length recorded-properties))
	     (let ((keys (get-property-list-keys expected-properties)))
	       (dolist (key keys)
		 (assert-equal
		  (getf expected-properties key)
		  (getf recorded-properties key)))))
	   (let ((compare-content-fn (getf expected-entry :compare-content)))
	     (let ((test (funcall compare-content-fn (getf recorded-entry :content))))
	       (assert-true test))))
	  ((eq action :close-element)
	   (assert-equal (getf expected-entry :context) (getf recorded-entry :context)))
	  (t
	   (error
	    'simple-error
	    :format-control "Unsupported action: ~a"
	    :format-arguments (list action))))))))

(define-test walk-tree-test-1 ()
  (let ((doc '("TEXT-1" "TEXT-2" "TEXT-3")))
    (let ((recording (record-tree-walk doc)))
      (assert-recording
       recording
       (list
	(list :action :text :text "TEXT-1")
	(list :action :text :text "TEXT-2")
	(list :action :text :text "TEXT-3"))))))

(define-test walk-tree-test-2 ()
  (let ((doc '("TEXT-1" (heading (:name "H1" :id 1)) "TEXT-2" "TEXT-3")))
    (let ((recording (record-tree-walk doc)))
      (assert-recording
       recording
       (list
	(list :action :text :text "TEXT-1")
	(list :action :open-element
	      :form "heading"
	      :form-properties (list :id 1 :name "H1")
	      :compare-content (lambda(content) (declare (ignore content)) t))
	(list :action :close-element :context nil)
	(list :action :text :text "TEXT-2")
	(list :action :text :text "TEXT-3"))))))

(define-test walk-tree-test-3 ()
  (let ((doc '((heading (:name "H1") (heading (:name "H1.1"))))))
    (let ((recording (record-tree-walk doc)))
      (assert-recording
       recording
       (list
	(list :action :open-element
	      :form "heading"
	      :form-properties (list :name "H1")
	      :compare-content (lambda(content) (declare (ignore content)) t))
	(list :action :open-element
	      :form "heading"
	      :form-properties (list :name "H1.1")
	      :compare-content (lambda(content) (declare (ignore content)) t))
	(list :action :close-element :context nil)
	(list :action :close-element :context nil))))))

(define-test walk-tree-test-4 ()
  (let ((doc
	  '((heading
	     (:name "H1"
	      :cl-html-unit-test-open-element-return-context "H1-CLOSE")))))
    (let ((recording (record-tree-walk doc)))
      (assert-recording
       recording
       (list
	(list :action :open-element
	      :form "heading"
	      :form-properties
	      (list
	       :name "H1"
	       :cl-html-unit-test-open-element-return-context "H1-CLOSE")
	      :compare-content (lambda(content) (declare (ignore content)) t))
	(list :action :close-element :context "H1-CLOSE"))))))

(define-test walk-tree-test-content-1 ()
  (let ((doc '("TEXT-1" (heading (:name "H1") "TEXT-2" "TEXT-3"))))
    (let ((recording (record-tree-walk doc)))
      (assert-recording
       recording
       (list
	(list :action :text :text "TEXT-1")
	(list :action :open-element
	      :form "heading"
	      :form-properties (list :name "H1")
	      :compare-content (lambda(content)
				 ;;(format t "~%Content:~a~%" content)
				 (and
				  (= 2 (length content))
				  (string= "TEXT-2" (first content))
				  (string= "TEXT-3" (second content)))))
	(list :action :text :text "TEXT-2")
	(list :action :text :text "TEXT-3")
	(list :action :close-element :context nil))))))

(define-test walk-tree-test-content-2 ()
  (let ((doc '("TEXT-1" (heading (:name "H1") "TEXT-2") "TEXT-3")))
    (let ((recording (record-tree-walk doc)))
      (assert-recording
       recording
       (list
	(list :action :text :text "TEXT-1")
	(list :action :open-element
	      :form "heading"
	      :form-properties (list :name "H1")
	      :compare-content (lambda(content)
				 ;;(format t "~%Content:~a~%" content)
				 (and
				  (= 1 (length content))
				  (string= "TEXT-2" (first content)))))
	(list :action :text :text "TEXT-2")
	(list :action :close-element :context nil)
	(list :action :text :text "TEXT-3"))))))

(define-test walk-tree-test-content-3 ()
  (let ((doc '("TEXT-1" (heading (:name "H1")) "TEXT-2")))
    (let ((recording (record-tree-walk doc)))
      (assert-recording
       recording
       (list
	(list :action :text :text "TEXT-1")
	(list :action :open-element
	      :form "heading"
	      :form-properties (list :name "H1")
	      :compare-content (lambda(content)
				 ;;(format t "~%Content:~a~%" content)
				 (not content)))
	(list :action :close-element :context nil)
	(list :action :text :text "TEXT-2"))))))

