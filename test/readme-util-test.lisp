(in-package :cl-html-readme-test)

;;
;; Helper functions
;;

(defun test-format-string-invoke-format-string
    (str
     &key
       replace-tabs
       escape
       input-tab-width
       output-tab-width)
  "Invoke string formatting with keyword params"
  (cl-html-readme::format-string-impl str replace-tabs escape input-tab-width output-tab-width))

(defun test-format-string-make-input-string (items)
  "Build a string containing tabs"
  (let ((string-stream (make-string-output-stream)))
    (dolist (item items)
      (cond
	((stringp item)
	 (write-string item string-stream))
	((eq :tab item)
	 (write-char #\Tab string-stream))
	(t
	 (error (format nil "Dont know how to format ~a" item)))))
    (get-output-stream-string string-stream)))

(defun test-format-string-format-output-string (str)
  "Replace tabs and chars with with comparison-friendly literals"
  (let ((string-stream (make-string-output-stream)))
    (dotimes (i (length str))
      (let ((ch (elt str i)))
	(cond
	  ((eql ch #\Tab)
	   (write-string "<tab>" string-stream))
	  ((eql ch #\Space)
	   (write-string "<spc>" string-stream))
	  (t (write-char ch string-stream)))))
    (get-output-stream-string string-stream)))

;;
;; Tests
;;

(define-test test-format-string-1 ()
  (let* ((input-str (test-format-string-make-input-string (list "ABC")))
	 (output-str
	   (test-format-string-format-output-string
	    (test-format-string-invoke-format-string
	     input-str
	     :replace-tabs nil
	     :escape nil
	     :input-tab-width 4
	     :output-tab-width 4))))
    (assert-equal "ABC" output-str)))

;;
;; A bunch of basic tests
;;

(define-test test-format-string-2 ()
  (let* ((input-str (test-format-string-make-input-string (list :tab "ABC")))
	 (output-str
	   (test-format-string-format-output-string
	    (test-format-string-invoke-format-string
	     input-str
	     :replace-tabs nil
	     :escape nil
	     :input-tab-width 4
	     :output-tab-width 4))))
    (assert-equal "<tab>ABC" output-str)))

(define-test test-format-string-3 ()
  (let* ((input-str (test-format-string-make-input-string (list :tab " " "ABC")))
	 (output-str
	   (test-format-string-format-output-string
	    (test-format-string-invoke-format-string
	     input-str
	     :replace-tabs nil
	     :escape nil
	     :input-tab-width 4
	     :output-tab-width 4))))
    (assert-equal "<tab><spc>ABC" output-str)))

(define-test test-format-string-4 ()
  (let* ((input-str (test-format-string-make-input-string (list " " :tab "ABC")))
	 (output-str
	   (test-format-string-format-output-string
	    (test-format-string-invoke-format-string
	     input-str
	     :replace-tabs nil
	     :escape nil
	     :input-tab-width 4
	     :output-tab-width 4))))
    (assert-equal "<spc><tab>ABC" output-str)))

;;
;; Tab replacement tests
;;

(define-test test-format-string-replace-tabs-1 ()
  (let* ((input-str (test-format-string-make-input-string (list :tab "ABC")))
	 (output-str
	   (test-format-string-format-output-string
	    (test-format-string-invoke-format-string
	     input-str
	     :replace-tabs t
	     :escape nil
	     :input-tab-width 4
	     :output-tab-width 4))))
    (assert-equal "<spc><spc><spc><spc>ABC" output-str)))


