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
  "Helper function to invoke string formatting with a very explicit interface"
  (cl-html-readme::format-string-impl str replace-tabs escape input-tab-width output-tab-width))

(defun test-format-string-make-input-string (items)
  "Build a string containing tabs and spaces where such characters are represented by keywords"
  (let ((string-stream (make-string-output-stream)))
    (dolist (item items)
      (cond
	((stringp item)
	 (write-string item string-stream))
	((eq :tab item)
	 (write-char #\Tab string-stream))
	((eq :space item)
	 (write-char #\Space string-stream))
	(t
	 (error (format nil "Dont know how to format ~a" item)))))
    (get-output-stream-string string-stream)))

(defun test-format-string-format-output-string (str)
  "Parse a string and replace tabs and spaces with comparison friendly string literals"
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

(define-test test-format-string-plain ()
  "Plain string in and out"
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

(define-test test-format-string-pass-through-tab-1 ()
  "Pass through a tab"
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

(define-test test-format-string-pass-through-tab-2 ()
  "Pass through multiple tabs"
  (let* ((input-str (test-format-string-make-input-string (list :tab :tab "ABC")))
	 (output-str
	   (test-format-string-format-output-string
	    (test-format-string-invoke-format-string
	     input-str
	     :replace-tabs nil
	     :escape nil
	     :input-tab-width 4
	     :output-tab-width 4))))
    (assert-equal "<tab><tab>ABC" output-str)))

(define-test test-format-string-pass-through-tab-3 ()
  "Pass through a sequence of tabs and spaces"
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

(define-test test-format-string-pass-through-tab-4 ()
  "Pass through a sequence of tabs and spaces"
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

(define-test test-format-string-replace-tabs-1 ()
  "Replace a tab with spaces"
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

(define-test test-format-string-replace-tabs-2 ()
  "Replace a sequence of tabs and spaces with a sequence of spaces"
  (let* ((input-str
	   (test-format-string-make-input-string
	    (list
	     :tab
	     :space
	     :space
	     :tab
	     "ABC")))
	 (output-str
	   (test-format-string-format-output-string
	    (test-format-string-invoke-format-string
	     input-str
	     :replace-tabs t
	     :escape nil
	     :input-tab-width 4
	     :output-tab-width 4))))
    (assert-equal "<spc><spc><spc><spc><spc><spc><spc><spc>ABC" output-str)))

(define-test test-format-string-replace-tabs-3 ()
  "Replace a sequence of tabs and spaces with a sequence of spaces"
  (let* ((input-str
	   (test-format-string-make-input-string
	    (list
	     :space
	     :space
	     :tab
	     "ABC")))
	 (output-str
	   (test-format-string-format-output-string
	    (test-format-string-invoke-format-string
	     input-str
	     :replace-tabs t
	     :escape nil
	     :input-tab-width 4
	     :output-tab-width 4))))
    (assert-equal "<spc><spc><spc><spc>ABC" output-str)))
