(in-package :cl-readme)

(defparameter *HOME-DIRECTORY* nil "Home directory of the current project.")

(defun validate-home-directory ()
  (if (not *HOME-DIRECTORY*)
      (error "Variable *HOME-DIRECTORY* not set")))

(defun lambda-list-arg-to-string (arg)
  (cond
    ((keywordp arg)
     (format nil ":~a" arg))
    ((symbolp arg)
     (symbol-name arg))
    ((stringp arg)
     (format nil "\"~a\"" arg))
    ((listp arg)
     (concatenate 'string
		  "("
		  (reduce
		   (lambda(buffer item)
		     (concatenate 'string buffer
				  (if (> (length buffer) 0) " " "")
				  (lambda-list-arg-to-string item)))
		   arg
		   :initial-value "")
		  ")"))
    (t (format nil "~a" arg))))

;; uses sbcl extensions
(defun make-function-declaration-string (f)
  (let ((f-name (symbol-name f))
	(f-lambda-list-str
	 (mapcar
	  (lambda (item) (lambda-list-arg-to-string item))
	  (sb-introspect:function-lambda-list f))))
    (let ((ll (reduce
	       (lambda(buffer item) (concatenate 'string buffer item " "))
	       f-lambda-list-str
	       :initial-value "")))
      (concatenate 'string
		   "<b>"
		   (string-downcase (package-name (symbol-package f)))
		   ":"
		   (string-downcase f-name)
		   "</b>"
		   " "
		   (string-downcase ll)))))

(defun make-function-string (f &key (append-separator t))
  "Creates a string that consists of the name, the lambda list and the documentation string of a function.<br>
   The function has the following arguments:
   <ul>
      <li>f A symbol denoting a function.</li>
      <li>:append-separator If t then a separator is added after the documentation string.</li>
   </ul>"
  (concatenate
   'string
   "<p>"
   (make-function-declaration-string f)
   "</p><p>"
   (documentation f 'function)
   "</p>"
   (if append-separator "<hr/>" "")))

(defun make-condition-string (c &key (append-separator t))
  "Creates a string that consists of the name and the documentation string of a condition.<br>
   The function has the following arguments:
   <ul>
      <li>c A symbol denoting a condition.</li>
      <li>:append-separator If t then a separator is added after the documentation string.</li>
   </ul>"
  (concatenate
   'string
   "<b>" (string-downcase (symbol-name c)) "</b>"
   "<p>"
   (documentation c 'type)
   "</p>"
   (if append-separator "<hr/>" "")))

(defun make-variable-string (v &key (append-separator t))
  "Creates a string that consists of the name and the documentation string of a variable.<br>
   The function has the following arguments:
   <ul>
      <li>v A symbol denoting a variable.</li>
      <li>:append-separator If t then a separator is added after the documentation string.</li>
   </ul>"
  (concatenate
   'string
   "<b>" (string-downcase (package-name (symbol-package v))) ":" (symbol-name v) "</b>"
   "<p>"
   (documentation v 'variable)
   "</p>"
   (if append-separator "<hr/>" "")))

(defun current-date ()
  "Creates a string that consists of the current date and time."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    ;; 2018-09-19 21:28:16
    (let ((str (format nil "~4,'0d-~2,'0d-~2,'0d  ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec)))
      str)))

(defun make-path (path)
  "Creates a path relative to *HOME-DIRECTORY*. The function has the following arguments:
   <ul>
      <li>path The path, e.g. examples/example-1.lisp.</li>
   </ul>"
  (validate-home-directory)
  (concatenate 'string *HOME-DIRECTORY* path))

(defun read-text-file (path)
  "Reads a file and returns it as a string. Does not add any styling. 
   The file must not contain HTML markup. The function has the following arguments:
   <ul>
      <li>path Path of the file relative to *HOME-DIRECTORY*.</li>
   </ul>"
  (let ((output (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s output)
      (with-open-file (fh (make-path path) :direction :input :external-format :utf-8)
	(loop 
	   (let ((str (read-line fh nil)))
	     (if (not str)
		 (return)
		 (format s "~a~%" str))))))
    (string-trim '(#\Space #\Tab #\Newline) output)))

(defun example-code (path &key (example-number nil))
  "Reads a file that represents example code and returns it as a string. 
   The file must not contain HTML markup. The function has the following arguments:
   <ul>
      <li>path Path of the file relative to *HOME-DIRECTORY*.</li>
      <li>:example-number Optional number of the example. Used for creating the header of the code block.
   </ul>"
  (concatenate 'string
	       "<p><b>Example"
	       (if example-number (format nil " ~a" example-number) "")
	       ":</b></p>"
	       "<p><pre><code>"
	       (read-text-file path)
	       "</code></pre></p>"))

