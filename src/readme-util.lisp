(in-package :cl-readme)

(defparameter *home-directory* nil "Home directory of the current project.")
(defparameter *tab-width* 8 "Width of a tab. Used, when tabs are to be replaced with space characters.")

(defun validate-home-directory ()
  (if (not *home-directory*)
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
(defun sbcl-make-function-decl (f)
  "Returns a function declaration string using SBCL specific functionality."
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

(defun current-date ()
  "Returns a string representing the current date and time."
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
      (get-decoded-time)
    (declare (ignore dow dst-p tz))
    ;; 2018-09-19 21:28:16
    (let ((str (format nil "~4,'0d-~2,'0d-~2,'0d  ~2,'0d:~2,'0d:~2,'0d" yr mon day hr min sec)))
      str)))

(defun make-path (path)
  "Creates a path relative to *home-directory*. The function has the following arguments:
   <ul>
      <li>path The path, e.g. examples/example-1.lisp.</li>
   </ul>"
  (validate-home-directory)
  (concatenate 'string *home-directory* path))

(defun format-string (str &key replace-tabs escape)
  (let ((l 0))
    (let ((string-stream (make-string-output-stream)))
      (labels ((append-char (ch)
		 (write-char ch string-stream)
		 (setf l (+ 1 l)))
	       (append-string (str)
		 (write-string str string-stream)
		 (setf l (+ (length str) l)))
	       (append-tab ()
		 (if (not replace-tabs)
		     (append-char #\Tab)
		     (progn 
		       (append-char #\Space)
		       (let* ((tab-pos (floor (/ l *tab-width*))) ;; 0...n
			      (fill-in (- (* (+ tab-pos 1) *tab-width*) l)))
			 (dotimes (i fill-in)
			   (append-char #\Space)))))))
	(dotimes (i (length str))
	  (let ((ch (elt str i)))
	    (cond
	      ((eql ch #\Tab)
	       (append-tab))
	      ((and escape (eql ch #\<))
	       (append-string "&lt;"))
	      ((and escape (eql ch #\>))
	       (append-string "&gt;"))
	      ((and escape (eql ch #\&))
	       (append-string "&amp;"))
	      (t (append-char ch)))))
	(get-output-stream-string string-stream)))))

(defun read-file (path &key (replace-tabs nil) (escape nil))
  (let ((output (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s output)
      (with-open-file (fh (make-path path) :direction :input :external-format :utf-8)
	(loop 
	   (let ((str (read-line fh nil)))
	     (if (not str)
		 (return)
		 (format s "~a~%" (format-string str :replace-tabs replace-tabs :escape escape)))))))
    (string-trim '(#\Space #\Tab #\Newline) output)))

(defun read-verbatim (path &key (replace-tabs nil))
  "Reads a file and returns it as a string. Does not add any styling. This function
   is typically used to insert plain HTML files into the documentation.
   The function has the following arguments:
   <ul>
      <li>path Path of the file relative to *home-directory*.</li>
      <li>:replace-tabs If t then tabs are replaced with spaces according to the *tab-width* variable.</li>
   </ul>"
  (read-file path :replace-tabs replace-tabs :escape nil))

(defun read-code (path)
  "Reads a file that represents code and returns it as string. Tabs are replaced by 
   spaces according to the *tab-width* variable. Special characters are replaced with their HTML character entities.
   The code is embedded into HTML &lt;p&gt;&lt;pre&gt;&lt;code&gt; markup. The function has the following arguments:
   <ul>
      <li>path Path of the file relative to *home-directory*.</li>
   </ul>"
  (concatenate 'string
	       "<p><pre><code>"
	       (read-file path :replace-tabs t :escape t)
	       "</code></pre></p>"))
