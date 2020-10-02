(in-package :cl-readme)

(defun make-path (path)
  "Creates a path relative to *home-directory*. The function has the following arguments:
   <ul>
      <li>path The path, e.g. examples/example-1.lisp.</li>
   </ul>"
  (if (not *home-directory*)
      (error "Variable *HOME-DIRECTORY* not set"))
  (if (pathnamep *home-directory*)
      (merge-pathnames path *home-directory*)
      (concatenate 'string *home-directory* path)))

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
  "Reads a text file and returns it as a string. The function has the following arguments:
   <ul>
      <li>path Path of the file relative to *home-directory*.</li>
      <li>:replace-tabs If t then tabs are replaced with spaces according to the *tab-width* variable.</li>
      <li>:escape If t then special characters are replaced with HTML character entities.</li>
   </ul>"
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

