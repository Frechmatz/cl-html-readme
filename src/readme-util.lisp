(in-package :cl-html-readme)

(defparameter *home-directory* nil
  "Home directory of the project for which documentation is to be generated.")

(defparameter *tab-width* 8
  "The width of a tab.")

(defun make-path (path)
  "Creates an absolute path. The function has the following parameters:
   <ul>
      <li>path A path relative to *home-directory*, e.g. <code>\"examples/example-1.lisp\"</code>. A path can be represented as String or as Pathname.</li>
   </ul>"
  (if (not *home-directory*)
      (error "Variable *HOME-DIRECTORY* not set"))
  (if (pathnamep *home-directory*)
      (merge-pathnames path *home-directory*)
      (concatenate 'string *home-directory* path)))

(defun format-string-impl (str replace-tabs escape input-tab-width)
  (let ((l 0))
    (let ((string-stream (make-string-output-stream)))
      (labels ((append-char (ch)
		 (write-char ch string-stream)
		 (setf l (+ 1 l)))
	       (append-character-entity (str)
		 (write-string str string-stream)
		 (setf l (+ 1 l)))
	       (append-tab ()
		 (if (not replace-tabs)
		     (append-char #\Tab)
		     (progn 
		       (append-char #\Space)
		       (let* ((tab-pos (floor (/ l input-tab-width))) ;; 0...n
			      (fill-in (- (* (+ tab-pos 1) input-tab-width) l)))
			 (dotimes (i fill-in)
			   (append-char #\Space)))))))
	(dotimes (i (length str))
	  (let ((ch (elt str i)))
	    (cond
	      ((eql ch #\Tab)
	       (append-tab))
	      ((and escape (eql ch #\<))
	       (append-character-entity "&lt;"))
	      ((and escape (eql ch #\>))
	       (append-character-entity "&gt;"))
	      ((and escape (eql ch #\&))
	       (append-character-entity "&amp;"))
	      ((and escape (eql ch #\"))
	       (append-character-entity "&quot;"))
	      (t (append-char ch)))))
	(get-output-stream-string string-stream)))))

(defun format-string (str &key replace-tabs escape)
  (format-string-impl str replace-tabs escape *tab-width*))

(defun read-stream (stream &key (replace-tabs nil) (escape nil))
  "Reads a text stream and returns it as a string. The function has the following parameters:
   <ul>
      <li>stream An input stream.</li>
      <li>:replace-tabs If t then tabs are replaced with spaces according to the *tab-width* variable.</li>
      <li>:escape If t then special characters are replaced with HTML character entities.</li>
   </ul>"
  (let ((output (make-array '(0) :element-type 'base-char
			    :fill-pointer 0 :adjustable t)))
    (with-output-to-string (s output)
      (loop 
	(let ((str (read-line stream nil)))
	  (if (not str)
	      (return)
	      (format s "~a~%" (format-string str :replace-tabs replace-tabs :escape escape))))))
  (string-trim '(#\Space #\Tab #\Newline) output)))

(defun read-file (path &key (replace-tabs nil) (escape nil))
  "Reads a text file and returns it as a string. The function has the following parameters:
   <ul>
      <li>path Path of the file relative to *home-directory*.</li>
      <li>:replace-tabs If t then tabs are replaced with spaces according to the *tab-width* variable.</li>
      <li>:escape If t then special characters are replaced with HTML character entities.</li>
   </ul>"
  (with-open-file (fh (make-path path) :direction :input :external-format :utf-8)
    (read-stream fh :replace-tabs replace-tabs :escape escape)))

(defun read-string (string &key (replace-tabs nil) (escape nil))
  "Parses a multiline string and returns it as a string. The function has the following parameters:
   <ul>
      <li>string A string.</li>
      <li>:replace-tabs If t then tabs are replaced with spaces according to the *tab-width* variable.</li>
      <li>:escape If t then special characters are replaced with HTML character entities.</li>
   </ul>"
  (let ((stream (make-string-input-stream string)))
    (read-stream stream :replace-tabs replace-tabs :escape escape)))

