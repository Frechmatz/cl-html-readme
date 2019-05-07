cl-readme
=========

Readme generation utilities for my Common Lisp projects.  

A Readme file in markdown format is created in two steps:

1.  Generate an HTML file using the cl-readme package. Lisp documentation strings are supposed to use HTML as markup language. This package uses SBCL extensions for the generation of lambda list strings.
2.  Convert the HTML file to markdown using the JavaScript "turndown" library.

Installation
------------

    
    cd ~/quicklisp/local-projects
    git clone https://github.com/Frechmatz/cl-readme.git
    # Install Node.js script cl-readme-html2markdown globally 
    cd ~/quicklisp/local-projects/cl-readme/node
    npm install --global
    # To uninstall use: npm rm --global cl-readme-html2markdown
    

Example
-------

### Generate the HTML file

    (in-package :cl-readme-make-readme)
    
    (defun write-html ()
      (let ((cl-readme:*home-directory* "/Users/olli/src/lisp/cl-readme/")
            (cl-readme:*tab-width* 8))
        (let ((docstr (concatenate
                       'string
                       "<html><body>"
                       "<h1>cl-readme</h1>"
                       (read-text-file "make-readme/introduction.html")
                       "<h2>Installation</h2>"
                       (read-text-file "make-readme/installation.html")
                       "<h2>Example</h2>"
                       "<h3>Generate the HTML file</h3>"
                       (example-code "make-readme/make-readme.lisp" :omit-header t)
                       "<h3>Convert the HTML file to Markdown</h3>"
                       (read-text-file "make-readme/html2md.html")
                       "<h2>API</h2>"
                       (make-variable-string 'cl-readme:*home-directory* :append-separator t)
                       (make-variable-string 'cl-readme:*tab-width* :append-separator t)
                       (make-function-string 'cl-readme:make-function-string :append-separator t)
                       (make-function-string 'cl-readme:make-condition-string :append-separator t)
                       (make-function-string 'cl-readme:make-variable-string :append-separator t)
                       (make-function-string 'cl-readme:read-text-file :append-separator t)
                       (make-function-string 'cl-readme:example-code :append-separator t)
                       (make-function-string 'cl-readme:current-date :append-separator t)
                       (make-function-string 'cl-readme:make-path :append-separator nil)
                       "<hr/><p><small>Generated " (current-date) "</small></p>"
                       "</body></html>")))
          (with-open-file (fh (make-path "make-readme/generated/readme.html")
                              :direction :output
                              :if-exists :supersede
                              :if-does-not-exist :create
                              :external-format :utf-8)
            (format fh "~a" docstr)))))
    
    ;;(write-html)

### Convert the HTML file to Markdown

    
    $ cl-readme-html2markdown /Users/olli/src/lisp/cl-readme/make-readme/generated/readme.html /Users/olli/src/lisp/cl-readme/make-readme/generated/readme.md
    

API
---

**cl-readme:\*home-directory\***

Home directory of the current project.

* * *

**cl-readme:\*tab-width\***

Width of a tab. Used, when tabs are to be replaced with space characters.

* * *

**cl-readme:make-function-string** f &key (append-separator t)

Returns the HTML representation of a function. This function depends on SBCL extensions. The function has the following arguments:

*   f A symbol denoting a function.
*   :append-separator If t then a separator is added after the documentation string.

* * *

**cl-readme:make-condition-string** c &key (append-separator t)

Returns the HTML representation of a condition. The function has the following arguments:

*   c A symbol denoting a condition.
*   :append-separator If t then a separator is added after the documentation string.

* * *

**cl-readme:make-variable-string** v &key (append-separator t)

Returns the HTML representation of a variable (defvar, defparameter). The function has the following arguments:

*   v A symbol denoting a variable.
*   :append-separator If t then a separator is added after the documentation string.

* * *

**cl-readme:read-text-file** path &key (replace-tabs nil) (escape nil)

Reads a file and returns it as a string. Does not add any styling. This function is typically used to insert plain HTML files into the documentation. The function has the following arguments:

*   path Path of the file relative to \*home-directory\*.
*   :replace-tabs If t then tabs are replaced with spaces according to the \*tab-width\* variable.
*   :escape If t then characters such as '<', '>', '&' are replaced with their entities.

* * *

**cl-readme:example-code** path &key (example-number nil) (omit-header nil)

Returns the HTML representation of example code denoted by a path. Tabs are replaced by spaces according to the \*tab-width\* variable. The function has the following arguments:

*   path Path of the file relative to \*home-directory\*.
*   :example-number Optional number of the example. Used for rendering the header of the code block.
*   :omit-header If t then do not render the "Example" heading

* * *

**cl-readme:current-date**

Returns a string representing the current date and time.

* * *

**cl-readme:make-path** path

Creates a path relative to \*home-directory\*. The function has the following arguments:

*   path The path, e.g. examples/example-1.lisp.

* * *

Generated 2019-05-07 20:29:03