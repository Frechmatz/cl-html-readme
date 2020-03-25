(in-package :cl-readme)

(defparameter *home-directory* nil
  "Home directory of the current project.")

(defparameter *tab-width* 8
  "Width of a tab. Used, when tabs are to be replaced with space characters.")

(defparameter *get-heading-class* (lambda(level) (declare (ignore level)) nil)
  "Function that returns the value of the HTML class attribute of a heading element.
 The function is called with the indentation level of the heading element and must return nil
or a string.")

(defparameter *get-toc-container-class* (lambda(level) (declare (ignore level)) nil)
  "Function that returns the value of the HTML class attribute of a toc &lt;ul&gt; element.
 The function is called with the indentation level of the toc element and must return nil
or a string.")

(defparameter *get-toc-item-class* (lambda(level) (declare (ignore level)) nil)
  "Function that returns the value of the HTML class attribute of a toc &lt;li&gt; element.
 The function is called with the indentation level of the toc element and must return nil
or a string.")


