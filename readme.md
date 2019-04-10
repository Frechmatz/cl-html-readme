cl-readme
=========

Readme generation utilities for my Common Lisp projects.

API
---

**cl-readme:\*HOME-DIRECTORY\***

Home directory of the current project.

* * *

**cl-readme:make-function-string** f &key (append-separator t)

Creates a string that consists of the name, the lambda list and the documentation string of a function.  
The function has the following arguments:

*   f A symbol denoting a function.
*   :append-separator If t then a separator is added after the documentation string.

* * *

**cl-readme:make-condition-string** c &key (append-separator t)

Creates a string that consists of the name and the documentation string of a condition.  
The function has the following arguments:

*   c A symbol denoting a condition.
*   :append-separator If t then a separator is added after the documentation string.

* * *

**cl-readme:read-text-file** path

Reads a file and returns it as a string. Does not add any styling. The file must not contain HTML markup. The function has the following arguments:

*   path Path of the file relative to \*HOME-DIRECTORY\*.

* * *

**cl-readme:example-code** path &key (example-number nil)

Reads a file that represents example code and returns it as a string. The file must not contain HTML markup. The function has the following arguments:

*   path Path of the file relative to \*HOME-DIRECTORY\*.
*   :example-number Optional number of the example. Used for creating the header of the code block.

* * *

**cl-readme:current-date**

Creates a string that consists of the current date and time.

* * *

**cl-readme:make-path** path

Creates a path relative to \*HOME-DIRECTORY\*. The function has the following arguments:

*   path The path, e.g. examples/example-1.lisp.

* * *

Generated 2019-04-10 22:55:25