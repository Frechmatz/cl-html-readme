((semantic (:name "header" :class "semantic-header-class" :style "semantic-header-style" )
	   (heading (:name "Example" :class "example-class" :style "example-style")))
 (semantic (:name "nav")
	   (heading (:name "Table of contents")
		    (toc)))
 (semantic (:name "section")
	   (heading (:name "Chapter 1" :toc t :class "chapter-1-class" :style "chapter-1-style" :toc-class "chapter-1-toc-class" :toc-style "chapter-1-toc-style"))
	   "Lorem ipsum"
	   (heading (:name "Chapter 2" :toc t))
	   "Lorem ipsum")
 (semantic (:name "footer") "Lorem ipsum"))))
