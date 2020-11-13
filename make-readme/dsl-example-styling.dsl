((semantic (:name "header" :class "semantic-class" :style "semantic-style" )
	   (heading (:name "Example" :class "heading-class" :style "heading-style")))
 (semantic (:name "nav")
	   (heading (:name "Table of contents")
		    (toc (:root-class "toc-root-class"
			  :root-style "toc-root-style"
			  :container-class "toc-container-class"
			  :container-style "toc-container-style"
			  :item-class "toc-item-class"
			  :item-style "toc-item-style"))))
 (semantic (:name "section")
	   (heading (:name "Chapter 1" :toc t)
		    "Lorem ipsum"
		    (heading (:name "Chapter 1.1" :toc t)
			     "Lorem ipsum"))
	   (heading (:name "Chapter 2" :toc t))
	   "Lorem ipsum")
 (semantic (:name "footer") "Lorem ipsum"))
