# Generate documentation with abcl
echo -------------------------------------------------------------
echo Not working due to stack overflow while running the lisp code
echo Tried with
echo - Armed Bear Common Lisp 1.9.2
echo - Java 21.0.1 Oracle Corporation
echo -------------------------------------------------------------
echo Press any key to continue
read
abcl --batch --load generate-doc.lisp
