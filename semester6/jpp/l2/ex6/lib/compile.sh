gcc lib_recursive.c -c
gcc lib_iterative.c -c
ar rcs liblib_iterative.a lib_iterative.o
ar rcs liblib_recursive.a lib_recursive.o
