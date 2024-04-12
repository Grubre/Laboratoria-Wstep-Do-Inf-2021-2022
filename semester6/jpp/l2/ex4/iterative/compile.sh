gnatmake iterative
gnatbind -Lck -o ck.adb iterative.ali
gnatmake ck.adb
ar cr libck.a ck.o iterative.o
