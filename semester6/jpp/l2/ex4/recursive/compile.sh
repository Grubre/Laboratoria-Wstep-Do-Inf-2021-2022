gnatmake recursive
gnatbind -Lck -o ck.adb recursive.ali
gnatmake ck.adb
ar cr libck.a ck.o recursive.o
