#!/bin/bash

./compile.sh compressor
./compile.sh decompressor
./compressor $1 compressed.txt
./ratio.sh $1 compressed.txt
./decompressor compressed.txt decompressed.txt
diff $1 decompressed.txt
