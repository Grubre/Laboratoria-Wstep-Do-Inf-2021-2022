#!/bin/bash

gcc test.c ./iterative/target/debug/libiterative.a -o iterative_test
gcc test.c ./recursive/target/debug/librecursive.a -o recursive_test
