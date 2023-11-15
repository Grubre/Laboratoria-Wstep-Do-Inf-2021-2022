#!/bin/bash

if [ "$#" -eq 0 ]; then
    echo "Usage: $0 <input_file>" >&2
    exit 1
fi

flex -o "$1.yy.c" "$1.l"

flex_status=$? # Capture the exit status of flex

if [ $flex_status -ne 0 ]; then
    echo "Flex failed to generate the C file." >&2
    exit 1
fi

gcc "$1.yy.c" -o "$1" -lfl -lm

gcc_status=$? # Capture the exit status of gcc

if [ $gcc_status -ne 0 ]; then
    echo "GCC failed to compile the executable." >&2
    rm "$1.yy.c"  # Clean up generated files before exiting
    exit 1
fi

rm "$1.yy.c"

echo "Compilation successful."

