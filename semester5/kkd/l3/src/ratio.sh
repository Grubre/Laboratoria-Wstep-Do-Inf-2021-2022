#!/bin/bash

calculate_ratio() {
  if [ $2 -eq 0 ]; then
    echo "Undefined (division by zero)"
  else
    ratio=$(awk "BEGIN { printf \"%.2f\", $1 / $2 }")
    echo $ratio
  fi
}

if [ $# -ne 2 ]; then
  echo "Usage: $0 <file1> <file2>"
  exit 1
fi

file1=$1
file2=$2

if [ ! -f "$file1" ]; then
  echo "File $file1 does not exist."
  exit 1
fi

if [ ! -f "$file2" ]; then
  echo "File $file2 does not exist."
  exit 1
fi

size1=$(stat -c %s "$file1")
size2=$(stat -c %s "$file2")

ratio=$(calculate_ratio $size1 $size2)

echo "Size of $file1: $size1 bytes"
echo "Size of $file2: $size2 bytes"
echo "Compression Ratio: $ratio"
