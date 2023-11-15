#!/bin/bash

# Check if the correct number of arguments is provided
if [ "$#" -lt 2 ] || [ "$#" -gt 5 ]; then
    echo "Usage: $0 <flex_file_name> <test_name> [-p] [-o <output_file>]"
    exit 1
fi

# Flags initialization
print_output=false
output_file=""

# Parse optional flags
for ((i=3; i<=$#; i++)); do
    if [ "${!i}" == "-p" ]; then
        print_output=true
    elif [ "${!i}" == "-o" ]; then
        ((i++))
        output_file="${!i}"
    fi
done

# Compile the Flex file
./compile.sh "$1"

compile_status=$?
if [ $compile_status -ne 0 ]; then
    echo "Error: Compilation of \"$1\" failed with exit code $compile_status"
    exit 1
fi

# Check if the Flex binary exists
if [ ! -x "$1" ]; then
    echo "Error: The Flex binary \"$1\" does not exist or is not executable"
    exit 1
fi

# Check if test files exist
if [ ! -f "tests/$2.txt" ] || [ ! -f "tests/$2_expected.txt" ]; then
    echo "Error: Test files for \"$2\" not found"
    exit 1
fi

# Run the test and store output in a file if -o option is provided
output=$(cat "tests/$2.txt" | ./"$1")
if [ -n "$output_file" ]; then
    echo "$output" > "$output_file"
fi

# Print output to stdout if -p option is provided
if [ "$print_output" == true ]; then
    echo "Output for test \"$2\":"
    echo "$output"
fi

# Run the test and check for differences
colordiff -b -y -s <(echo "$output") "tests/$2_expected.txt"
