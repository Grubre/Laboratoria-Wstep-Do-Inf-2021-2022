#!bin/bash
OIFS="$IFS"
IFS=$'\n'
for file in $(find . -type f)
do
    lowercased=$(echo $file | tr '[A-Z]' '[a-z]')
    if [[ $file != $lowercased ]]; then
        mv $file $lowercased
    fi
done
