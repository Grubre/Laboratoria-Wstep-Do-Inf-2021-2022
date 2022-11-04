#!bin/bash
for file in *
do
    lowercased=""
    for (( i=0; i<${#file}; i++ ));
    do
        letter="${file:$i:1}"
        if [[ "$letter" =~ [A-Z] ]]; then
            letter=$(echo $letter | tr '[A-Z]' '[a-z]')
        fi
        lowercased="${lowercased}${letter}"
    done
    if [[ $file != $lowercased ]]; then
        mv $file $lowercased
    fi
done
