IFS=$'\n'
files=$(find . -type f | sort)
hashes=()

for file in ${files[*]}
do
    hashes+=($(sha256sum $file))
done

arr_length=${#hashes[@]}

duplicates=()

for hash in ${hashes[@]}
do
    left=0
    right=$((arr_length-1))
    cut_hash=$(cut -d " " -f 1 <<< $hash)
    while [[ $left -le $right ]]; do
        mid=$((left + ((right - left) / 2)))
        mid_val=${hashes[$mid]}
        cut_mid=$(cut -d " " -f 1 <<< $mid_val)
        if [[ $cut_mid > $cut_hash ]]; then
            right=$((right - mid-1))
        elif [[ $cut_mid < $cut_hash ]]; then
            left=$((mid+1))
        elif [[ $mid_val != $hash ]]; then
            duplicates+=($hash)
            break
        else
            if [[ $mid -ge 1 ]]; then
                cut_mid=$(cut -d " " -f 1 <<< $hashes[$((mid-1))])
                if [[ $cut_mid == $cut_hash ]]; then
                    duplicates+=($hash)
                    break
                fi
            fi
            if [[ $mid -le $((arr_length-1)) ]]; then
                cut_mid=$(cut -d " " -f 1 <<< $hashes[$((mid+1))])
                if [[ $cut_mid == $cut_hash ]]; then
                    duplicates+=($hash)
                    break
                fi
            fi
            break
        fi
    done
done

for dup in ${duplicates[@]}
do
    name=$(cut -d " " -f 2- <<< $dup)
    echo $(du <<< $name)
done
