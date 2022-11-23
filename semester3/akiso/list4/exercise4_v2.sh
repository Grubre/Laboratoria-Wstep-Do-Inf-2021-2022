IFS=$'\n'
files=$(find . -type f | sort)
hashes=""

for file in ${files[*]}
do
    hashes+=$(sha256sum $file)"\n"
    echo "$file"
done

echo $hashes

# to get just the hash, without filename
# | cut -d " " -f 1 

# for ((i = 0; i < ${#hashes[@]}; i++)); do
#     echo -e ${hashes[i]}
# done

# arr_length=${#hashes[@]}
#
# duplicates=()
#
# # echo "arr_len="$arr_length
#
# for hash in ${hashes[@]}
# do
#     echo "========================================================================================="
#     echo $hash":"
#     left=0
#     right=$((arr_length-1))
#     # cut_hash is just the hash, without filename
#     # original variable looks like (hash) (filename)
#     cut_hash=$(cut -d " " -f 1 <<< $hash)
#     while [[ $left -le $right ]]; do
#         mid=$((left + ((right - left) / 2)))
#         mid_val=${hashes[$mid]}
#         cut_mid=$(cut -d " " -f 1 <<< $mid_val)
#         echo $mid
#         if [[ $cut_mid > $cut_hash ]]; then
#             right=$((right - mid-1))
#         elif [[ $cut_mid < $cut_hash ]]; then
#             left=$((mid+1))
#         elif [[ $mid_val != $hash ]]; then
#             duplicates+=($hash)
#             break
#         else
#             break
#         fi
#     done
# done

# IFS
uniq -D --check-chars=64 ${hashes} | echo -e
# for dup in ${duplicates[@]}
# do
#     echo $dup
# done
