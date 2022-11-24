IFS=$'\n\t\0'
read -r -a sums -d '\0' <<< $(find . -type f -print0 | xargs -0 sha256sum)
read -r -a sizes -d '\0' <<< $(find . -type f -print0 | xargs -0 du -sh -b | cut -f1)
for ((i = 0; i < ${#sums[@]}; i++)); do
    arr+=${sizes[i]}$'\t'${sums[i]}$'\t'${sizes[i]}$'\n'
done
# Najpierw sortujemy po hashu (2 kolumna) potem po wielkosci (1 kolumna)
read -r -a sorted -d '\0' <<< $(sort -n -r -k 2,1 <<< ${arr[@]} | sed '/^[[:space:]]*$/d' | cut -d $'\t' -f 2,3 | column -t)
for ((i = 0; i < ${#sorted[@]}; i++)); do
    cut1=$(echo ${sorted[i]} | cut -f1 -d ' ')
    cut_behind=""
    cut_ahead=""
    if [[ $i -ge 1 ]]; then
        cut_behind=$(echo ${sorted[i-1]} | cut -f1 -d ' ')
    fi
    if [[ $i -lt ${#sorted[@]} ]]; then
        cut_ahead=$(echo ${sorted[i+1]} | cut -f1 -d ' ')
    fi
    if [[ $cut1 == $cut_behind || $cut1 == $cut_ahead ]]; then
        echo ${sorted[i]}
    fi
done
