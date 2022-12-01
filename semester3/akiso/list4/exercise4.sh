IFS=$'\n\0'
read -r -a sums -d '\0' <<< $(find $1 -type f -print0 | xargs -0 sha256sum | cut -d ' ' -f1,3 --output-delimiter=$'\t')
read -r -a sizes -d '\0' <<< $(find $1 -type f -print0 | xargs -0 du -s -b | cut -f1)
for ((i = 0; i < ${#sums[@]}; i++)); do
    arr+=${sums[i]}$'\t'${sizes[i]}$'\n'
done
# Najpierw sortujemy po hashu (1 kolumna) potem po wielkosci (3 kolumna)
read -r -a sorted -d '\0' <<< $(sort -r -n -t $'\t' -k1b,1 -k2b,3 <<< ${arr[@]} | sed '/^[[:space:]]*$/d')
for ((i = 0; i < ${#sorted[@]}; i++)); do
    cut1=$(echo ${sorted[i]} | cut -f1 -d $'\t')
    cut_behind=""
    cut_ahead=""
    if [[ $i -ge 1 ]]; then
        cut_behind=$(echo ${sorted[i-1]} | cut -f1 -d $'\t')
    fi
    if [[ $i -lt ${#sorted[@]} ]]; then
        cut_ahead=$(echo ${sorted[i+1]} | cut -f1 -d $'\t')
    fi
    if [[ $cut1 == $cut_behind || $cut1 == $cut_ahead ]]; then
        echo ${sorted[i]} | cut -d $'\t' -f2,3
    fi
done
