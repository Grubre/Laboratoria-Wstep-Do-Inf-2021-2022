accepted_build_types=("Release" "Debug")

if [ ! -z "$1" ]; then
    for acc in ${accepted_build_types[@]}; do
        if [[ $1 == $acc ]]; then
            ./build/$acc/mpislist2
        fi
    done
else
    echo "Build type not provided!"
fi
