IFS="\n"
finds=$(find . -type f -print0 | xargs -0 sha256sum | sort | uniq -w32 --all-repeated=separate)
echo $finds
