echo "Building decoder"
./build.sh decoder
echo "Building encoder"
./build.sh encoder

echo "Encoding..."
./encoder $1 encoded $2
echo "Decoding..."
./decoder encoded decoded $2

./ratio.sh $1 encoded

diff -s $1 decoded
