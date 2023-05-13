test_cases=(8 16 32)

for i in ${test_cases[@]}; do
echo "=============================================="
echo "INSERTION SORT:"
echo "=============================================="
echo "LOSOWA PERMUTACJA:"
../random_generator $i | ./insertion_sort
echo "MALEJACA PERMUTACJA:"
../decreasing_generator $i | ./insertion_sort
echo "ROSNACA PERMUTACJA:"
../increasing_generator $i | ./insertion_sort
echo "=============================================="
echo "MERGE SORT:"
echo "=============================================="
echo "LOSOWA PERMUTACJA:"
../random_generator $i | ./merge_sort
echo "MALEJACA PERMUTACJA:"
../decreasing_generator $i | ./merge_sort
echo "ROSNACA PERMUTACJA:"
../increasing_generator $i | ./merge_sort
echo "=============================================="
echo "QUICK SORT:"
echo "=============================================="
echo "LOSOWA PERMUTACJA:"
../random_generator $i | ./quick_sort
echo "MALEJACA PERMUTACJA:"
../decreasing_generator $i | ./quick_sort
echo "ROSNACA PERMUTACJA:"
../increasing_generator $i | ./quick_sort
done
