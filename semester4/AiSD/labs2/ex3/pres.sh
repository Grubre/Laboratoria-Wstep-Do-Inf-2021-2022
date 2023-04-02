test_cases=(8 16 32)

for i in ${test_cases[@]}; do
echo "=============================================="
echo "DUAL PIVOT QUICK SORT (n = $i):"
echo "=============================================="
echo "LOSOWA PERMUTACJA:"
../random_generator $i | ./dual_pivot_pres
echo "MALEJACA PERMUTACJA:"
../decreasing_generator $i | ./dual_pivot_pres
echo "ROSNACA PERMUTACJA:"
../increasing_generator $i | ./dual_pivot_pres
done
