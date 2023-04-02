test_cases=(8 16 32)

for i in ${test_cases[@]}; do
echo "=============================================="
echo "HYBRID(QUICK + INSERTION) SORT (n = $i):"
echo "=============================================="
echo "LOSOWA PERMUTACJA:"
../random_generator $i | ./hybrid_sort_pres
echo "MALEJACA PERMUTACJA:"
../decreasing_generator $i | ./hybrid_sort_pres
echo "ROSNACA PERMUTACJA:"
../increasing_generator $i | ./hybrid_sort_pres
done
