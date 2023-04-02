clang++ merge_sort.cpp -o merge_sort -std=c++20
clang++ quick_sort.cpp -o quick_sort -std=c++20
clang++ dual_pivot.cpp -o dual_pivot_sort -std=c++20
clang++ insertion_sort.cpp -o insertion_sort -std=c++20
./quick_sort > quick.txt
./dual_pivot_sort > dual_pivot.txt
./merge_sort > merge.txt
./insertion_sort > insertion.txt
