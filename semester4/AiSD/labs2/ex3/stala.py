import matplotlib.pyplot as plt
import math

def process_data(filename):
    with open(filename, "r") as file:
        data = [list(map(int, line.strip().split())) for line in file]

    n_values = sorted(set(row[0] for row in data))
    comp_sum = {n: 0 for n in n_values}
    swap_sum = {n: 0 for n in n_values}
    counts = {n: 0 for n in n_values}

    for row in data:
        n, comparisons, swaps = row
        comp_sum[n] += comparisons
        swap_sum[n] += swaps
        counts[n] += 1

    avg_comparisons = [comp_sum[n] / counts[n] for n in n_values]
    avg_swaps = [swap_sum[n] / counts[n] for n in n_values]

    return n_values, avg_comparisons, avg_swaps

# Process data from each file
n_pivot, avg_comp_pivot, avg_swaps_pivot = process_data("dual_pivot.txt")

# Figure 1: Average Comparisons
plt.figure()
plt.plot(n_pivot, [comp + swap for comp, swap in zip(avg_comp_pivot, avg_swaps_pivot)], label="Dual Pivot Quick Sort", marker="o")
plt.plot(n_pivot, [(n * math.log2(n)) for n in n_pivot], label="n log n", marker="o")
plt.xlabel("n")
plt.ylabel("Average Comparisons")
plt.title("n vs Average Comparisons")
plt.legend()
plt.show()


