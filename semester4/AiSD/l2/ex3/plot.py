import matplotlib.pyplot as plt

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
n_merge, avg_comp_merge, avg_swaps_merge = process_data("merge.txt")
n_insertion, avg_comp_insertion, avg_swaps_insertion = process_data("insertion.txt")
n_quick, avg_comp_quick, avg_swaps_quick = process_data("quick.txt")
n_pivot, avg_comp_pivot, avg_swaps_pivot = process_data("dual_pivot.txt")

# Figure 1: Average Comparisons
plt.figure()
plt.plot(n_merge, avg_comp_merge, label="Merge Sort", marker="o")
plt.plot(n_insertion, avg_comp_insertion, label="Insertion Sort", marker="o")
plt.plot(n_quick, avg_comp_quick, label="Quick Sort", marker="o")
plt.plot(n_pivot, avg_comp_pivot, label="Dual Pivot Quick Sort", marker="o")
plt.xlabel("n")
plt.ylabel("Average Comparisons")
plt.title("n vs Average Comparisons")
plt.legend()
plt.show()

# Figure 2: Average Swaps
plt.figure()
plt.plot(n_merge, avg_swaps_merge, label="Merge Sort", marker="o")
plt.plot(n_insertion, avg_swaps_insertion, label="Insertion Sort", marker="o")
plt.plot(n_quick, avg_swaps_quick, label="Quick Sort", marker="o")
plt.plot(n_pivot, avg_swaps_pivot, label="Dual Pivot Quick Sort", marker="o")
plt.xlabel("n")
plt.ylabel("Average Swaps")
plt.title("n vs Average Swaps")
plt.legend()
plt.show()

# Figure 3: Average Comparisons divided by n
plt.figure()
plt.plot(n_merge, [comp / n for n, comp in zip(n_merge, avg_comp_merge)], label="Merge Sort", marker="o")
plt.plot(n_insertion, [comp / n for n, comp in zip(n_insertion, avg_comp_insertion)], label="Insertion Sort", marker="o")
plt.plot(n_quick, [comp / n for n, comp in zip(n_quick, avg_comp_quick)], label="Quick Sort", marker="o")
plt.plot(n_pivot, [comp / n for n, comp in zip(n_pivot, avg_comp_pivot)], label="Dual Pivot Quick Sort", marker="o")
plt.xlabel("n")
plt.ylabel("Average Comparisons / n")
plt.title("n vs Average Comparisons / n")
plt.legend()
plt.show()

# Figure 4: Average Swaps divided by n
plt.figure()
plt.plot(n_merge, [swap / n for n, swap in zip(n_merge, avg_swaps_merge)], label="Merge Sort", marker="o")
plt.plot(n_insertion, [swap / n for n, swap in zip(n_insertion, avg_swaps_insertion)], label="Insertion Sort", marker="o")
plt.plot(n_quick, [swap / n for n, swap in zip(n_quick, avg_swaps_quick)], label="Quick Sort", marker="o")
plt.plot(n_pivot, [comp / n for n, comp in zip(n_pivot, avg_swaps_pivot)], label="Dual Pivot Quick Sort", marker="o")
plt.xlabel("n")
plt.ylabel("Average Swaps / n")
plt.title("n vs Average Swaps / n")
plt.show()
