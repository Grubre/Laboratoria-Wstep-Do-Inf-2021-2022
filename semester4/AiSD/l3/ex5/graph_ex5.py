import pandas as pd
import matplotlib.pyplot as plt

# Load the data from the CSV file
data = pd.read_csv('ex5.csv')

# Create a figure with two subplots: one for comparisons, one for swaps
fig, axes = plt.subplots(nrows=2, ncols=1, figsize=(10, 8))

# List of algorithms
algorithms = ['quick_sort', 'select_quick_sort', 'dual_pivot_quick_sort', 'select_dual_pivot_quick_sort']

# For each algorithm, plot the number of comparisons and swaps as a function of n

subset = data[data['algorithm'] == 'quick_sort']
axes[0].plot(subset['n'], subset['comparisons'], label='quick_sort')
axes[1].plot(subset['n'], subset['swaps'], label='quick_sort')

subset = data[data['algorithm'] == 'dual_pivot_quick_sort']
axes[0].plot(subset['n'], subset['comparisons'], label='dual_pivot_quick_sort')
axes[1].plot(subset['n'], subset['swaps'], label='dual_pivot_quick_sort')

subset = data[data['algorithm'] == 'select_quick_sort']
axes[0].plot(subset['n'], subset['comparisons'], label='select_quick_sort')
axes[1].plot(subset['n'], subset['swaps'], label='select_quick_sort')

subset = data[data['algorithm'] == 'select_dual_pivot_quick_sort']
axes[0].plot(subset['n'], subset['comparisons'], label='select_dual_pivot_quick_sort')
axes[1].plot(subset['n'], subset['swaps'], label='select_dual_pivot_quick_sort')


# for algo in algorithms:
#     subset = data[data['algorithm'] == algo]
#     axes[0].plot(subset['n'], subset['comparisons'], label=algo)
#     axes[1].plot(subset['n'], subset['swaps'], label=algo)

# Set the labels for the x-axis and y-axis
axes[0].set(xlabel='n', ylabel='Comparisons')
axes[1].set(xlabel='n', ylabel='Swaps')

# Set the title of the subplots
axes[0].set_title('Number of comparisons for different sorting algorithms')
axes[1].set_title('Number of swaps for different sorting algorithms')

# Add a legend to each subplot
axes[0].legend()
axes[1].legend()

# Adjust the layout and display the plot
plt.tight_layout()
plt.savefig('ex5.png')
