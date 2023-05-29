import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

# Load the data
data = pd.read_csv('data/ex4_random.csv')

# Group by 'n' and calculate the mean of 'comparisons'
average_data = data.groupby('n')['comparisons'].mean().reset_index()

# Define the theoretical O(logn) function
def log_func(n, c):
    return c * np.log2(n)

# Perform curve fitting to estimate the constant 'c'
params, _ = curve_fit(log_func, average_data['n'], average_data['comparisons'])

# Print the estimated constant
print(f"The hidden constant in the O(logn) complexity of binary search is approximately: {params[0]}")

# Create a plot
plt.figure(figsize=(10, 6))

# Plot the actual data
plt.plot(average_data['n'], average_data['comparisons'], label='Average comparisons')

# Plot the theoretical O(logn) function
plt.plot(average_data['n'], log_func(average_data['n'], params[0]), label='c * log2(n) with c=' + str(round(params[0], 2)))

plt.xlabel('n')
plt.ylabel('Average Number of Comparisons')
plt.legend()
plt.title('Binary Search: Average Number of Comparisons vs. n')
plt.grid(True)
plt.savefig('plots/ex4_random.png')  # Save the figure
plt.show()

