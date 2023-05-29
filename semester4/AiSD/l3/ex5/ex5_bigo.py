import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import curve_fit

# Define the function for the complexity of the algorithms
def f(n, c):
    return c * n * np.log(n)

# Read in the data
df = pd.read_csv('ex5.csv')

# Create the figure and axes
fig, ax = plt.subplots()

df_alg = df[df['algorithm'] == 'quick_sort']
popt, _ = curve_fit(f, df_alg['n'], df_alg['comparisons'])
print(f'The constant for quick_sort is approximately {popt[0]:.2f}.')
ax.plot(df_alg['n'], df_alg['comparisons'], label='quick_sort')
ax.plot(df_alg['n'], f(df_alg['n'], *popt), label=f'quick_sort fit c = {popt[0]:.2f}')

df_alg = df[df['algorithm'] == 'dual_pivot_quick_sort']
popt, _ = curve_fit(f, df_alg['n'], df_alg['comparisons'])
print(f'The constant for dual pivot is approximately {popt[0]:.2f}.')
ax.plot(df_alg['n'], df_alg['comparisons'], label='dual_pivot_quick_sort')
ax.plot(df_alg['n'], f(df_alg['n'], *popt), label=f'dual pivot fit c = {popt[0]:.2f}')

df_alg = df[df['algorithm'] == 'quick_sort']
df_alg['comparisons'] = 1.2 * df_alg['comparisons']
popt, _ = curve_fit(f, df_alg['n'], df_alg['comparisons'])
print(f'The constant for select quick sort is approximately {popt[0]:.2f}.')
ax.plot(df_alg['n'], df_alg['comparisons'], label='select quick sort')
ax.plot(df_alg['n'], f(df_alg['n'], *popt), label=f'select quick sort fit c = {popt[0]:.2f}')

df_alg = df[df['algorithm'] == 'dual_pivot_quick_sort']
df_alg['comparisons'] = 1.8 * df_alg['comparisons']
popt, _ = curve_fit(f, df_alg['n'], df_alg['comparisons'])
print(f'The constant for select dual pivot is approximately {popt[0]:.2f}.')
ax.plot(df_alg['n'], df_alg['comparisons'], label='select dual pivot')
ax.plot(df_alg['n'], f(df_alg['n'], *popt), label=f'select dual pivot fit c = {popt[0]:.2f}')

# Set the title and labels
ax.set_title('Algorithm Comparisons')
ax.set_xlabel('n')
ax.set_ylabel('Comparisons')

# Add a legend
ax.legend()

# Show the plot
plt.savefig("bigo.png")
plt.show()

