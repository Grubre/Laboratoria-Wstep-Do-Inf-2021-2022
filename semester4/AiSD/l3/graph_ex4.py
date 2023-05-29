
import pandas as pd
import matplotlib.pyplot as plt

# Load the data
fixed_values_data = pd.read_csv('data/ex4.csv')
random_values_data = pd.read_csv('data/ex4_random.csv')

# Average the results for the random values
random_values_data = random_values_data.groupby(['n', 'v']).mean().reset_index()

# Combine the data
data = pd.concat([fixed_values_data, random_values_data])

# Create subplots for comparisons and time
fig, axs = plt.subplots(2, figsize=(10, 12))

# Create a list of unique 'v' labels
labels = data['v'].unique()

# Plot comparisons
for label in labels:
    subset = data[data['v'] == label]
    axs[0].plot(subset['n'], subset['comparisons'], label='v = ' + label)
axs[0].set_xlabel('n')
axs[0].set_ylabel('Average Number of Comparisons')
axs[0].legend()
axs[0].grid(True)

# Plot time
for label in labels:
    subset = data[data['v'] == label]
    axs[1].plot(subset['n'], subset['time'], label='v = ' + label)
axs[1].set_xlabel('n')
axs[1].set_ylabel('Average Time Taken (microseconds)')
axs[1].legend()
axs[1].grid(True)

# Set the title and save the figure
fig.suptitle('Binary Search Performance')
plt.savefig('plots/ex4.png')

