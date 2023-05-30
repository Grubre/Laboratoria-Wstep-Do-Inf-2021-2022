import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Read the data from csv files
increasing_data = pd.read_csv('./data/increasing_data.csv')
random_data = pd.read_csv('./data/random_data.csv')

# Calculate means over the repetitions
increasing_data = increasing_data.groupby('n').mean().reset_index()
random_data = random_data.groupby('n').mean().reset_index()

# Initialize a subplot grid
fig, axs = plt.subplots(3, 2, figsize=(15, 20))

# List of features to compare
features = ['max_comparisons', 'max_pointer_reads', 'max_pointer_substitutions', 
            'avg_comparisons', 'avg_pointer_reads', 'avg_pointer_substitutions']

# Iterate over features and create subplots
for i, feature in enumerate(features):
    ax = axs[i//2, i%2]
    sns.lineplot(data=increasing_data, x='n', y=feature, ax=ax, label='Increasing Data')
    sns.lineplot(data=random_data, x='n', y=feature, ax=ax, label='Random Data')
    ax.set_ylabel(feature)
    ax.legend()

plt.tight_layout()

# Save the figure
plt.savefig('comparison_plot.png')

