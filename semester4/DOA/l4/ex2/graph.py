#!/usr/bin/env python
import pandas as pd
import matplotlib.pyplot as plt

# Read the CSV file
data = pd.read_csv('ex2.csv')

# Calculate the average maxFlow for each pair of k and i
data = data.groupby(['k', 'i'])['maxFlow'].mean().reset_index()

# Get the unique values of k
k_values = data['k'].unique()
for k in k_values:
    # Filter the data for the current k value
    k_data = data[data['k'] == k]

    # Create a new figure for the current k value
    plt.figure()

    # Create a scatter plot of i vs. maxFlow
    plt.scatter(k_data['i'], k_data['maxFlow'])

    # Add title and labels
    plt.title(f'k = {k}')
    plt.xlabel('i')
    plt.ylabel('maxFlow')

    # Show the plot
    plt.savefig('plots/k' + str(k) + '.png')
