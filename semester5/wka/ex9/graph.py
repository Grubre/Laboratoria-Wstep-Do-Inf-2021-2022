
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np

# Read the CSV file
file_path = './data.csv'  # Replace with the actual file path
data = pd.read_csv(file_path)

# Extract data columns
no_fixed_points_percentage = data['no_fixed_points_percentage']
one_fixed_point_percentage = data['one_fixed_point_percentage']
cycles = data['cycles']

# Create scatter plots
plt.figure(figsize=(10, 6))

# Scatter plot for no_fixed_points_percentage
plt.subplot(3, 1, 1)
plt.scatter(range(len(no_fixed_points_percentage)), no_fixed_points_percentage, c='b', s=10)
plt.title('No Fixed Points')
plt.xlabel('Data Point')
plt.ylabel('Percentage')
plt.ylim(0, 1)  # Set y-axis limits to [0, 1]

# Scatter plot for one_fixed_point_percentage
plt.subplot(3, 1, 2)
plt.scatter(range(len(one_fixed_point_percentage)), one_fixed_point_percentage, c='g', s=10)
plt.title('One Fixed Point')
plt.xlabel('Data Point')
plt.ylabel('Percentage')
plt.ylim(0, 1)  # Set y-axis limits to [0, 1]

# Scatter plot for cycles
plt.subplot(3, 1, 3)
plt.scatter(range(len(cycles)), cycles, c='r', s=10)
plt.title('Cycles Count')
plt.xlabel('Data Point')
plt.ylabel('Cycles')

# Save the graph to an image file
output_image_path = 'graph.png'
plt.tight_layout()
plt.savefig(output_image_path)
