import pandas as pd
import matplotlib.pyplot as plt

# Load the data
df = pd.read_csv('data/ex3.csv')

# Create a figure
fig, ax = plt.subplots(2, 1, figsize=(10, 10))

# Loop over each group size
for group_size in df['group_size'].unique():
    # Create a mask for the current group size
    mask = df['group_size'] == group_size

    # Plot the number of comparisons for this group size
    ax[0].plot(df[mask]['n'], df[mask]['comparisons'], label=f'Group Size {group_size}')

    # Plot the number of swaps for this group size
    ax[1].plot(df[mask]['n'], df[mask]['swaps'], label=f'Group Size {group_size}')

# Set the labels and title for the first plot
ax[0].set_xlabel('n')
ax[0].set_ylabel('Number of Comparisons')
ax[0].set_title('Number of Comparisons for k = 3,5,7,9')
ax[0].legend()

# Set the labels and title for the second plot
ax[1].set_xlabel('n')
ax[1].set_ylabel('Number of Swaps')
ax[1].set_title('Number of Swaps for k = 3,5,7,9')
ax[1].legend()

# Show the plots
plt.tight_layout()
plt.savefig("plots/ex3.png")
plt.show()

