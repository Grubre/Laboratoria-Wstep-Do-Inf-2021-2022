
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

data = pd.read_csv('data/ex2.csv')

average_data = data.groupby(['n', 'algorithm']).mean().reset_index()

quickselect_data = average_data[average_data['algorithm'] == 'quickselect']
randomized_select_data = average_data[average_data['algorithm'] == 'randomized_select']

fig, axs = plt.subplots(2, figsize=(10, 12))

# Number of Comparisons plot
axs[0].plot(quickselect_data['n'], quickselect_data['comparisons'], label='select')
axs[0].plot(randomized_select_data['n'], randomized_select_data['comparisons'], label='randomized Select')
axs[0].set_xlabel('n')
axs[0].set_ylabel('Average Number of Comparisons')
axs[0].legend()
axs[0].set_title('Number of Comparisons: select vs randomized Select')
axs[0].grid(True)

# Number of Swaps plot
axs[1].plot(quickselect_data['n'], quickselect_data['swaps'], label='select')
axs[1].plot(randomized_select_data['n'], randomized_select_data['swaps'], label='randomized Select')
axs[1].set_xlabel('n')
axs[1].set_ylabel('Average Number of Swaps')
axs[1].legend()
axs[1].set_title('Number of Swaps: select vs randomized Select')
axs[1].grid(True)

plt.tight_layout()
plt.savefig('plots/ex2.png')  # Save the figure
plt.show()

