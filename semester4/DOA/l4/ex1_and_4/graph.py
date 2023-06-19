#!/usr/bin/env python
# import pandas as pd
# import matplotlib.pyplot as plt
#
# # Read the data from the CSV file
# karp = pd.read_csv("./karp.csv")
# dinic = pd.read_csv("./dinic.csv")
#
# # Separate figures for each value
# for value in ['time', 'maxflow', 'paths']:
#     plt.figure()  # Create a new figure
#     plt.scatter(karp['k'], karp[value], marker='o', label='Edmond-Karp')  # Plot the data
#     # plt.scatter(dinic['k'], dinic[value], marker='o', label='Dinic')  # Plot the data
#     plt.xlabel('k')  # Set the x-axis label
#     if value == 'time':
#         plt.ylabel('czas')  # Set the y-axis label
#         plt.title(f'Wykres sredniego czasu wykonania od k [ms]')  # Set the title
#         # plt.scatter(dinic['k'], dinic[value], marker='o', label='Dinic')  # Plot the data
#     if value == 'maxflow':
#         plt.ylabel('przeplyw')  # Set the y-axis label
#         plt.title(f'Wykres sredniego maksymalnego przeplywu od k')  # Set the title
#         # plt.scatter(dinic['k'], karp[value], marker='o', label='Dinic')  # Plot the data
#     if value == 'paths':
#         plt.ylabel('liczba sciezek')  # Set the y-axis label
#         plt.title(f'Wykres sredniej liczby sciezek od k')  # Set the title
#         # plt.scatter(dinic['k'], karp[value], marker='o', label='Dinic')  # Plot the data
#     plt.legend()  # Add a legend
#     plt.grid(True)  # Add grid lines
#     plt.savefig('plots/karp_' + value + '.png')  # Show the figure
#

import pandas as pd
import matplotlib.pyplot as plt

# Read the data from the CSV file
data = pd.read_csv("./dinic_vs_karp.csv")

# Separate figures for each value
for value in ['time', 'maxflow', 'paths']:
    plt.figure()  # Create a new figure
    for algo in data['algorithm'].unique():
        plt.scatter(data[data['algorithm']==algo]['k'], data[data['algorithm']==algo][value], label=algo)  # Plot the data for each algorithm
    plt.xlabel('k')  # Set the x-axis label
    plt.ylabel(value)  # Set the y-axis label
    plt.title(f'Comparison of {value} between algorithms')  # Set the title
    plt.legend()  # Add legend
    plt.grid(True)  # Add grid lines
    plt.savefig(f'plots/karp_vs_dinic_{value}.png')  # Save the figure
    plt.close()  # Close the figure to free memory
