import matplotlib.pyplot as plt
import numpy as np
import sys

def get_results(filename):
    f = open(filename, "r")
    indexes = []
    harmonics = []
    simulation = []
    lines = f.readlines()
    for x in range(0,len(lines),3):
        indexes.append(int(lines[x]))
        harmonics.append(float(lines[x+1]))
        simulation.append(float(lines[x+2]))
    f.close()
    return (indexes,harmonics,simulation)

k1 = get_results("k_1_graph_output.txt")
k10 = get_results("k_10_graph_output.txt")
k100 = get_results("k_100_graph_output.txt")
k1000 = get_results("k_1000_graph_output.txt")
k10000 = get_results("k_10000_graph_output.txt")
k100000 = get_results("k_100000_graph_output.txt")

indexes = k10000[0]
harmonics = k10000[1]
simulation1 = k1[2]
simulation10 = k10[2]
simulation100 = k100[2]
simulation1000 = k1000[2]
simulation10000 = k10000[2]
simulation100000 = k100000[2]






plt.plot(indexes,simulation100000, alpha=0.8, label="k=100000", color="pink")
plt.plot(indexes,simulation10000, alpha=0.8, label="k=10000", color="green")
plt.plot(indexes,simulation1000, alpha=0.8, label="k=1000", color= "blue")
plt.plot(indexes,simulation100, alpha=0.8, label="k=100", color="orange")
plt.plot(indexes,simulation10, alpha=0.6, label="k=10", color="yellow")
plt.plot(indexes,simulation1, alpha=0.4, label="k=1", color="teal")
plt.plot(indexes,harmonics, alpha=0.3, label="szereg harmoniczny", linewidth = 5, color="red")
plt.xlabel("dlugosc ciagu")
plt.ylabel("srednia ilosc cykli permutacji")
plt.legend() 
plt.show()

average_list = [abs(x-y) for x,y in zip(harmonics,simulation1)]
average1 = sum(average_list) / len(average_list)
print("k = 1: "+str(average1))
average_list = [abs(x-y) for x,y in zip(harmonics,simulation10)]
average10 = sum(average_list) / len(average_list)
print("k = 10: "+str(average10))
average_list = [abs(x-y) for x,y in zip(harmonics,simulation100)]
average100 = sum(average_list) / len(average_list)
print("k = 100: "+str(average100))
average_list = [abs(x-y) for x,y in zip(harmonics,simulation1000)]
average1000 = sum(average_list) / len(average_list)
print("k = 1000: "+str(average1000))
average_list = [abs(x-y) for x,y in zip(harmonics,simulation10000)]
average10000 = sum(average_list) / len(average_list)
print("k = 10000: "+str(average10000))
average_list = [abs(x-y) for x,y in zip(harmonics,simulation100000)]
average100000 = sum(average_list) / len(average_list)
print("k = 100000: "+str(average100000))