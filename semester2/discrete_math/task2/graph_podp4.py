import matplotlib.pyplot as plt
import numpy as np
import sys

def get_results(filename):
    f = open(filename, "r")
    indexes = []
    simulation = []
    lines = f.readlines()
    for x in range(0,len(lines),2):
        indexes.append(int(lines[x]))
        simulation.append(float(lines[x+1]))
    f.close()
    return (indexes,simulation)


def f(x):
    return x

numbers = get_results("k_10000_podp4_graph_output.txt")

indexes = numbers[0]
simulation = numbers[1]

average_list = [x/y for x,y in zip(indexes,simulation)]
average = sum(average_list) / len(average_list)

for x,y in zip(indexes,simulation):
    print("constant = " + str(y/x))

golomb = 0.624329

y = golomb * np.arange(0,len(numbers[1]),1) + 0.2

plt.plot(indexes,simulation, alpha=1, label="average max length", color="teal")
plt.plot(indexes,y, label="y=0.6254x", color="red")
plt.xlabel("dlugosc ciagu")
plt.ylabel("srednia ilosc cykli permutacji")
plt.legend() 
plt.show()