import networkx as nx
import numpy as np
import matplotlib.pyplot as plt

A = [
    [0, 1, 0, 1, 0, 0, 0, 0, 1, 1],
    [1, 0, 1, 0, 1, 0, 0, 0, 0, 0],
    [0, 1, 0, 1, 0, 1, 0, 0, 1, 0],
    [1, 0, 1, 0, 1, 0, 1, 0, 0, 1],
    [0, 1, 0, 1, 0, 0, 0, 1, 1, 0],
    [0, 0, 1, 0, 0, 0, 1, 0, 0, 1],
    [0, 0, 0, 1, 0, 1, 0, 1, 0, 0],
    [0, 0, 0, 0, 1, 0, 1, 0, 1, 0],
    [1, 0, 1, 0, 1, 0, 0, 1, 0, 0],
    [1, 0, 0, 1, 0, 1, 0, 0, 0, 0]
]

c = [
    [0,  57,  0,  91,  0,  0,  0,  0,  85,  20],
    [57,  0,  23,  0,  48,  0,  0,  0,  0,  0],
    [0,  23,  0,  15,  0,  47,  0,  0,  98,  0],
    [91,  0,  15,  0,  60,  0,  21,  0,  0,  67],
    [0,  48,  0,  60,  0,  0,  0,  43,  44,  0],
    [0,  0,  47,  0,  0,  0,  13,  0,  0,  55],
    [0,  0,  0,  21,  0,  13,  0,  11,  0,  0],
    [0,  0,  0,  0,  43,  0,  11,  0,  16,  0],
    [85,  0,  98,  0,  44,  0,  0,  16,  0,  0],
    [20,  0,  0,  67,  0,  55,  0,  0,  0,  0]
]

t = [
    [0,  5,  0,  7,  0,  0,  0,  0,  6,  4],
    [5,  0,  3,  0,  2,  0,  0,  0,  0,  0],
    [0,  3,  0,  1,  0,  4,  0,  0,  8,  0],
    [7,  0,  1,  0,  6,  0,  5,  0,  0,  7],
    [0,  2,  0,  6,  0,  0,  0,  3,  4,  0],
    [0,  0,  4,  0,  0,  0,  1,  0,  0,  5],
    [0,  0,  0,  0,  0,  1,  0,  0,  8,  0],
    [0,  0,  0,  0,  3,  0,  1,  0,  1,  0],
    [6,  0,  8,  0,  4,  0,  0,  1,  0,  0],
    [4,  0,  0,  7,  0,  5,  0,  0,  0,  0]
]

G = nx.from_numpy_array(np.array(A), create_using=nx.DiGraph)

node_labels = {i: i+1 for i in range(len(A))}
nx.set_node_attributes(G, node_labels, "label")

edge_labels_c = {(i, j): c[i][j] for i in range(len(A)) for j in range(len(A)) if A[i][j] == 1}
edge_labels_t = {(i, j): t[i][j] for i in range(len(A)) for j in range(len(A)) if A[i][j] == 1}
edge_labels = {(i, j): f"c={edge_labels_c[(i, j)]}, t={edge_labels_t[(i, j)]}" for i, j in edge_labels_c.keys()}

pos = nx.spring_layout(G, seed=42, iterations= 1000)
nx.draw(G, pos, with_labels=True, node_size=800, font_size=16, font_weight="bold", node_color="skyblue")
nx.draw_networkx_edge_labels(G, pos, edge_labels=edge_labels, font_size=12, font_weight="bold")

plt.savefig("graph.png", transparent = True)
plt.show()
