import random
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt

print("executing...")

def c_func(idx):
    return 2137


def construct_N_matrix():
    N = [[0 if i == j else random.randrange(1, 500) for i in range(0, 20)] for j in range(0, 20)]
    return N


def construct_graph():
    G = nx.null_graph()
    n = 20
    G.add_edges_from([(i, (i) % (n - 1) + 1, {"c": c_func(i)}) for i in range(1, n)])
    G.add_edges_from([(0, i) for i in range(1, n)])
    # G.add_edges_from([(i, (i+ 20 // 2) % 20, {"c": c_func(i)}) for i in range(0, 20, 3)])
    for i in G.edges(data = True):
        pass
        # print(i)
    return G


def reset_flow_func(G: nx.Graph):
    for i in G.edges:
        edge = G[i[0]][i[1]]
        edge["a"] = 0

def set_probability(G: nx.Graph, p):

    pass

def simulate_flow(N, G: nx.Graph):
    reset_flow_func(G)
    for j in range(len(N)):
        for i in range(len(N[j])):
            print("Going from " + str(i) + " to " + str(j))
            path = nx.shortest_path(G,i,j)
            if len(path) > 1:
                print(path)
                for e in range(len(path) - 1):
                    G[path[e]][path[e + 1]]["a"] += N[path[e]][path[e+1]]


def draw_circular_with_center(G: nx.Graph):
    pos = nx.circular_layout(G)

    center_x = sum(x for x, _ in pos.values()) / len(pos)
    center_y = sum(y for x, y in pos.values()) / len(pos)
    pos[0] = np.array([center_x, center_y])


    nx.draw(G, pos)
    edge_labels = nx.get_edge_attributes(G,'a')
    nx.draw_networkx_edge_labels(G, pos, edge_labels = edge_labels)
    nx.draw_networkx_labels(G, pos)


graph = construct_graph()
N = construct_N_matrix()
G = sum(sum(N,[]))
print(N)
reset_flow_func(graph)
set_probability(graph, 95)
simulate_flow(N,graph)
draw_circular_with_center(graph)
plt.savefig('graph.png', transparent = True)
plt.show()
