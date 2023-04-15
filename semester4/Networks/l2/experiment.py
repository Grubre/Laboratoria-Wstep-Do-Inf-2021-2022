import random
import networkx as nx
import numpy as np
import matplotlib.pyplot as plt

def construct_N_matrix(intensity):
    N = [[0 if i == j else intensity for i in range(0, 20)] for j in range(0, 20)]
    return N

def construct_handcrafted_graph(CAPACITY):
    G = nx.null_graph()
    G.add_edge(0, 1, a = 0, c = CAPACITY)
    G.add_edge(0, 19, a = 0, c = CAPACITY)
    G.add_edge(1, 2, a = 0, c = CAPACITY)
    G.add_edge(2, 3, a = 0, c = CAPACITY)
    G.add_edge(3, 4, a = 0, c = CAPACITY)
    G.add_edge(3, 17, a = 0, c = CAPACITY)
    G.add_edge(4, 5, a = 0, c = CAPACITY)
    G.add_edge(4, 19, a = 0, c = CAPACITY)
    G.add_edge(5, 6, a = 0, c = CAPACITY)
    G.add_edge(5, 14, a = 0, c = CAPACITY)
    G.add_edge(6, 7, a = 0, c = CAPACITY)
    G.add_edge(7, 8, a = 0, c = CAPACITY)
    G.add_edge(7, 12, a = 0, c = CAPACITY)
    G.add_edge(8, 9, a = 0, c = CAPACITY)
    G.add_edge(9, 10, a = 0, c = CAPACITY)
    G.add_edge(9, 12, a = 0, c = CAPACITY)
    G.add_edge(10, 11, a = 0, c = CAPACITY)
    G.add_edge(11, 12, a = 0, c = CAPACITY)
    G.add_edge(12, 13, a = 0, c = CAPACITY)
    G.add_edge(12, 19, a = 0, c = CAPACITY)
    G.add_edge(14, 15, a = 0, c = CAPACITY)
    G.add_edge(15, 16, a = 0, c = CAPACITY)
    G.add_edge(15, 19, a = 0, c = CAPACITY)
    G.add_edge(16, 17, a = 0, c = CAPACITY)
    G.add_edge(16, 18, a = 0, c = CAPACITY)
    G.add_edge(17, 18, a = 0, c = CAPACITY)
    return G


def reset_flow_func(G: nx.Graph):
    for i in G.edges:
        edge = G[i[0]][i[1]]
        edge["a"] = 0

def simulate_flow(N, G: nx.Graph, packet_size):
    reset_flow_func(G)
    for j in range(len(N)):
        for i in range(len(N[j])):
            print("Going from " + str(i) + " to " + str(j))
            path = nx.shortest_path(G,i,j)
            if len(path) > 1:
                print(path)
                for e in range(len(path) - 1):
                    G[path[e]][path[e + 1]]["a"] += N[path[e]][path[e+1]] * packet_size

def draw(graph: nx.Graph):
    pos = nx.spring_layout(graph, iterations=1000, seed=10)
    nx.draw_networkx(graph, pos, node_color='#ab4b44')
    edge_labels = nx.get_edge_attributes(graph,'a')
    nx.draw_networkx_edge_labels(graph, pos, edge_labels = edge_labels, font_size=6)
    nx.draw_networkx_labels(graph, pos)

def experiment(G: nx.Graph, probability: float) -> bool:
    G = G.copy()
    # First remove edges
    for (u,v) in G.edges:
        p = random.random()
        if p > probability:
            print("Removing u, v")
            G.remove_edge(u,v)

    if not nx.is_connected(G):
        return False
    return True

# parameters
MAX_PROPAGATION = 1#s
CAPACITY = 1_000_000_000 #Gb/s
AVG_BITS_PER_PACKET = 10_000
INTENSITY = 1500
PROBABILITY = 0.995

N = construct_N_matrix(1500)
G = sum(sum(N,[]))

graph = construct_handcrafted_graph(CAPACITY)
for i in range(100):
    print(experiment(graph, PROBABILITY))
# print(N)
# reset_flow_func(graph)
# simulate_flow(N, graph, AVG_BITS_PER_PACKET)
# draw(graph)
#
# plt.savefig('graph.png', transparent = True)
# plt.show()
