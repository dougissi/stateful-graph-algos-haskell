import argparse
import networkx as nx
import matplotlib.pyplot as plt


def buildGraph(edges_file: str) -> nx.Graph:
    edges = []
    with open(edges_file) as f:
        for line in f:
            nodes = line.strip().split(' ')
            edge = (int(nodes[0]), int(nodes[1]))
            edges.append(edge)

    G = nx.Graph()
    G.add_edges_from(edges)
    return G


def get_formatted_traversal(G: nx.Graph, src: int) -> str:
    traversal = []
    seenFirstNode = False

    for start, end in nx.dfs_edges(G, src):
        if not seenFirstNode:
            traversal.append(start)
            seenFirstNode = True
        traversal.append(end)
    return str(traversal).replace(' ', '')


def get_formatted_shortest_path_lens(G: nx.Graph) -> str:
    shortest_path_lens = []
    for (src, paths_dict) in nx.all_pairs_shortest_path_length(G):
        lens = []
        for (dest, length) in paths_dict.items():
            if dest != src:
                lens.append((dest, length))
        shortest_path_lens.append((src, sorted(lens)))
    return str(sorted(shortest_path_lens)).replace(' ', '')


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-e', '--edgesFile')
    args = parser.parse_args()

    G = buildGraph(args.edgesFile)

    traversal = get_formatted_traversal(G, src=1)
    print(f"traversal:\n{traversal}")

    shortest_path_lens = get_formatted_shortest_path_lens(G)
    print(f"shortest path lengths:\n{shortest_path_lens}")

    nx.draw(G, with_labels=True, font_weight='bold')
    plt.show()
