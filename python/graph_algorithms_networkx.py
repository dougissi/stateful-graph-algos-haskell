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
    prev_end = None
    for start, end in nx.dfs_edges(G, src):
        if prev_end and prev_end != start:
            raise ValueError(f"End of previous edge '{prev_end}' does not match start of next edge '{start}'")
        traversal.append(start)
        prev_end = end
    traversal.append(end)  # include end of last edge
    return str(traversal).replace(' ', '')


def get_formatted_shortest_paths(G: nx.Graph) -> str:
    shortest_paths = []
    for (src, paths_dict) in nx.all_pairs_shortest_path_length(G):
        paths = []
        for (dest, length) in paths_dict.items():
            if dest != src:
                paths.append((dest, length))
        shortest_paths.append((src, sorted(paths)))
    return str(shortest_paths).replace(' ', '')


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument('-e', '--edgesFile')
    args = parser.parse_args()

    G = buildGraph(args.edgesFile)

    traversal = get_formatted_traversal(G, src=4)
    print("traversal:", traversal)

    shortest_paths = get_formatted_shortest_paths(G)
    print("shortest paths:", shortest_paths)

    nx.draw(G, with_labels=True, font_weight='bold')
    plt.show()

    