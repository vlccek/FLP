import sys
import plotly.graph_objects as go

# Třída představující uzel stromu
class TreeNode:
    def __init__(self, label):
        self.label = label
        self.children = []

    def __repr__(self):
        return f"TreeNode({self.label})"

# Vrátí počet počátečních mezer (předpokládáme 2 mezery na úroveň)
def indent_level(line):
    return (len(line) - len(line.lstrip(' '))) // 2

# Parsuje lineární seznam řádků (s indentací) a sestavuje strom pomocí zásobníku.
def parse_tree(lines):
    stack = []
    root = None
    for line in lines:
        stripped = line.lstrip(' ')
        level = indent_level(line)
        node = TreeNode(stripped)
        if not stack:
            root = node
            stack.append((node, level))
        else:
            # Odstraňuj uzly, které nejsou rodiči aktuálního řádku.
            while stack and stack[-1][1] >= level:
                stack.pop()
            if stack:
                parent = stack[-1][0]
                parent.children.append(node)
            else:
                root = node
            stack.append((node, level))
    return root

# Přiřadí každému uzlu souřadnice pomocí DFS – x je pořadí a y odpovídá hloubce (negativně pro lepší vizualizaci shora dolů)
def assign_positions(root):
    positions = {}
    x_counter = 0

    def dfs(node, depth):
        nonlocal x_counter
        positions[node] = (x_counter, -depth)
        x_counter += 1
        for child in node.children:
            dfs(child, depth + 1)
    dfs(root, 0)
    return positions

# Vytvoří seznam hran pro vykreslení stromu (spáruje rodiče a potomky)
def get_edges(root, positions):
    edges = []
    def dfs(node):
        for child in node.children:
            edges.append((positions[node], positions[child]))
            dfs(child)
    dfs(root)
    return edges

def main():
    if len(sys.argv) < 2:
        print("Použití: python script.py <vstupni_soubor>")
        sys.exit(1)

    input_file = sys.argv[1]
    with open(input_file, 'r', encoding='utf-8') as f:
        content = f.read()

    # Rozdělíme obsah souboru na řádky a odstraníme prázdné řádky
    lines = [line for line in content.strip().split('\n') if line.strip()]

    # Sestav strom ze řádků
    tree_root = parse_tree(lines)
    positions = assign_positions(tree_root)
    edges = get_edges(tree_root, positions)

    # Připrav data pro vykreslení uzlů
    node_x, node_y, node_labels = [], [], []
    for node, (x, y) in positions.items():
        node_x.append(x)
        node_y.append(y)
        node_labels.append(node.label)

    # Připrav data pro vykreslení hran
    edge_x, edge_y = [], []
    for (x0, y0), (x1, y1) in edges:
        edge_x += [x0, x1, None]
        edge_y += [y0, y1, None]

    # Trace pro hrany (čáry mezi uzly)
    edge_trace = go.Scatter(
        x=edge_x, y=edge_y,
        mode='lines',
        line=dict(width=1, color='#888'),
        hoverinfo='none'
    )

    # Trace pro uzly (body s textem)
    node_trace = go.Scatter(
        x=node_x, y=node_y,
        mode='markers+text',
        text=node_labels,
        textposition="bottom center",
        marker=dict(
            size=20,
            color='skyblue',
            line_width=2
        )
    )

    fig = go.Figure(data=[edge_trace, node_trace],
                    layout=go.Layout(
                        title='Vizualizace stromu',
                        showlegend=False,
                        hovermode='closest',
                        margin=dict(b=20, l=5, r=5, t=40),
                        xaxis=dict(showgrid=False, zeroline=False, showticklabels=False),
                        yaxis=dict(showgrid=False, zeroline=False, showticklabels=False)
                    ))

    fig.show()

if __name__ == '__main__':
    main()
    