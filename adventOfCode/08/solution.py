from functools import reduce
import re


def main():
    with open('input.txt', 'r') as inputfile:
        text = inputfile.read()
    tree_values = [int(v) for v in re.findall(r'[0-9]+', text)]
    length, total = metadata_sum(tree_values)
    print(total)
    length, value = node_value(tree_values)
    print(value)


# Part One


def metadata_sum(tree_values):
    children_count, metadata_count = tree_values[:2]
    position, total = reduce(
        lambda a, b: [sum(z) for z in zip(a, metadata_sum(tree_values[a[0]:]))],
        range(children_count),
        (2, 0)
    )
    node_end = position + metadata_count
    metadata = tree_values[position:node_end]
    total += sum(metadata)
    return node_end, total


# Part Two


def node_value(tree_values):
    children_count, metadata_count = tree_values[:2]
    position = 2
    children_values = []
    for index in range(children_count):
        length, value = node_value(tree_values[position:])
        position += length
        children_values.append(value)
    node_end = position + metadata_count
    metadata = tree_values[position:node_end]
    if children_values:
        total = sum(metadata.count(i + 1) * value for i, value in enumerate(children_values))
    else:
        total = sum(metadata)
    return node_end, total


if __name__ == '__main__':
    main()
