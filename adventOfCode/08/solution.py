from functools import partial, reduce
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
        partial(metadata_sum_folder, tree_values=tree_values),
        range(children_count),
        (2, 0)
    )
    node_end = position + metadata_count
    metadata = tree_values[position:node_end]
    total += sum(metadata)
    return node_end, total


def metadata_sum_folder(a, b, tree_values):
    position, total = a
    length, value = metadata_sum(tree_values[position:])
    return position + length, total + value


# Part Two


def node_value(tree_values):
    children_count, metadata_count = tree_values[:2]
    position, children_values = reduce(
        partial(node_value_folder, tree_values=tree_values),
        range(children_count),
        (2, [])
    )
    node_end = position + metadata_count
    metadata = tree_values[position:node_end]
    if children_values:
        total = sum(
            metadata.count(i + 1) * value
            for i, value in enumerate(children_values)
        )
    else:
        total = sum(metadata)
    return node_end, total


def node_value_folder(a, b, tree_values):
    position, children_values = a
    length, value = node_value(tree_values[position:])
    return position + length, children_values + [value]


if __name__ == '__main__':
    main()
