import numpy as np
import re


with open("input.txt", "r") as inputfile:
    lines = [line.replace("\n", "") for line in inputfile.readlines()]


def parse_line(line):
    pattern = r"#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"
    match = re.match(pattern, line)
    if not match:
        raise Exception(f"No pattern match: {line}")
    groups = match.groups()
    return dict(zip(
        ["id", "x", "y", "width", "height"],
        map(int, groups)
    ))


parsed_lines = list(map(parse_line, lines))
shape = (
    max(line["y"] + line["height"] + 1 for line in parsed_lines),
    max(line["x"] + line["width"] + 1 for line in parsed_lines)
)
fabric = np.zeros(shape, int)


# Part One


for line in parsed_lines:
    fabric[
        line["y"]:line["y"] + line["height"],
        line["x"]:line["x"] + line["width"]
    ] += 1

print(len(np.transpose((fabric > 1).nonzero())))


# Part Two


for line in parsed_lines:
    rect = fabric[
        line["y"]:line["y"] + line["height"],
        line["x"]:line["x"] + line["width"]
    ]
    if len(np.transpose((rect > 1).nonzero())) == 0:
        print(line["id"])
