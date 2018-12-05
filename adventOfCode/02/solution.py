from collections import Counter
from functools import reduce
from operator import mul

with open("input.txt", "r") as inputfile:
    lines = [line.replace("\n", "") for line in inputfile.readlines()]


# Part One


def categorize(line):
    counter = Counter(line)
    return (2 in counter.values(), 3 in counter.values())


categories = [categorize(line) for line in lines]

category_counts = [sum(x) for x in zip(*categories)]

checksum = mul(*category_counts)

print(checksum)


# Part Two


def matching_characters(base, comparison):
    return "".join(c[0] for c in zip(base, comparison) if c[0] == c[1])


def samesies(base, comparison):
    on_count = len(matching_characters(base, comparison))
    off_count = len(base) - on_count
    return off_count == 1


for i, base_line in enumerate(lines):
    for comparison_line in lines[i+1:]:
        if samesies(base_line, comparison_line):
            print(matching_characters(base_line, comparison_line))
            break
