import numpy as np
import sys


def main():
    map_ = np.array(digest_input())
    for _ in range(10):
        map_ = iterate(map_)
    wooded = (map_ == 1).nonzero()[0].size
    yards = (map_ == 2).nonzero()[0].size
    print(wooded * yards)


def digest_input():
    if TESTING:
        filename = 'test.txt'
    else:
        filename = 'input.txt'
    with open(filename, 'r') as inputfile:
        lines = [line.replace('\n', '') for line in inputfile.readlines()]
    return [list(map(['.', '|', '#'].index, line)) for line in lines]


def iterate(map_):
    new_map = np.zeros(map_.shape, int)
    for y, row in enumerate(map_):
        for x, acre in enumerate(row):
            new_acre = advance_acre(y, x, map_)
            new_map[y, x] = new_acre
    return np.array(new_map)


def advance_acre(y, x, map_):
    acre_type = map_[y, x]
    around = acres_around(y, x, map_)
    fields_around = (around == 0).nonzero()[0].size
    trees_around = (around == 1).nonzero()[0].size
    yards_around = (around == 2).nonzero()[0].size
    if acre_type == 0:
        if trees_around >= 3:
            return 1
        else:
            return 0
    elif acre_type == 1:
        if yards_around >= 3:
            return 2
        else:
            return 1
    else:
        if yards_around >= 1 and trees_around >= 1:
            return 2
        else:
            return 0


def acres_around(y, x, map_):
    ylen, xlen = map_.shape
    all_acres = [
        (y-1, x-1), (y-1, x), (y-1, x+1),
        (y,   x-1),           (y,   x+1),
        (y+1, x-1), (y+1, x), (y+1, x+1)
    ]
    inbounds = [
        (y, x)
        for y, x in all_acres
        if 0 <= x < xlen and 0 <= y < ylen
    ]
    return map_[tuple(zip(*inbounds))]


TESTING = 'test' in sys.argv

if __name__ == '__main__':
    main()
