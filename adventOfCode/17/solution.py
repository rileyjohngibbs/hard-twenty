from functools import reduce
import numpy as np
import re
import sys


def main():
    clay_coords = digest_input()
    ground = Ground(clay_coords)
    while True:
        if TESTING:
            ground.show()
        try:
            ground.tick()
        except WaterFinished:
            water_count = ground.count_water()
            settled_count = ground.count_water(False)
            break
    print(water_count)
    print(settled_count)
    return ground


def digest_input():
    if TESTING:
        filename = 'test.txt'
    else:
        filename = 'input.txt'
    with open(filename, 'r') as inputfile:
        input_lines = [line.replace('\n', '') for line in inputfile.readlines() if line]
    clay_coords = tuple(map(coords_from_input_line, input_lines))
    return clay_coords


def coords_from_input_line(input_line):
    xs = re.search(r'x=([0-9]+)(?:\.\.([0-9]+))?', input_line).groups()
    min_x, max_x = int(xs[0]), int(xs[1] or xs[0])
    ys = re.search(r'y=([0-9]+)(?:\.\.([0-9]+))?', input_line).groups()
    min_y, max_y = int(ys[0]), int(ys[1] or ys[0])
    return ((min_y, max_y), (min_x, max_x))


class Ground(object):

    SPRING = (0, 500)
    SAND = 0
    CLAY = 1
    WATER = 3
    FALL = 4

    def __init__(self, clay_coords):
        (self.min_y, self.max_y), (self.min_x, self.max_x) = reduce(
            update_min_max_y_x, 
            clay_coords
        )
        shape = (self.max_y + 1, self.max_x - self.min_x + 3)
        self.map_ = np.zeros(shape, int)
        for (y1, y2), (x1, x2) in clay_coords:
            self.map_[y1:y2+1, x1-self.min_x+1:x2-self.min_x+2] = 1
        self.map_value(self.SPRING, 2)
        self.water_fronts = [self.SPRING]

    def show(self, depth=None):
        depth = (depth or self.max_y) + 1
        print("\n".join(
            "".join(map(self.show_loc, row))
            for row in self.map_[:depth]
        ))
        print('')

    def show_loc(self, loc_val):
        return {
            self.SAND: '.',
            self.CLAY: '#',
            2: '+',
            self.WATER: '~',
            self.FALL: '|'
        }[loc_val]

    def map_value(self, loc, value=None):
        y, x = loc
        if y > self.max_y:
            raise OffBottom((y, x))
        if value is not None:
            self.map_[y, x - self.min_x + 1] = value
        try:
            return self.map_[y, x - self.min_x + 1]
        except IndexError:
            return 0
    
    def up(self, loc, value=None):
        y = loc[0] - 1
        x = loc[1]
        return self.map_value((y, x), value)

    def down(self, loc, value=None):
        y = loc[0] + 1
        x = loc[1]
        return self.map_value((y, x), value)

    def left(self, loc, value=None):
        y = loc[0]
        x = loc[1] - 1
        return self.map_value((y, x), value)

    def right(self, loc, value=None):
        y = loc[0]
        x = loc[1] + 1
        return self.map_value((y, x), value)

    def here(self, loc, value=None):
        y, x = loc
        return self.map_value((y, x), value)

    def forward(self, n, depth=None):
        for _ in range(n):
            self.tick()
        if depth is not None:
            self.show(depth)

    def tick(self):
        if not self.water_fronts:
            raise WaterFinished()
        self.water_fronts = set(reduce(
            lambda a, b: a + b,
            map(self.advance_front, self.water_fronts)
        ))

    def advance_front(self, water_front):
        if self.here(water_front) == self.SAND:
            self.here(water_front, self.FALL)
        try:
            down = self.down(water_front)
        except OffBottom:
            return []
        if down in (self.SAND, self.FALL):
            self.down(water_front, self.FALL)
            return [(water_front[0] + 1, water_front[1])]
        left_front = water_front
        right_front = water_front
        while self.left(left_front) in (self.SAND, self.FALL) \
                and self.down(left_front) not in (self.SAND, self.FALL):
            self.left(left_front, self.FALL)
            left_front = (left_front[0], left_front[1] - 1)
        while self.right(right_front) in (self.SAND, self.FALL) \
                and self.down(right_front) not in (self.SAND, self.FALL):
            self.right(right_front, self.FALL)
            right_front = (right_front[0], right_front[1] + 1)
        left_trapped = self.left(left_front) == self.CLAY and self.down(left_front)
        right_trapped = self.right(right_front) == self.CLAY and self.down(right_front)
        if left_trapped and right_trapped:
            self.settle(left_front, right_front)
            return [(water_front[0] - 1, water_front[1])]
        if self.down(left_front) not in (self.SAND, self.FALL):
            left_front = None
        if self.down(right_front) not in (self.SAND, self.FALL):
            right_front = None
        lateral_fronts = list(filter(None, (left_front, right_front)))
        return lateral_fronts

    def settle(self, left_front, right_front):
        xs = range(left_front[1], right_front[1] + 1)
        row = (left_front[0], np.array(xs))
        self.map_value(row, self.WATER)

    def count_water(self, include_falls=True):
        field = self.map_[self.min_y:]
        if include_falls:
            return ((field == self.WATER) + (field == self.FALL)).nonzero()[0].size
        else:
            return (field == self.WATER).nonzero()[0].size


class OffBottom(Exception):
    pass


class WaterFinished(Exception):
    pass


def update_min_max_y_x(current, new):
    (min_y, max_y), (min_x, max_x) = current
    ys, xs = new
    return (
        (min(min_y, *ys), max(max_y, *ys)),
        (min(min_x, *xs), max(max_x, *xs))
    )


TESTING = 'test' in sys.argv

if __name__ == '__main__':
    ground = main()
