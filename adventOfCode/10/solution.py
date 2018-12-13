import numpy as np
import re
import sys


def main():
    np.set_printoptions(threshold=3000, linewidth=3000)
    sky = make_sky()
    sky.minimize()
    sky.smallshow()
    print(sky.time)


def make_sky():
    return Sky(get_points())


def get_points():
    filename = 'input.txt' if not TESTING else 'test.txt'
    with open(filename, 'r') as inputfile:
        inputlines = inputfile.readlines()
    points = [Point.from_string(line) for line in inputlines]
    return points


class Sky(object):

    def __init__(self, points):
        self.points = np.array([[point.x, point.y] for point in points])
        self.velocities = np.array([
            [point.velocity[0], point.velocity[1]]
            for point in points
        ])
        self.time = 0

    @property
    def shape(self):
        return self.get_shape()

    def minimize(self):
        shx, shy = self.get_shape()
        self.increment()
        new_shx, new_shy = self.get_shape()
        while new_shy < shy:
            shx, shy = new_shx, new_shy
            self.increment()
            new_shx, new_shy = self.get_shape()
        self.decrement()

    def increment(self, n=1):
        self.points += n * self.velocities
        self.time += n

    def decrement(self, n=1):
        self.points -= n * self.velocities
        self.time -= n

    def get_shape(self):
        xs, ys = self.points.transpose()
        x_min, x_max = xs.min(), xs.max()
        y_min, y_max = ys.min(), ys.max()
        shape = (x_max - x_min + 1, y_max - y_min + 1)
        return shape

    def show(self):
        '''Efficient but hard to read'''
        xs, ys = self.points.transpose()
        x_min, x_max = xs.min(), xs.max()
        y_min, y_max = ys.min(), ys.max()
        shape = (x_max - x_min + 1, y_max - y_min + 1)
        field = np.zeros(shape, int)
        xcoords = xs - x_min
        ycoords = ys - y_min
        field[xcoords, ycoords] = 7
        print(field)

    def smallshow(self):
        '''Too inefficient when stars are very spread out'''
        xs, ys = self.points.transpose()
        x_min, x_max = xs.min(), xs.max()
        y_min, y_max = ys.min(), ys.max()
        shape = (x_max - x_min + 1, y_max - y_min + 1)
        field = [['.' for _ in range(shape[1])] for _ in range(shape[0])]
        for p in self.points:
            field[p[0] - x_min][p[1] - y_min] = '#'
        for row in field:
            print(''.join(row))


class Point(object):

    def __init__(self, position, velocity):
        self.position = position
        self.velocity = velocity

    @staticmethod
    def from_string(inputline):
        py, px, vy, vx = [int(s) for s in re.findall(r'-?[0-9]+', inputline)]
        return Point((px, py), (vx, vy))

    def move(self):
        self.position = tuple(sum(z) for z in zip(self.position, self.velocity))

    @property
    def x(self):
        return self.position[0]

    @property
    def y(self):
        return self.position[1]


if __name__ == '__main__':
    TESTING = 'test' in sys.argv
    main()
