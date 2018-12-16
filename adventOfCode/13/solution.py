from enum import Enum
from pprint import pprint
import sys


def main():
    track_map, carts = digest_input()
    while len(carts) > 1:
        if TESTING:
            show(track_map, carts)
            print('*'*20)
        for cart in sorted(carts, key=lambda c: c.array_index):
            cart.move(track_map)
            if not cart.crashed and cart.check_crash(carts):
                print(f'Crash @ (y, x) = {cart.array_index}')
        carts = list(filter(lambda cart: not cart.crashed, carts))
    last_cart = next(iter(carts), None)
    location = last_cart.array_index if last_cart else None
    print(f'Last cart @ (y, x) = {location}')


def digest_input():
    filename = 'test.txt' if TESTING else 'input.txt'
    with open(filename, 'r') as inputfile:
        lines = [line.replace('\n', '') for line in inputfile.readlines()]
    carts = []
    track_map = []
    for y, row in enumerate(lines):
        track_row = []
        track_map.append(track_row)
        for x, value in enumerate(row):
            if value in ('<', '^', 'v', '>'):
                direction, cart = Cart.read_input(value, x, y)
                carts.append(cart)
            else:
                direction = next(filter(lambda d: d.value == value, Direction), None)
            track_row.append(direction)
    return track_map, carts


def show(track_map, carts):
    printable = [[piece.value for piece in row] for row in track_map]
    for cart in carts:
        y, x = cart.array_index
        arrow = {(0, 1): '>', (1, 0): 'v', (0, -1): '<', (-1, 0): '^'}[cart.delta]
        printable[y][x] = arrow
    print('\n'.join(''.join(row) for row in printable))


class Direction(Enum):
    hor = '-'
    ver = '|'
    left = '\\'
    right = '/'
    inter = '+'
    none = ' '


class Cart(object):

    def __init__(self, arrow, x, y):
        self.delta = {'<': (0, -1), '^': (-1, 0), '>': (0, 1), 'v': (1, 0)}[arrow]
        self.array_index = (y, x)
        self.crashed = False
        self.turn = self._turn()
        if TESTING:
            print(f'{arrow} {self.delta} {self.array_index}')

    @classmethod
    def read_input(cls, value, x, y):
        direction = Direction.hor if value in ('<', '>') else Direction.ver
        cart = Cart(value, x, y)
        return direction, cart

    def _turn(self):
        turns = [
            lambda y, x: (-x, y),
            lambda y, x: (y, x),
            lambda y, x: (x, -y)
        ]
        index = 0
        while True:
            yield turns[index]
            index = (index + 1) % 3

    def move(self, track_map):
        if not self.crashed:
            self.array_index = tuple(map(sum, zip(self.delta, self.array_index)))
            y, x = self.array_index
            self.delta = self.determine_delta(track_map[y][x])

    def determine_delta(self, track_piece):
        if track_piece in (Direction.hor, Direction.ver):
            return self.delta
        elif track_piece == Direction.inter:
            return next(self.turn)(*self.delta)
        else:
            turns = {
                Direction.left: lambda y, x: (-x, y) if self.delta[0] else (x, -y),
                Direction.right: lambda y, x: (x, -y) if self.delta[0] else (-x, y)
            }
            return turns[track_piece](*self.delta)

    def check_crash(self, carts):
        for cart in carts:
            if cart.array_index == self.array_index and cart is not self:
                self.crashed = True
                cart.crashed = True
        return self.crashed


TESTING = 'test' in sys.argv

if __name__ == '__main__':
    main()
