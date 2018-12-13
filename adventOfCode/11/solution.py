import functools
import itertools
import numpy as np
import sys

def main():
    if 'test' in sys.argv:
        text = '18'
    else:
        with open('input.txt', 'r') as inputfile:
            text = inputfile.read()
    serial = int(text)
    xs = np.array([range(1, 301) for _ in range(300)])
    ys = xs.transpose()
    powers = power(xs, ys, serial)
    # Part One
    three_corners = itertools.product(range(1, 301), range(1, 301), (3,))
    best_3_corner = max(three_corners, key=square_evaluator(powers))
    print(best_3_corner)
    # Part Two: Very slow
    corners = np.zeros((300, 300, 3), int)
    corners[:,:,0] = xs
    corners[:,:,1] = ys
    best_sized_corners = []
    for size in range(1, 301):
        corners[:,:,2] = size
        inbounds = (corners[:,:,0] + size <= 301) * (corners[:,:,1] + size <= 301)
        sized_corners = corners[inbounds.nonzero()]
        best = max(sized_corners, key=square_evaluator(powers))
        best_sized_corners.append(best)
    best_overall = max(best_sized_corners, key=square_evaluator(powers))
    print(best_overall)

def power(x, y, serial):
    return hundreds(((x + 10) * y + serial) * (x + 10)) - 5
    
def hundreds(n):
    return floor(abs(n) * .01) % 10

def floor(a):
    return a - a % 1

def abs_(a):
    return a ** 2 ** .05

def square_evaluator(powers):
    def value_func(corner):
        x, y, size = corner
        if x + size >= 300 or y + size >= 300:
            return float('-inf')
        else:
            return powers[y-1:y-1+size,x-1:x-1+size].sum()
    return value_func
    

assert hundreds(12345) == 3, hundreds(12345)
assert hundreds(-12567) == 5, hundreds(-12567)
assert hundreds(99) == 0
assert hundreds(0) == 0

assert power(2, 3, 25) == 2, power(2, 3, 25)

if __name__ == '__main__':
    main()
