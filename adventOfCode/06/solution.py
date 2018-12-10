from functools import reduce
from sys import argv

if "test" not in argv:
    with open('input.txt', 'r') as inputfile:
        coords = [p.replace('\n', '') for p in inputfile.readlines() if p]
else:
    coords = [
        "1, 1",
        "1, 6",
        "8, 3",
        "3, 4",
        "5, 5",
        "8, 9",
    ]


class Point(object):

    def __init__(self, x, y):
        self.x = x
        self.y = y

    def __hash__(self):
        return (self.x, self.y).__hash__()

    def __repr__(self):
        return f"P({self.x}, {self.y})"

    @classmethod
    def from_string(cls, coord_string):
        x, y = [int(c) for c in coord_string.split(', ')]
        return cls(x, y)

    def distance(self, *args):
        if len(args) == 1:
            x, y = args[0].x, args[0].y
            return self.distance(x, y)
        elif len(args) == 2:
            x, y = args
            return abs(x - self.x) + abs(y - self.y)
        else:
            raise TypeError(f"{self.__cls__}.distance takes a Point or x, y")

    def closest_center(self, centers):
        closest_center = centers[0]
        shortest_distance = self.distance(centers[0])
        for center in centers[1:]:
            center_distance = self.distance(center)
            if center_distance < shortest_distance:
                shortest_distance = center_distance
                closest_center = center
            elif center_distance == shortest_distance:
                closest_center = None
        return closest_center


class Center(Point):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.area = []


centers = [Center.from_string(coord) for coord in coords]
min_x = min(center.x for center in centers)
max_x = max(center.x for center in centers)
min_y = min(center.y for center in centers)
max_y = max(center.y for center in centers)

tapestry = reduce(lambda x, y: x + y, [
    [
        Point(x, y)
        for x in range(min_x, max_x + 1)
    ]
    for y in range(min_y, max_y + 1)
])


# Part One


infinite_areas = set([])
for point in tapestry:
    closest_center = point.closest_center(centers)
    if closest_center is not None:
        closest_center.area.append(point)
        if point.x in (min_x, max_x) or point.y in (max_y, max_y):
            infinite_areas.add(closest_center)


def not_infinite(center):
    return center not in infinite_areas


biggest_center = max(filter(not_infinite, centers), key=lambda c: len(c.area))
print(len(biggest_center.area))


# Part Two


def close_enough(point):
    return sum(map(point.distance, centers)) < 10000


print(len(list(filter(close_enough, tapestry))))
