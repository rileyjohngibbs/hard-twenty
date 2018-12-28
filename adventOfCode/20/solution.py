from functools import reduce
import sys


def main():
    route_string = digest_input()
    fac = Facility()
    fac.add_route(route_string)
    if TESTING:
        fac.show()
    fac.set_distances()
    # Part One
    print(fac.max_distance())
    # Part Two
    print(len([room for room in fac.all_rooms if room.distance >= 1000]))


def digest_input():
    if TESTING:
        filename = 'test2.txt'
    else:
        filename = 'input.txt'
    with open(filename, 'r') as foo:
        text = foo.read()
    return text


class Facility(dict):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.setdefault(0, {})[0] = Room(0, 0)

    def add_route(self, route_string, start=None):
        current_room = start or self[0][0]
        if TESTING:
            print(f'From {current_room.x}, {current_room.y}:')
        index = 0
        while index < len(route_string):
            direction = route_string[index]
            if direction == '^':
                index += 1
            elif direction == '(':
                current_room, delta = self.add_route(route_string[index + 1:], current_room)
                index += delta + 1
            elif direction in ('$', ')'):
                index += 1
                break
            elif direction == '|':
                current_room, delta = self.add_route(route_string[index + 1:], start)
                index += delta
            else:
                if TESTING:
                    print(f'Go {direction}')
                current_room = self.use_door(current_room, direction)
                index += 1
        return current_room, index

    @staticmethod
    def _new_coords(room, direction):
        x, y = room.x, room.y
        new_x, new_y = {
            'N': (x, y - 1),
            'E': (x + 1, y),
            'S': (x, y + 1),
            'W': (x - 1, y)
        }[direction]
        return new_x, new_y
        

    def add_door(self, from_room, direction):
        to_x, to_y = self._new_coords(from_room, direction)
        from_room.set_exit(direction)
        to_room = self.room(to_x, to_y)
        to_room.set_entrance(direction)

    def room(self, x, y):
        return self.setdefault(x, {}).setdefault(y, Room(x, y))

    def show(self):
        x_vals = self.keys()
        min_x, max_x = min_max(x_vals)
        y_vals = reduce(lambda a, b: a + list(b.keys()), self.values(), [])
        min_y, max_y = min_max(y_vals)
        print('#'*(max_x - min_x + 1)*2 + '#')
        for y in range(min_y, max_y + 1):
            this_row = ['#']
            next_row = ['#']
            for x in range(min_x, max_x + 1):
                room = self.room(x, y)
                this_row.append('.' if not (x, y) == (0, 0) else 'X')
                next_row.append('-' if 'S' in room.doors else '#')
                this_row.append('|' if 'E' in room.doors else '#')
                next_row.append('#')
            print(*this_row, sep='')
            print(*next_row, sep='')

    def use_door(self, room, direction):
        self.add_door(room, direction)
        to_x, to_y = self._new_coords(room, direction)
        return self[to_x][to_y]

    def set_distances(self, start=None):
        _start = start or self[0][0]
        current_rooms = set([_start])
        _start.distance = 0
        while current_rooms:
            room = current_rooms.pop()
            distance = room.distance + 1
            new_rooms = [self.use_door(room, d) for d in room.doors]
            for new_room in new_rooms:
                if distance < new_room.distance:
                    new_room.distance = distance
                    current_rooms.add(new_room)

    def max_distance(self):
        return self.farthest_room().distance

    @property
    def all_rooms(self):
        return reduce(lambda a, b: a + list(b.values()), self.values(), [])

    def farthest_room(self):
        return max(self.all_rooms, key=lambda r: r.distance)


def min_max(iterable, start=(0, 0)):
    def reducer(m, x):
        return min(m[0], x), max(m[1], x)
    min_, max_ = reduce(reducer, iterable, start)
    return min_, max_


class Room(object):

    def __init__(self, x, y):
        self.x = x
        self.y = y
        self.distance = float('inf')
        self._doors = {k: False for k in ['N', 'E', 'S', 'W']}

    @property
    def doors(self):
        return [k for k, v in self._doors.items() if v]

    def _set_door(self, direction):
        self._doors[direction] = True

    def set_exit(self, direction):
        self._set_door(direction)

    def set_entrance(self, direction):
        _direction = {'N': 'S', 'E': 'W', 'S': 'N', 'W': 'E'}[direction]
        self._set_door(_direction)


TESTING = 'test' in sys.argv

if __name__ == '__main__':
    main()
