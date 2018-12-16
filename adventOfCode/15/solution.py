from enum import Enum
import pdb
import sys


def main():
    input_lines = digest_input()
    cave = Cave(input_lines)
    if TESTING:
        cave.show()
    while not cave.is_over():
        cave.round()
        if TESTING:
            cave.show()
    print(cave.outcome())


def digest_input():
    if TESTING:
        filename = 'test.txt'
    else:
        filename = 'input.txt'
    with open(filename, 'r') as inputfile:
        input_lines = inputfile.readlines()
    return [line.replace('\n', '') for line in input_lines]


class Feature(Enum):
    wall = '#'
    open_ = '.'


class Combatant(object):

    enemy = None

    def __init__(self, location, cave):
        self.power = 3
        self.hp = 200
        self.loc = location
        self.cave = cave

    def take_turn(self):
        path = self.closest_path_to_target()
        if path is None:
            return
        if len(path.steps) > 2:
            self.move(path.steps[1])
        target = self.select_target()
        if target is not None:
            self.attack(target)

    def closest_path_to_target(self):
        paths = [Path(self.loc)]
        explored = {}
        target_paths = []
        while paths and not target_paths:
            candidate_paths = []
            for path in paths:
                candidate_paths.extend(path.extend())
            new_paths = []
            for candidate_path in candidate_paths:
                if candidate_path.steps[-1] in explored:
                    if candidate_path < explored[candidate_path.steps[-1]]:
                        new_paths.remove(explored[candidate_path.steps[-1]])
                        explored[candidate_path.steps[-1]] = candidate_path
                        new_paths.append(candidate_path)
                else:
                    explored[candidate_path.steps[-1]] = candidate_path
                    new_paths.append(candidate_path)
            unblocked_paths = []
            for new_path in new_paths:
                occupant = self.cave.occupant(new_path.steps[-1])
                if occupant is Feature.open_:
                    unblocked_paths.append(new_path)
                elif type(occupant).__name__ == self.enemy:
                    target_paths.append(new_path)
            paths = unblocked_paths
        return min(target_paths, key=lambda p: p.steps[-1]) if target_paths else None

    def move(self, new_location):
        self.cave.vacate(self.loc)
        self.cave.occupy(new_location, self)
        self.loc = new_location

    def select_target(self):
        y, x = self.loc
        targets = [
            self.cave.occupant(loc)
            for loc in ((y-1, x), (y+1, x), (y, x-1), (y, x+1))
            if type(self.cave.occupant(loc)).__name__ == self.enemy
        ]
        return min(targets, key=lambda t: (t.hp, t.loc)) if targets else None

    def attack(self, target):
        target.take_damage(self.power)

    def take_damage(self, damage):
        self.hp = max(self.hp - damage, 0)
        if self.hp == 0:
            self.cave.vacate(self.loc)
            self.loc = (None, None)


class Elf(Combatant):

    enemy = 'Goblin'


class Goblin(Combatant):

    enemy = 'Elf'


class Path(object):

    def __init__(self, location, history=None):
        self.steps = (history or []) + [location]

    def __lt__(self, p):
        if len(self.steps) == len(p.steps):
            return self.steps < p.steps
        else:
            return len(self.steps) < len(p.steps)

    def __eq__(self, p):
        return self.steps == p.steps

    def __gt__(self, p):
        if len(self.steps) == len(p.steps):
            return self.steps > p.steps
        else:
            return len(self.steps) > len(p.steps)

    def __repr__(self):
        return f'P<{"-".join(str(step) for step in self.steps)}>'

    def extend(self):
        y, x = self.steps[-1]
        extensions = [(y+1, x), (y-1, x), (y, x+1), (y, x-1)]
        return [Path(extension, self.steps) for extension in extensions]

class Cave(object):

    def __init__(self, input_lines):
        self.map_ = []
        goblins, elves = [], []
        for y, line in enumerate(input_lines):
            new_row = []
            self.map_.append(new_row)
            for x, mark in enumerate(line):
                if mark == '#':
                    new_row.append(Feature.wall)
                elif mark == '.':
                    new_row.append(Feature.open_)
                elif mark == 'E':
                    elves.append(Elf((y, x), self))
                    new_row.append(elves[-1])
                elif mark == 'G':
                    goblins.append(Goblin((y, x), self))
                    new_row.append(goblins[-1])
                else:
                    raise ValueError(f'Did not recognize map marking <{mark}>.')
        self.goblins = goblins
        self.elves = elves
        self.rounds = 0

    def show(self):
        for row in self.map_:
            print(''.join(map(self.space_string, row)))

    def space_string(self, space):
        if space in Feature:
            return space.value
        elif type(space) == Elf:
            return 'E'
        elif type(space) == Goblin:
            return 'G'
        else:
            raise ValueError(f'Dunno how to represent {space}.')

    def vacate(self, loc):
        self.map_[loc[0]][loc[1]] = Feature.open_

    def occupy(self, loc, combatant):
        if self.occupant(loc) is not Feature.open_:
            raise TypeError(f'Combatant {combatant} is trying to move to {loc}, but it is not open: {occupant}')
        self.map_[loc[0]][loc[1]] = combatant

    def occupant(self, loc):
        return self.map_[loc[0]][loc[1]]

    def round(self):
        combatants = sorted(self.goblins + self.elves, key=lambda c: c.loc)
        for combatant in combatants:
            if combatant.hp > 0:
                combatant.take_turn()
        self.rounds += 1
        self.cleanup()

    def cleanup(self):
        self.elves = [elf for elf in self.elves if elf.hp > 0]
        self.goblins = [goblin for goblin in self.goblins if goblin.hp > 0]

    def is_over(self):
        return not any(g.hp for g in self.goblins) or not any(e.hp for e in self.elves)

    def outcome(self):
        hp = sum(g.hp for g in self.goblins) + sum(e.hp for e in self.elves)
        return hp * self.rounds


TESTING = 'test' in sys.argv

if __name__ == '__main__':
    main()
