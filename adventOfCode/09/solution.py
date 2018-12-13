import re
import sys


def main():
    if TESTING:
        text = '9 69'
        text = '10 1618'
        text = '13 7999'
    else:
        with open('input.txt', 'r') as inputfile:
            text = inputfile.read()
    player_count, marble_max = map(int, re.findall(r'[0-9]+', text))
    # Part One: Takes under a second
    game = Game(player_count, marble_max)
    game.play_game()
    print(game.high_score())
    # Part Two: Takes 20-30 seconds
    game = Game(player_count, marble_max * 100)
    game.play_game()
    print(game.high_score())


class Game(object):

    def __init__(self, player_count, marble_max):
        self.player_count = player_count
        self.marble_max = marble_max
        self.player_scores = [0] * self.player_count

    def play_game(self):
        circle = Circle()
        self.player_scores = [0] * self.player_count
        for value in range(1, self.marble_max + 1):
            if value % 23:
                circle.add_marble(value)
            else:
                removed = circle.remove_marble()
                self.player_scores[(value - 1) % self.player_count] += value + removed
            if TESTING and self.marble_max < 70:
                circle.show()

    def high_score(self):
        return max(self.player_scores)


class Circle(object):

    def __init__(self):
        self.current = Marble(0)

    def show(self):
        print(f'({self.current})', end='')
        p = self.current.clock
        while p is not self.current:
            print(f' {str(p).rjust(2)} ', end='')
            p = p.clock
        print('')

    def add_marble(self, value):
        counter = self.current.clock
        clock = self.current.clock.clock
        self.current = Marble(value, counter, clock)

    def remove_marble(self):
        target = self.current.step_counter(7)
        self.current = target.clock
        target.counter.clock, target.clock.counter = target.clock, target.counter
        value = target.value
        del target
        return value


class Marble(object):

    def __init__(self, value, counter=None, clock=None):
        self.value = value
        self.counter = counter or self
        self.counter.clock = self
        self.clock = clock or self
        self.clock.counter = self

    def __repr__(self):
        return str(self.value)

    def step_counter(self, n):
        if n == 0:
            return self
        else:
            return self.counter.step_counter(n-1)

    def step_clock(self, n):
        if n == 0:
            return self
        else:
            return self.clock.step_clock(n-1)


if __name__ == '__main__':
    TESTING = 'test' in sys.argv
    main()
