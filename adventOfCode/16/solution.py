import re
import sys


def main():
    samples, part_two = digest_input()
    # Part One
    greater_than_three = [s for s in samples if len(filter_ops(s)) >= 3]
    print(len(greater_than_three))


def digest_input():
    if TESTING:
        filename = 'test.txt'
    else:
        filename = 'input.txt'
    with open(filename, 'r') as inputfile:
        text = inputfile.read()
    samples_text, part_two = text.split('\n\n\n\n')
    sample_pattern = (
        'Before: \[([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+)\]\n'
        '([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)\n'
        'After:  \[([0-9]+), ([0-9]+), ([0-9]+), ([0-9]+)\]'
    )
    matches = re.findall(sample_pattern, samples_text)
    samples = [
        {
            'before': [int(g) for g in match[:4]],
            'op': [int(g) for g in match[4:8]],
            'after': [int(g) for g in match[8:]]
        }
        for match in matches
    ]
    return samples, part_two


def filter_ops(sample):
    before = sample['before']
    after = sample['after']
    a, b, c = sample['op'][1:]
    valid_ops = [op for op in Ops._all() if op(a, b, c, before) == after]
    return valid_ops


class Ops:

    @classmethod
    def _all(cls):
        return [getattr(cls, a) for a in dir(cls) if not a.startswith('_')]

    @staticmethod
    def addr(a, b, c, before):
        value = before[a] + before[b]
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def addi(a, b, c, before):
        value = before[a] + b
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def mulr(a, b, c, before):
        value = before[a] * before[b]
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def muli(a, b, c, before):
        value = before[a] * b
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def banr(a, b, c, before):
        value = before[a] & before[b]
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def bani(a, b, c, before):
        value = before[a] & b
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def borr(a, b, c, before):
        value = before[a] | before[b]
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def bori(a, b, c, before):
        value = before[a] | b
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def setr(a, b, c, before):
        value = before[a]
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def seti(a, b, c, before):
        value = a
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def gtir(a, b, c, before):
        value = int(a > before[b])
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def gtri(a, b, c, before):
        value = int(before[a] > b)
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def gtrr(a, b, c, before):
        value = int(before[a] > before[b])
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def eqir(a, b, c, before):
        value = int(a == before[b])
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def eqri(a, b, c, before):
        value = int(before[a] == b)
        return before[:c] + [value] + before[c+1:]

    @staticmethod
    def eqrr(a, b, c, before):
        value = int(before[a] == before[b])
        return before[:c] + [value] + before[c+1:]


all_ops = Ops._all()
assert len(all_ops) == 16

after = Ops.eqrr(2, 1, 3, [5, 3, 5, 8])
assert after == [5, 3, 5, 0], after

after = Ops.eqrr(2, 0, 3, [5, 3, 5, 8])
assert after == [5, 3, 5, 1], after

after = Ops.gtir(2, 0, 3, [5, 3, 5, 8])
assert after == [5, 3, 5, 0], after


TESTING = 'test' in sys.argv

if __name__ == '__main__':
    main()
