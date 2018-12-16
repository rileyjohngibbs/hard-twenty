import numpy as np
import re
import sys


NUM_OPS = 16


def main():
    samples, instructions = digest_input()
    # Part One
    greater_than_three = [s for s in samples if len(filter_ops(s)) >= 3]
    print(len(greater_than_three))
    # Part Two
    logic = np.zeros((NUM_OPS, NUM_OPS))
    all_ops = Ops._all()
    for i, sample in enumerate(samples):
        code = sample['op'][0]
        ops = filter_ops(sample)
        for j, op in enumerate(all_ops):
            if op not in ops:
                logic[code,j] = -1
    count = 0
    while not solved(logic):
        for v in range(16):
            for vector in (logic[v], logic[:,v]):
                if has_answer(vector):
                    eliminate(vector)
                elif answer_forced(vector):
                    mark_answer(vector)
        count += 1
        if count % 10 == 0:
            print(f'{count} and stil solving...')
    opcodes = [np.transpose((logic[i] == 1).nonzero())[0,0] for i in range(16)]
    registers = [0, 0, 0, 0]
    for instruction in instructions:
        operation = all_ops[opcodes[instruction[0]]]
        registers = operation(*instruction[1:], registers)
    print(registers[0])


def digest_input():
    if TESTING:
        filename = 'test.txt'
    else:
        filename = 'input.txt'
    with open(filename, 'r') as inputfile:
        text = inputfile.read()
    samples_text, instructions_text = text.split('\n\n\n\n')
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
    instruction_pattern = r'([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)'
    instructions = [
        [int(a) for a in row]
        for row in re.findall(instruction_pattern, instructions_text)
    ]
    return samples, instructions


def filter_ops(sample):
    before = sample['before']
    after = sample['after']
    a, b, c = sample['op'][1:]
    valid_ops = [op for op in Ops._all() if op(a, b, c, before) == after]
    return valid_ops


def solved(logic):
    row_solved = (logic == 1).any(1).all()
    col_solved = (logic == 1).any(0).all()
    right_num = len(np.transpose((logic == 1).nonzero())) == NUM_OPS
    return row_solved and col_solved and right_num


def has_answer(vector):
    return (vector == 1).any()


def eliminate(vector):
    vector[(vector != 1).nonzero()] = -1


def answer_forced(vector):
    return np.transpose((vector != -1).nonzero()).size == 1


def mark_answer(vector):
    vector[(vector != -1).nonzero()] = 1


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
assert len(all_ops) == NUM_OPS

after = Ops.eqrr(2, 1, 3, [5, 3, 5, 8])
assert after == [5, 3, 5, 0], after

after = Ops.eqrr(2, 0, 3, [5, 3, 5, 8])
assert after == [5, 3, 5, 1], after

after = Ops.gtir(2, 0, 3, [5, 3, 5, 8])
assert after == [5, 3, 5, 0], after


TESTING = 'test' in sys.argv

if __name__ == '__main__':
    main()
