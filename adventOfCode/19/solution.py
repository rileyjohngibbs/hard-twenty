from pdb import pm, set_trace
import re
import sys


def main():
    instruction_register, instructions = digest_input()
    # Part One
    registers = [0, 0, 0, 0, 0, 0]
    index = 0
    while index < len(instructions):
        instruction = instructions[index]
        apply_instruction(registers, instruction)
        registers[instruction_register] += 1
        index = registers[instruction_register]
    print(registers[0])
    # Part Two
    registers = [1, 0, 0, 0, 0, 0]
    index = 0
    history = []
    skip = 1000
    nine_bump, thirteen_bump, four_bump = True, True, False
    while index < len(instructions):
        skip -= 1
        instruction = instructions[index]
        history.append({'registers': registers[:], 'index': index, **instruction})
        if not skip:
            show_last(history)
            set_trace()
            if not skip:
                skip = 10
        apply_instruction(registers, instruction)
        registers[instruction_register] += 1
        index = registers[instruction_register]
    print(registers[0])


def digest_input():
    filename = 'test.txt' if TESTING else 'input.txt'
    with open(filename, 'r') as inputfile:
        lines = inputfile.readlines()
    instruction_register = int(re.search(r'[0-9]', lines[0])[0])
    return instruction_register, list(map(line_to_instructions, lines[1:]))


def line_to_instructions(line):
    pattern = r'([a-z]+) ([0-9]+) ([0-9]+) ([0-9]+)'
    operation, a, b, c = re.match(pattern, line).groups()
    return {'op': operation, 'args': (int(a), int(b), int(c))}


def apply_instruction(registers, instruction):
    op = getattr(Ops, instruction["op"])
    op(*instruction['args'], registers)


def show_last(history, a=1, b=None):
    for snapshot in history[-a:-a+b if b else None]:
        show(snapshot)


def show(snapshot):
    print(
        f'{"-"*1} {snapshot["index"]:2} {snapshot["op"]} {"-"*9}\n'
        f'{str(snapshot["args"][0]):2} '
            f'{snapshot["registers"][0]:10}{snapshot["registers"][3]:10}\n'
        f'{str(snapshot["args"][1]):2} '
            f'{snapshot["registers"][1]:10}{snapshot["registers"][4]:10}\n'
        f'{str(snapshot["args"][2]):2} '
            f'{snapshot["registers"][2]:10}{snapshot["registers"][5]:10}'
    )


class Ops:

    @classmethod
    def _all(cls):
        return [getattr(cls, a) for a in dir(cls) if not a.startswith('_')]

    @staticmethod
    def addr(a, b, c, before):
        value = before[a] + before[b]
        before[c] = value

    @staticmethod
    def addi(a, b, c, before):
        value = before[a] + b
        before[c] = value

    @staticmethod
    def mulr(a, b, c, before):
        value = before[a] * before[b]
        before[c] = value

    @staticmethod
    def muli(a, b, c, before):
        value = before[a] * b
        before[c] = value

    @staticmethod
    def banr(a, b, c, before):
        value = before[a] & before[b]
        before[c] = value

    @staticmethod
    def bani(a, b, c, before):
        value = before[a] & b
        before[c] = value

    @staticmethod
    def borr(a, b, c, before):
        value = before[a] | before[b]
        before[c] = value

    @staticmethod
    def bori(a, b, c, before):
        value = before[a] | b
        before[c] = value

    @staticmethod
    def setr(a, b, c, before):
        value = before[a]
        before[c] = value

    @staticmethod
    def seti(a, b, c, before):
        value = a
        before[c] = value

    @staticmethod
    def gtir(a, b, c, before):
        value = int(a > before[b])
        before[c] = value

    @staticmethod
    def gtri(a, b, c, before):
        value = int(before[a] > b)
        before[c] = value

    @staticmethod
    def gtrr(a, b, c, before):
        value = int(before[a] > before[b])
        before[c] = value

    @staticmethod
    def eqir(a, b, c, before):
        value = int(a == before[b])
        before[c] = value

    @staticmethod
    def eqri(a, b, c, before):
        value = int(before[a] == b)
        before[c] = value

    @staticmethod
    def eqrr(a, b, c, before):
        value = int(before[a] == before[b])
        before[c] = value


TESTING = 'test' in sys.argv

if __name__ == '__main__':
    main()
