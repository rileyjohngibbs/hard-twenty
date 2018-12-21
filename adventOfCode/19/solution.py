import re
import sys


def main():
    instruction_register, instructions = digest_input()
    registers = [0, 0, 0, 0, 0, 0]
    index = 0
    while index < len(instructions):
        instruction = instructions[index]
        registers = apply_instruction(registers, instruction)
        registers[instruction_register] += 1
        index = registers[instruction_register]
    print(registers[0])
    registers = [1, 0, 0, 0, 0, 0]
    index = 0
    while index < len(instructions):
        instruction = instructions[index]
        registers = apply_instruction(registers, instruction)
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
    return op(*instruction['args'], registers)


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


TESTING = 'test' in sys.argv

if __name__ == '__main__':
    main()
