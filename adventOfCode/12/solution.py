import re
import sys


def main():
    initial_states, rules = digest_input()
    # Part One
    zero_index = 0
    states = initial_states[:]
    for _ in range(20):
        states, zero_delta = increment_states(states, rules)
        zero_index += zero_delta
    print(sum(index - zero_index for index, plant in enumerate(states) if plant))
    # Part Two
    zero_index = 0
    states = initial_states[:]
    generations_remaining = 50_000_000_000
    seen_states = {tuple(states): (generations_remaining, zero_index)}
    while generations_remaining:
        states, zero_delta = increment_states(states, rules)
        zero_index += zero_delta
        generations_remaining -= 1
        key = tuple(states)
        if key not in seen_states:
            seen_states[key] = (generations_remaining, zero_index)
        else:
            previous_gen, previous_zero = seen_states[key]
            jumps = int(generations_remaining / (previous_gen - generations_remaining))
            zero_index += (zero_index - previous_zero) * jumps
            generations_remaining = generations_remaining % (previous_gen - generations_remaining)
    print(sum(index - zero_index for index, plant in enumerate(states) if plant))


def digest_input():
    if TESTING:
        filename = 'test.txt'
    else:
        filename = 'input.txt'
    with open(filename, 'r') as inputfile:
        inputlines = inputfile.readlines()
    initial_string = re.findall(r'[#\.]', inputlines[0])
    initial_states = [{'#': True, '.': False}[s] for s in initial_string]
    rule_strings = inputlines[2:]
    rules = [digest_rule_string(s) for s in rule_strings]
    return initial_states, rules


def digest_rule_string(string):
    match = re.match(r'([\.#]{5}) => (\.|#)', string)
    return Rule(*match.groups())


def increment_states(states, rules):
    temp_states = [False, False, *states, False, False]
    new_states = []
    for index, state in enumerate(temp_states):
        left = max(index - 2, 0)
        right = min(index + 3, len(temp_states))
        center = temp_states[left:right]
        left_buffer = [False] * max(2 - index, 0)
        right_buffer = [False] * max(index - len(temp_states) + 3, 0)
        check_state = left_buffer + center + right_buffer
        for rule in rules:
            if rule.match(check_state):
                new_states.append(rule.conclusion)
                break
        else:
            if TESTING:
                new_states.append(False)
            else:
                raise Exception(f'No valid rule found for state: {check_state}')
    zero_delta = 2
    while new_states[0] is False:
        new_states.pop(0)
        zero_delta -= 1
    while new_states[-1] is False:
        new_states.pop(-1)
    return new_states, zero_delta


class Rule(object):

    def __init__(self, hypothesis_string, conclusion_string):
        map_ = {'#': True, '.': False}
        self.hypothesis = [map_[s] for s in hypothesis_string]
        self.conclusion = map_[conclusion_string]

    def match(self, states):
        return self.hypothesis == states


TESTING = 'test' in sys.argv

if __name__ == '__main__':
    main()
