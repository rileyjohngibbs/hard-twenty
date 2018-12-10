import numpy as np
import re

with open('input.txt', 'r') as inputfile:
    input_lines = [s.replace('\n', '') for s in inputfile.readlines()]

steps = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
requirements = np.zeros((26, 26), dtype=bool)

def index(step):
    return ord(step) - 65

for line in input_lines:
    regex = r'Step ([A-Z]) must be finished before step ([A-Z]) can begin.'
    needed, todo = re.match(regex, line).groups()
    requirements[index(todo), index(needed)] = 1

remaining = steps
order = []

while remaining:
    for step in remaining:
        step_index = index(step)
        if not requirements[step_index].any():
            requirements[:, step_index].fill(0) 
            order.append(step)
            remaining = remaining.replace(step, '')
            break

print(''.join(order))
