import numpy as np
import re

with open('input.txt', 'r') as inputfile:
    input_lines = [s.replace('\n', '') for s in inputfile.readlines()]

steps = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'

def index(step):
    return ord(step) - 65


# Part One


requirements = np.zeros((26, 26), dtype=bool)

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


# Part Two


requirements = np.zeros((26, 26), dtype=int)

for line in input_lines:
    regex = r'Step ([A-Z]) must be finished before step ([A-Z]) can begin.'
    needed, todo = re.match(regex, line).groups()
    requirements[index(todo), index(needed)] = 60 + index(needed) + 1

workers = [None, None, None, None, None]

remaining = steps
order = []
time_count = 0

def reqs_done(step):
    return not requirements[index(step)].any()


def unclaimed(step):
    return step not in workers


while remaining:
    time_count += 1
    if time_count % 50 == 0:
        print(time_count)
    for i, step in enumerate(workers):
        if step is None:
            candidates = filter(
                lambda rem: reqs_done(rem) and unclaimed(rem), 
                remaining
            )
            step = next(candidates, None)
            workers[i] = step
            if step is not None:
                print(f"Worker {i} is starting {step}")
                order.append(step)
        if step is not None:
            step_index = index(step)
            column = requirements[:, step_index]
            column -= 1
            column[(column < 0).nonzero()] = 0
            if not column.any():
                workers[i] = None
                remaining = remaining.replace(step, '')

print(time_count)
