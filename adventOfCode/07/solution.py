import numpy as np
import re
import sys

TESTING = "test" in sys.argv

if TESTING:
    input_lines = [
        "Step C must be finished before step A can begin.",
        "Step C must be finished before step F can begin.",
        "Step A must be finished before step B can begin.",
        "Step A must be finished before step D can begin.",
        "Step B must be finished before step E can begin.",
        "Step D must be finished before step E can begin.",
        "Step F must be finished before step E can begin.",
    ]
else:
    with open('input.txt', 'r') as inputfile:
        input_lines = [s.replace('\n', '') for s in inputfile.readlines()]

steps = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' if not TESTING else 'ABCDEF'

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


requirements = np.zeros((27 - 20*TESTING, 26 - 20*TESTING), dtype=int)

for line in input_lines:
    regex = r'Step ([A-Z]) must be finished before step ([A-Z]) can begin.'
    needed, todo = re.match(regex, line).groups()
    requirements[index(todo), index(needed)] = 60 * (not TESTING) + index(needed) + 1
    requirements[-1, index(todo)] = 60 * (not TESTING) + index(todo) + 1

workers = 2 + 3*(not TESTING)

remaining = steps
time_count = 0
log_pass = 0
steps_this_count = []


def reqs_done(step):
    return not requirements[index(step)].any()


def unclaimed(step):
    return step not in workers


def progress(step):
    step_index = index(step)
    column = requirements[:, step_index]
    column -= 1
    column[(column < 0).nonzero()] = 0
    if not column.any():
        global remaining
        remaining = remaining.replace(step, '')


while remaining:
    in_progress = [step for step in steps_this_count if step in remaining]
    new_steps = [
        step 
        for step in remaining 
        if reqs_done(step) and step not in steps_this_count
    ]
    steps_this_count = (in_progress + new_steps)[:workers]
    if 'log' in sys.argv:
        if log_pass:
            log_pass -= 1
        else:
            print(f'Time: {time_count}')
            print(steps_this_count)
            for step in steps:
                print(str(step).rjust(3), end='')
            print('')
            for row in requirements:
                for x in row:
                    print(str(x).rjust(3), end='')
                print('')
            log_pass = int(input('') or 1) - 1
    for step in steps_this_count:
        progress(step)
    time_count += 1

print(time_count)
