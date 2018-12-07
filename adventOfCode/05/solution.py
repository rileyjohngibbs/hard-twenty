with open('input.txt', 'r') as inputfile:
    polymer = inputfile.read().replace('\n', '')


def react(a, b):
    return a != b and a.lower() == b.lower()


final_polymer = []

for unit in polymer:
    final_polymer.append(unit)
    last_two = final_polymer[-2:]
    while len(final_polymer) >= 2 and react(*last_two):
        final_polymer[-2:] = []
        last_two = final_polymer[-2:]

print(len(final_polymer))
