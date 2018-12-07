with open('input.txt', 'r') as inputfile:
    polymer = inputfile.read().replace('\n', '')


def collapse(polymer, drop=''):
    final_polymer = []
    for unit in polymer.replace(drop.lower(), '').replace(drop.upper(), ''):
        final_polymer.append(unit)
        last_two = final_polymer[-2:]
        while len(final_polymer) >= 2 and react(*last_two):
            final_polymer[-2:] = []
            last_two = final_polymer[-2:]
    return ''.join(final_polymer)


def react(a, b):
    return a != b and a.lower() == b.lower()


# Part One


collapsed = collapse(polymer)
print(len(collapsed))


# Part Two

alphabet = [chr(i) for i in range(65, 65+26)]
print(min(len(collapse(collapsed, alpha)) for alpha in alphabet))
