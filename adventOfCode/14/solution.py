import sys


def main():
    recipe_count = digest_input()
    recipes = [3, 7]
    elves = [0, 1]
    while len(recipes) < recipe_count + 10:
        if TESTING:
            show(recipes, elves)
        recipes, elves = iterate(recipes, elves)
    print(''.join(map(str, recipes[-10:])))


def digest_input():
    if TESTING:
        return TESTING
    else:
        filename = 'input.txt'
        with open(filename, 'r') as inputfile:
            text = inputfile.read()
        return int(text)


def show(recipes, elves):
    for i, recipe in enumerate(recipes):
        if i in elves:
            print(f'({recipe})', end='')
        else:
            print(f' {recipe} ', end='')
    print('\n')


def iterate(recipes, elves):
    current_total = sum(recipes[elf] for elf in elves)
    added_recipes = [int(a) for a in str(current_total)]
    new_recipes = recipes + added_recipes
    new_elves = [next_recipe_index(elf, new_recipes) for elf in elves]
    return new_recipes, new_elves


def next_recipe_index(current, recipes):
    current_score = recipes[current]
    new_index = (current + 1 + current_score) % len(recipes)
    if TESTING:
        print(f'{current_score} moved to index {new_index}')
    return new_index


TESTING = int(next(iter(sys.argv[1:]), '0'))

if __name__ == '__main__':
    main()
