import sys


def main():
    # Part One
    if not TESTING or 'one' in sys.argv:
        recipe_count = int(digest_input())
        recipes = ['3', '7']
        elves = [0, 1]
        while len(recipes) < recipe_count + 10:
            if TESTING:
                show(recipes, elves)
            recipes, elves = iterate(recipes, elves)
        if TESTING:
            show(recipes, elves)
        print(''.join(map(str, recipes[recipe_count:recipe_count+10])))
    # Part Two
    if not TESTING or 'two' in sys.argv:
        recipe_list = digest_input()
        list_length = len(recipe_list)
        recipes = ['3', '7']
        elves = [0, 1]
        last_check = 0
        while recipe_list not in ''.join(recipes[-list_length-1:]):
            if len(recipes) - last_check > 1_000_000:
                last_check = len(recipes) - (len(recipes) % 1_000_000)
                print(f'Running length of the recipe list: {last_check}')
            if TESTING:
                show(recipes, elves)
            recipes, elves = iterate(recipes, elves)
        if TESTING:
            show(recipes, elves)
        print('Solution:')
        print(''.join(map(str, recipes)).find(recipe_list))


def digest_input():
    if TESTING:
        return TESTING
    else:
        filename = 'input.txt'
        with open(filename, 'r') as inputfile:
            text = inputfile.read().replace('\n', '')
        return text


def show(recipes, elves):
    for i, recipe in enumerate(recipes):
        if i in elves:
            print(f'({recipe})', end='')
        else:
            print(f' {recipe} ', end='')
    print('\n')


def iterate(recipes, elves):
    current_total = sum(int(recipes[elf]) for elf in elves)
    added_recipes = str(current_total)
    recipes.extend(added_recipes)
    new_elves = [next_recipe_index(elf, recipes) for elf in elves]
    return recipes, new_elves


def next_recipe_index(current, recipes):
    current_score = int(recipes[current])
    new_index = (current + 1 + current_score) % len(recipes)
    return new_index


TESTING = next(iter(sys.argv[1:]), False)

if __name__ == '__main__':
    main()
