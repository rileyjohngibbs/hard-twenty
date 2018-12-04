with open("input.txt", "rb") as inputfile:
    lines = inputfile.readlines()

print(sum(map(int, lines)))
