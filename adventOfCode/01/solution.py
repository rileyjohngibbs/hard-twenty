with open("input.txt", "rb") as inputfile:
    lines = inputfile.readlines()


# Part One


print(sum(map(int, lines)))


# Part Two


frequencies = set([0])
current_frequency = 0

def linegen():
    while True:
        for line in lines:
            yield line

for line in linegen():
    current_frequency += int(line)
    if current_frequency in frequencies:
        break
    else:
        frequencies.add(current_frequency)

print(current_frequency)
