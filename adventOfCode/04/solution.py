import re

with open("input.txt", "r") as inputfile:
    lines = [x.replace("\n", "") for x in inputfile.readlines() if x]

lines.sort()


def get_time(line):
    hour, minute = re.search(r"([0-9]+):([0-9]+)", line).groups()
    if hour == "23":
        return 0
    elif hour == "1":
        return 59
    else:
        return int(minute)


sleep_schedules = {}
guard_number = 0
for line in lines:
    shift_change = re.search(r"#([0-9]+)", line)
    if shift_change is not None:
        guard_number = int(shift_change.groups()[0])
        sleep_schedules.setdefault(guard_number, [0] * 60)
    sleep_start = re.search(r"falls asleep", line)
    if sleep_start is not None:
        start_time = get_time(line)
    sleep_stop = re.search(r"wakes up", line)
    if sleep_stop is not None:
        stop_time = get_time(line)
        sleep_schedules[guard_number][start_time:stop_time] = [
            count + 1
            for count in sleep_schedules[guard_number][start_time:stop_time]
        ]


# Part One


sleepiest_id, minutes = max(sleep_schedules.items(), key=lambda s: sum(s[1]))
sleepiest_minute = max(enumerate(minutes), key=lambda m: m[1])[0]
print(sleepiest_id * sleepiest_minute)


# Part Two


sleepiest_id, minutes = max(sleep_schedules.items(), key=lambda s: max(s[1]))
sleepiest_minute = max(enumerate(minutes), key=lambda m: m[1])[0]
print(sleepiest_id * sleepiest_minute)
