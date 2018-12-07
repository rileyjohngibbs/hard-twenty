var fs = require('fs');

const inputLines = fs.readFileSync('input.txt', 'ascii')
  .split('\n')
  .map((s) => s.replace('\n'))
  .filter((x) => x)
  .sort();

var sleepSchedules = [];

function parseTime(line) {
  const timeMatch = line.match(/([0-9]+):([0-9]+)/);
  const hour = timeMatch[1], minute = timeMatch[2];
  if (hour == 23) {
    return 0;
  } else if (hour == 1) {
    return 59;
  } else {
    return minute;
  }
}

for (j in inputLines) {
  let line = inputLines[j];
  if (Boolean(line.match(/begins shift/))) {
    guardId = line.match(/#[0-9]+/)[0];
    if (sleepSchedules[guardId] === undefined) {
      sleepSchedules[guardId] = new Array(59).fill(0);
    }
  } else if (Boolean(line.match(/falls asleep/))) {
    startTime = parseTime(line);
  } else {
    stopTime = parseTime(line);
    for (let i=startTime; i<stopTime; i++) {
      sleepSchedules[guardId][i] += 1;
    }
  }
}

// Part One

let sleepiestId, sleepiestTotal = 0;

for (guardId in sleepSchedules) {
  let totalSleep = sleepSchedules[guardId].reduce((a, b) => a + b);
  if (totalSleep > sleepiestTotal) {
    sleepiestId = guardId;
    sleepiestTotal = totalSleep;
  }
}

let sleepiestMinute, sleepiestCount = 0;

sleepSchedules[sleepiestId].forEach((count, minute) => {
  if (count > sleepiestCount) {
    sleepiestMinute = minute;
    sleepiestCount = count;
  }
});
console.log(parseInt(sleepiestId.match(/#(.*)/)[1]) * sleepiestMinute);

// Part Two

sleepiestCount = 0;

for (guardId in sleepSchedules) {
  for (minute in sleepSchedules[guardId]) {
    if (sleepSchedules[guardId][minute] > sleepiestCount) {
      sleepiestId = guardId;
      sleepiestMinute = minute;
      sleepiestCount = sleepSchedules[guardId][minute];
    }
  }
}
console.log(parseInt(sleepiestId.match(/#(.*)/)[1]) * sleepiestMinute);
