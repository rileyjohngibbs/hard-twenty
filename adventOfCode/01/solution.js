var fs = require('fs');

const lines = fs.readFileSync('input.txt', 'ascii')
  .split('\n')
  .map((x) => parseInt(x.replace('\n')))
  .filter((x) => x);

// Part One

const total = lines.reduce((x, y) => x + y);

console.log(total);

// Part Two

let frequencies = {};

var currentFrequency = 0;
var i = 0;

while (frequencies[currentFrequency] === undefined) {
  frequencies[currentFrequency] = true;
  currentFrequency += lines[i++];
  if (i === lines.length) {
    i = 0;
  }
}

console.log(currentFrequency);
