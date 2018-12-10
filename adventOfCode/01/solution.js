var fs = require('fs');

const lines = fs.readFileSync('input.txt', 'ascii')
  .split('\n')
  .map((x) => parseInt(x.replace('\n')))
  .filter((x) => x);

// Part One

const total = lines.reduce((x, y) => x + y);

console.log(total);

// Part Two


