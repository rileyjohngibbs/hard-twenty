var fs = require('fs');

lines = fs.readFileSync('input.txt', 'ascii').split('\n');

// Part One

function count(line) {
  counts = [];
  line.split('').forEach((c) => {
    counts[c] = counts[c] === undefined ? 1 : counts[c] + 1;
  });
  var twice = false;
  var thrice = false;
  for (key in counts) {
    if (counts[key] == 2) {
      twice = true;
    } else if (counts[key] == 3) {
      thrice = true;
    }
  }
  return [twice, thrice];
}

var twice_counts = 0;
var thrice_counts = 0;

lines.forEach((line) => {
  [twice, thrice] = count(line);
  twice_counts += twice;
  thrice_counts += thrice;
});

console.log(twice_counts * thrice_counts);

// Part Two

function samesies(base, comparison) {
  var same_count = samechars(base, comparison).length;
  var differences = base.length - same_count;
  return differences === 1;
}

function samechars(base, comparison) {
  var matches = "";
  base.split("").forEach((c, i) => {
    if (c === comparison[i]) {
      matches += c;
    }
  });
  return matches;
}

lines.forEach((base_line, i) => {
  lines.slice(i + 1).forEach((comparison_line) => {
    if (samesies(base_line, comparison_line)) {
      console.log(samechars(base_line, comparison_line));
    }
  });
});
