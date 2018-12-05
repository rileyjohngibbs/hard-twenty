var fs = require('fs');

lines = fs.readFileSync('input.txt', 'ascii').split('\n');

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
