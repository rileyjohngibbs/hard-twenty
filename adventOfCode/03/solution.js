var fs = require('fs');

var lines = fs.readFileSync('input.txt', 'ascii').split('\n');

const lineRegex = /#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)/;

function parseLine(line) {
  const match = line.match(lineRegex);
  return {
    'id': parseInt(match[1]),
    'x': parseInt(match[2]),
    'y': parseInt(match[3]),
    'width': parseInt(match[4]),
    'height': parseInt(match[5])
  }
}

const parsedLines = lines.filter((x) => x).map(parseLine);

const width = Math.max(...parsedLines.map((line) => {
  return line.x + line.width;
})) + 1;
const height = Math.max(...parsedLines.map((line) => {
  return line.y + line.height;
})) + 1;

let fabric = new Array(height).fill(null);
fabric.forEach((x, i) => {
  fabric[i] = new Array(width).fill(0);
});

parsedLines.forEach((line) => {
  for (let j = line.y; j < line.y + line.height; j++) {
    for (let i = line.x; i < line.x + line.width; i++) {
      fabric[j][i] += 1;
    }
  }
});

const overlaps = fabric.map((row) => {
  return row.map((x) => x > 1).reduce((a, b) => a + b);
}).reduce((a, b) => a + b);

console.log(overlaps);
