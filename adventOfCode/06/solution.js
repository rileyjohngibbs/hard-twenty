var fs = require('fs');

const inputLines = fs.readFileSync('input.txt', 'ascii')
//const inputLines = "1, 1\n1, 6\n8, 3\n3, 4\n5, 5\n8, 9"
  .split('\n')
  .map((s) => s.replace('\n'))
  .filter((x) => x);

const centers = inputLines.map((line) => {
  [ , x, y] = line.match(/([0-9]+), ([0-9]+)/).map((x) => parseInt(x));
  return {x, y, area: [], infinite: false};
});

const min_x = Math.min(...centers.map((c) => c.x));
const max_x = Math.max(...centers.map((c) => c.x));
const min_y = Math.min(...centers.map((c) => c.y));
const max_y = Math.max(...centers.map((c) => c.y));

const tapestry = [...Array(max_y + 1).keys()].slice(min_y).map((y) => {
  return [...Array(max_x + 1).keys()].slice(min_x).map((x) => {return {x, y}});
}).reduce((r, s) => r.concat(s));

tapestry.forEach((point) => {
  let closest_center = null;
  let shortest_distance = null;
  centers.forEach((center) => {
    let distance = Math.abs(point.x - center.x) + Math.abs(point.y - center.y);
    if (distance < shortest_distance || shortest_distance === null) {
      shortest_distance = distance;
      closest_center = center;
    } else if (distance === shortest_distance) {
      closest_center = null;
    }
  });
  if (closest_center !== null) {
    closest_center.area.push(point);
    if (point.x === min_x || point.x === max_x || point.y === min_y || point.y === max_y) {
      closest_center.infinite = true;
    }
  }
});

biggest_center = centers.reduce((c, d) => {
  if (!d.infinite) {
    return c.area.length < d.area.length ? d : c;
  } else {
    return c;
  }
});

console.log(biggest_center.area.length);
