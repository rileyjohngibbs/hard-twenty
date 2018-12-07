var fs = require('fs');

const polymer = fs.readFileSync('input.txt', 'ascii').replace('\n', '');

function collapse(polymer, drop) {
  let final_polymer = [];
  polymer.replace(new RegExp(drop, 'gi'), '').split('').forEach((a) => {
    final_polymer.push(a);
    while (reacts(...final_polymer.slice(a.length - 3))) {
      final_polymer.pop();
      final_polymer.pop();
    }
  });
  return final_polymer.join('');
}

function reacts(a, b) {
  if (!a || !b) {return false;}
  return (a != b) && (a.toLowerCase() === b.toLowerCase());
}


// Part One


const collapsed = collapse(polymer);
console.log(collapsed.length);


// Part Two


const alphabet = [...Array(26).keys()]
  .map((x) => String.fromCharCode(x + 97));
console.log(
  Math.min(...alphabet.map(
    (a) => collapse(collapsed, a).length
  ))
);
