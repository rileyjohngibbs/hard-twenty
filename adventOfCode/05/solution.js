var fs = require('fs');

const polymer = fs.readFileSync('input.txt', 'ascii').replace('\n', '');

function collapse(polymer) {
  let final_polymer = [];
  polymer.split('').forEach((a) => {
    final_polymer.push(a);
    while (reacts(...final_polymer.slice(a.length - 3))) {
      final_polymer.pop();
      final_polymer.pop();
    }
  });
  return final_polymer;
}

function reacts(a, b) {
  if (!a || !b) {return false;}
  return (a != b) && (a.toLowerCase() === b.toLowerCase());
}

const collapsed = collapse(polymer).join('');
console.log(collapsed.length);
