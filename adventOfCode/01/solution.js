var fs = require('fs');

fs.readFile('input.txt', 'ascii', function(err, contents) {
  var lines = contents.split('\n');
  var total = lines.map(function(x) {
    return parseInt(x) || 0;
  }).reduce(function(x, y) {
    return x + y;
  });
  console.log(total);
});
