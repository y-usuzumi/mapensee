var Benchmark = require('benchmark');
var suite = new Benchmark.Suite();

var longestCommonPrefix = function (strs) {
    if (strs.length === 0 || strs[0].length === 0) return;
    let j = 0;
    let prefix = '';

    for (j = 0; j < strs[0].length; j++) {
        for(let i = 0; i < strs.length - 1; i++){
            if(strs[i][j] !== strs[i + 1][j]) return prefix;
        }
        prefix += strs[0][j];
    }
    return prefix;
};

var longestCommonPrefix2 = function (strs) {
    if (strs.length === 0 || strs[0].length === 0) return;
    let j = 0;
    let prefix = [];

    for (j = 0; j < strs[0].length; j++) {
        for(let i = 0; i < strs.length - 1; i++){
            if(strs[i][j] !== strs[i + 1][j]) return prefix.join('');
        }
        prefix.push(strs[0][j]);
    }
    return prefix.join('');
};

var inputs = ["The quick brown fox jumps over the lazy dog", "The quick brown fox jaslkdjlakdfg", "The quick brown fox aklsjdfkjadf"]

// 添加测试
suite.add('No buffer', function() {
    longestCommonPrefix(inputs);
}).add('Buffer', function() {
    longestCommonPrefix2(inputs);
}).on('cycle', function(event) {
  console.log(String(event.target));
}).on('complete', function() {
  console.log('Fastest is ' + this.filter('fastest').map('name'));
}).run({ 'async': true });
