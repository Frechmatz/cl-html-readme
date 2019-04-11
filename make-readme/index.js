var TurndownService = require('turndown');
var fs = require('fs');

var htmlFile = process.argv[2];
var markdownFile = process.argv[3];

console.log('Input: ' + htmlFile);
console.log('Output: ' + markdownFile);
var htmlString = fs.readFileSync(htmlFile, 'utf8');
var turndownService = new TurndownService();
var markdown = turndownService.turndown(htmlString);
fs.writeFileSync(markdownFile, markdown);
