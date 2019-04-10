var TurndownService = require('turndown');
var fs = require('fs');

var home = '/Users/olli/src/lisp/cl-readme/';

var htmlString = fs.readFileSync(home + 'make-readme/generated/readme.html', 'utf8');
var turndownService = new TurndownService();
var markdown = turndownService.turndown(htmlString);
fs.writeFileSync(home + 'make-readme/generated/readme.md', markdown);
