#!/usr/bin/env node

var TurndownService = require('turndown');
var fs = require('fs');

var htmlFile = process.argv[2];
var markdownFile = process.argv[3];

if (!htmlFile || !markdownFile) {
    console.log('Usage: html2markdown <inputFile> <outputFile>');
    process.exit(1);
}

console.log('Input file: ' + htmlFile);
console.log('Output file: ' + markdownFile);
var htmlString = fs.readFileSync(htmlFile, 'utf8');
var turndownService = new TurndownService();
var markdown = turndownService.turndown(htmlString);
fs.writeFileSync(markdownFile, markdown);
console.log('Written ' + markdownFile);
