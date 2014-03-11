
regexFile =>
{regex}
;

====[regex]===============

regex => /{regexItem}*/{regexFlag}*;

====[/regex]==============



====[regexItem]===========

reWord=>[a-zA-Z\-]+;

reStart => ^;

reFinish => $;

inlineCharset=>{charsetChar};

charset=>\[{charsetChar}*\];

zeroOrMore=>{regexItem}\*;

#oneOrMore=>{regexItem}+;

#optional=>{regexItem}?;

separator: ''

====[/regexItem]===========

====[charsetChar]===========
anyChar=>\.;
wordChar=>\\w;
escapedChar=>\\[\-];
====[/charsetChar]==========



====[regexFlag]========

global=>g;

====[/regexFlag]=======
