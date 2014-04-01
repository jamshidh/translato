

regexFile =>
{regex}
;

====[regex]===============

regex => /{regexItem}*/{regexFlag}*;

====[/regex]==============



====[regexItem]===========

reWord=>[a-zA-Z\-:#]+;

reStart => ^;

reFinish => $;

escapedSlash=>\\/;

escapedDot=>\\\.;

inlineCharset=>{escapedCharsetChar};

charset=>\[{charsetChar}*\];
charsetWithDash=>\[{charsetChar}*-\];
inverseCharset=>\[^{charsetChar}*\];

zeroOrMore=>{regexItem}\*;

oneOrMore=>{regexItem}\+;

optional=>{regexItem}\?;

reParen=>\({regexItem}*\);

reRepeatCount => {regexItem}{reCount};

operators: '|'

separator: ''

====[/regexItem]===========

====[reCount]==================

reExact=>\{\d+\};

reBounds=>\{@min(\d+),@max(\d+)_\};

====[/reCount]=================



====[charsetChar:escapedCharsetChar]===========
regularChar=>@value([a-zA-Z0-9+#?]);
operators: '-'
====[/charsetChar]==========

====[escapedCharsetChar]===========
anyChar=>\.;
wordChar=>\\w;
notWordChar=>\\W;
spaceChar=>\\s;
notSpaceChar=>\\S;
digitChar=>\\d;
escapedChar=>\\[\-\\/?];
====[/escapedCharsetChar]==========



====[regexFlag]========

global=>g;
caseInsensitive=>i;

====[/regexFlag]=======
