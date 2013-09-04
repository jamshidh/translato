
file =>
{element}
;


ident => [a-zA-Z]\w*;


====[attribute]===============
attribute =>@name="@value([^"]*)";

separator: ' '
====[/attribute]==============

====[parameter]===============

parameter =>@value;

separator: '_, '

====[/parameter]==============



word =>[^<> \n\r\t]+;

body => \{
  {command}*
\};



====[node]======================

script =><script_>
  {command}*
</script_>;

script =><script src="@src([^"]*)">_</script>;

element =><@tagName {attribute}* />;

element =>{node}*;

text => {word}+;

left: <@tagName {attribute}*_>
;

right: _</@tagName>;

separator: '\n'

====[/node]=====================

====[command]========

varDeclaration => var @name_(=_{expression}_)?\;;

expressionCommand => {expression}_\;;

assignment => {lvalue} = {expression}_\;;

return => return( {expression})?_\;;

comment => //[^\n]*[\n];

if => if \({expression}\) {body}( else {body})?;

try => try {body}
catch\({expression}\) {body};

for => for \({expression}_\; {expression}_\; {expression}\) {body};

for => for \({varDeclaration}_\; {expression}_\; {expression}\) {body};

funcDeclaration => function @name\({parameter}*\) {body};

separator: '\n'

====[/command]==================



====[expression:lvalue]=========

num => @value(\d+);

string => "@value([^"]*)";

embeddedElement => {node}*;

operators: ' == ' ' < ' ' > ' ' <= ' ' >= ' r:' ^ ' '*' ' / ' ' + ' '-'

separator: '_, '

====[/expression]===============



====[lvalue]==================

variable => @name;

operators: '.'

function => {lvalue}\({expression}*\);

#array => {lvalue}\[{expression}\];

lambda => function () {body};

====[/lvalue]=================



