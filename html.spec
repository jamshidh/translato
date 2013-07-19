
file =>
{element}
;


ident => [a-zA-Z]\w*;


====[attribute]===============
attribute =>@name="@value([^"]*)";

separator: ' '
====[/attribute]==============

====[paramater]===============

paramater =>@value;

separator: '_, '

====[/paramater]==============



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

left: <@tagName {attribute}*_>;

right: </@tagName>;

separator: '\n'

====[/node]=====================

====[command]========

varDeclaration => var @name_(=_{expression}_)?\;;

expressionCommand => {expression}_\;;

assignment => {lvalue} = {expression}_\;;

return => return( {expression})?_\;;

comment => //@value{ident};

if => if \({expression}\) {body}( else {body})?;

try => try {body}
catch\({expression}\) {body};

for => for \({expression}_\; {expression}_\; {expression}\) {body};

for => for \({varDeclaration}_\; {expression}_\; {expression}\) {body};

funcDeclaration => function @name\({paramater}*\) {body};

separator: '\n'

====[/command]==================



====[expression:lvalue]=========

num => @value(\d+);

string => "@value([^"]*)";

operators: ' == ' ' < ' ' > ' ' <= ' ' >= ' '+' '*' '-'

separator: '_, '

====[/expression]===============



====[lvalue]==================

variable => @name;

function => {lvalue}\({expression}*\);

#array => {lvalue}\[{expression}\];

lambda => function () {body};

operators: '.'

====[/lvalue]=================



