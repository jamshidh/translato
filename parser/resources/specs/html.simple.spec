
file =>
{doctype}?

{element}
;

doctype => <!DOCTYPE @value>;


ident => [$a-zA-Z]\w*;

====[attribute]===============
attribute =>@name([a-zA-Z\-]+)(="@value([^"]*)")?;

#booleanAttribute => @name([a-zA-Z\-]+);

separator: ' '
====[/attribute]==============

====[parameter]===============

parameter =>@value;

separator: '_, '

====[/parameter]==============



word =>[^< \n\r\t]+;

body => \{
  {command}*
\};



====[node]======================

script =><script_>
  {command}*
</script_>;

script =><script src="@src([^"]*)">_</script>;

element =><@tagName_{attribute}*_/>;

element =>{node}*;

text => {word}+;

left: <@tagName_{attribute}*_>
  ;

right: _
</@tagName>;

separator: '\n'

====[/node]=====================

====[command]========

varDeclaration => var @name_(=_{expression}_)?\;;

expressionCommand => {expression}_\;;

assignment => {lvalue} = {expression}_\;;

return => return( {expression})?_\;;

comment => //[^\n]*[\n];

if => if \({expression}\) {body}( else {body})?;

if => if \({expression}\) {command}( else {command})?;

try => try {body}
catch\({expression}\) {body};

for => for \({expression}_\; {expression}_\; {expression}\) {body};

for => for \({varDeclaration}_ {expression}_\; {expression}\) {body};

iteratorFor => for \({variable} in {expression}_\) {body};

funcDeclaration => function @name\({parameter}*\) {body};

separator: '\n'

====[/command]==================

====[field]=====================

field => {string}:_{expression};

separator: '_, '

====[/field]====================

====[expression:lvalue]=========

num => @value(\d+);

string => "@value([^"]*)";

singleQuoteString => '@value([^']*)';

incrementor => {expression}\+\+;

booleanNot => !{expression};

array => \[_{expression}*_\];

object => \{_{field}*_\};

embeddedElement => {node}*;

operators: ' == ' ' < ' ' > ' ' <= ' ' >= ' r:' ^ ' '*' ' / ' ' + ' '-'

separator: '_, '

====[/expression]===============



====[lvalue]==================

variable => @name;

operators: '.'

function => {lvalue}\(_{expression}*_\);

arrayIndex => {lvalue}\[{expression}\];

lambda => function \(\) {body};

====[/lvalue]=================



