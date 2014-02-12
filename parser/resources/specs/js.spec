
jsFile =>

{command}*

;

====<subgrammars>===============
<{html5}>
====</subgrammars>==============

====[parameter]===============

parameter =>@value;

separator: '_, '

====[/parameter]==============



commandBody => \{
  {command}*
\};


====[command]========

varDeclaration => var @name_(=_{expression}_)?\;;

expressionCommand => {expression}_\;;

assignment => {lvalue} = {expression}_\;;

return => return( {expression})?_\;;

comment => //[^\n]*[\n];

if => if \({expression}\) {commandBody}( else {commandBody})?;

if => if \({expression}\) {command}( else {command})?;

try => try {commandBody}
catch\({expression}\) {commandBody};

for => for \({expression}_\; {expression}_\; {expression}\) {commandBody};

for => for \({varDeclaration}_ {expression}_\; {expression}\) {commandBody};

iteratorFor => for \({variable} in {expression}_\) {commandBody};

funcDeclaration => function @name\({parameter}*\) {commandBody};

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

embeddedElement => {widget};

operators: ' == ' ' < ' ' > ' ' <= ' ' >= ' r:' ^ ' '*' ' / ' ' + ' '-'

separator: '_, '

====[/expression]===============



====[lvalue]==================

variable => @name;

operators: '.'

function => {lvalue}\(_{expression}*_\);

arrayIndex => {lvalue}\[{expression}\];

lambda => function \(\) {commandBody};

====[/lvalue]=================



