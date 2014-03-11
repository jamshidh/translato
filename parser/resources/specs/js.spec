
whitespace:/\*.*\*/;

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



====[commandBody:command]====

fullBody => \{
  {command}*
\};

====[/commandBody]===========


====[varDeclaration]========

varDeclaration => @name_(=_{expression}_)?;

separator: '_,\n   '

====[/varDeclaration]=======


====[command]========

varDeclarationExpression => var {varDeclaration}*;

expressionCommand => {expression}_\;?;

assignment => {lvalue} = {expression}_\;?;
addAssignment => {lvalue} \+= {expression}_\;?;

return => return( {expression})?_\;?;
#return => return([ \t]+{expression})?_\;?;

comment => //[^\n]*[\n];

if => if \({expression}\) {commandBody}( else {commandBody})?;

try => try {commandBody}
catch\({expression}\) {commandBody};

for => for \({expression}_\; {expression}_\; {expression}\) {commandBody};

for => for \({varDeclaration}_ {expression}_\; {expression}\) {commandBody};

iteratorFor => for \({variable} in {expression}_\) {commandBody};

funcDeclaration => function @name\({parameter}*\) {fullBody};

blankCommand => \;;

separator: '\n'

====[/command]==================



====[fieldName]=====================

fieldName => "@value([^"]*)";
fieldName => '@value([^']*)';
fieldName => @value;

====[/fieldName]====================

====[field]=====================

field => {fieldName}:_{expression};

separator: '_, '

====[/field]====================

====[expression:lvalue]=========

num => @value(\d+);
nnum => -@value(\d+);

string => "@value([^"]*)";

singleQuoteString => '@value([^']*)';

incrementor => {expression}\+\+;

booleanNot => !{expression};

typeof => typeof {expression};

object => \{_{field}*_\};

embeddedElement => {widget};

conditional => {expression} \? {expression} : {expression};

void => void {expression};

operators: ' instanceof ' ' in ' ' === ' ' !== ' ' == ' ' != ' ' < ' ' > ' ' <= ' ' >= ' r:' ^ ' '_*_' ' / ' ' + ' '-' ' || ' ' && '

separator: '_, '

====[/expression]===============



====[lvalue]==================

variable => @name;

array => \[_{expression}*_\];

paren => \(_{expression}_\);

operators: '.'

function => {lvalue}\(_{expression}*_\);

arrayIndex => {lvalue}\[{expression}\];

lambda => function \(_{parameter}*_\) {fullBody};

====[/lvalue]=================



