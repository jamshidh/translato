
whitespace:/\*.*\*/;
whitespace://.*[\n];

jsFile =>

{command}*

;

====<subgrammars>===============
<{html5}>
<{regex}>
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


varName=>[$_a-zA-Z][$_a-zA-Z0-9]*;


====[varDeclaration]========

varDeclaration => @name([$_a-zA-Z][$_a-zA-Z0-9]*)_(=_{expression}_)?;

separator: '_,\n   '

====[/varDeclaration]=======


====[command]========

varDeclarationExpression => var {varDeclaration}*\;?;

expressionCommand => {expression}_\;?;

assignment => {lvalue} = {expression}_\;?;
addAssignment => {lvalue} \+= {expression}_\;?;

return => return( {expression})?_\;?;
#return => return([ \t]+{expression})?_\;?;

if => if \(_{expression}_\) {commandBody}( else {commandBody})?;

try => try {commandBody}
catch_\({expression}\) {commandBody};

for => for \({expression}_\; {expression}_\; {expression}\) {commandBody};

#Be careful- varDeclarationExpression and assignment already have terminating semicolons, don't repeat them 
#here

for => for \({varDeclarationExpression}_ {expression}_\; {expression}\) {commandBody};

for => for \({assignment}_ {expression}_\; {expression}\) {commandBody};

iteratorFor => for \({variable} in {expression}_\) {commandBody};

funcDeclaration => function @name\({parameter}*\) {fullBody};

blankCommand => \;;

separator: '\n'

====[/command]==================



====[fieldName]=====================

fieldName => "@value([^"]*)";
fieldName => '@value([^']*)';
#fieldName => @value;
fieldName => @value([$_a-zA-Z][$_a-zA-Z0-9]*);

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

delete => delete {expression};

new => new {expression};

throw => throw {expression};

object => \{_{field}*_\};

embeddedElement => {widget};

embeddedRegex => {regex};

conditional => {expression} \? {expression} : {expression};

void => void {expression};

operators: ' instanceof ' ' in ' ' === ' ' !== ' ' == ' ' != ' ' < ' ' > ' ' <= ' ' >= ' r:' ^ ' '_*_' ' / ' ' + ' '-' ' || ' ' && '

separator: '_, '

====[/expression]===============



====[lvalue]==================

variable => @name([$_a-zA-Z][$_a-zA-Z0-9]*);

array => \[_{expression}*_\];

paren => \(_{expression}_\);

operators: '.'

function => {lvalue}\(_{expression}*_\);

arrayIndex => {lvalue}\[{expression}\];

lambda => function \(_{parameter}*_\) {fullBody};

====[/lvalue]=================



