
whitespace:/\*.*\*/;
whitespace://.*[\n];

jsFile =>

{command}*

;

====<subgrammars>===============
<{regex}>
<{e4x}>
====</subgrammars>==============

====[parameter]===============

parameter =>{varName};

separator: '_, '

====[/parameter]==============



====[commandBody:command]====

blankCommandBody => \{ \};
  
fullBody => \{
  {command}*
\};

====[/commandBody]===========


varName=>[$_a-zA-Z][$_a-zA-Z0-9]*;


====[varDeclaration]========

varDeclaration => @name([$_a-zA-Z][$_a-zA-Z0-9]*)_(=_{expression}_)?;

separator: '_,\n   '

====[/varDeclaration]=======

====[case]===========

case=>
case {expression}:
  {command}*;

default=>
default:
  {command}*;

====[/case]==========

====[command]========

varDeclarationExpression => var {varDeclaration}*\;?;

expressionCommand => {expression}_\;?;

#assignment => {lvalue} = {expression}_\;?;
#addAssignment => {lvalue} \+= {expression}_\;?;
#subAssignment => {lvalue} -= {expression}_\;?;

return => return( {expression})?_\;?;
#return => return([ \t]+{expression})?_\;?;

if => if \(_{expression}_\) {commandBody}( else {commandBody})?;

do => do {commandBody} while\({expression}\)_\;;

while => while_\({expression}\) {commandBody};

switch => switch_\({expression}\) \{
  {case}*
\};

try => try {commandBody}
catch_\({expression}\) {commandBody};

for => for \({expressionList}_\; {expression}_\; {expression}\) {commandBody};

#Be careful- varDeclarationExpression and assignment already have terminating semicolons, don't repeat them 
#here

for => for \(_var {initializeList}_\; {expression}_\; {expression}_\) {commandBody};

#for => for \({assignment}_ {expression}_\; {expression}\) {commandBody};

iteratorDeclarationFor => for \(_var {variable} in {expression}_\) {commandBody};

iteratorFor => for \({variable} in {expression}_\) {commandBody};

blankCommand => \;;

separator: '\n'

====[/command]==================

initializeList => {initialize}+;

====[initialize]==============
initialize=>{variable}_=_{expression};
separator: '_, '
====[/initialize]=============


====[fieldName]=====================

fieldName => "@value([^"]*)";
fieldName => '@value([^']*)';
#fieldName => @value;
fieldName => @value([$_a-zA-Z][$_a-zA-Z0-9]*);

====[/fieldName]====================

====[field]=====================

field => {fieldName}_:_{expression};

separator: '_, '

====[/field]====================

====[number]============
int => @value(\d+);
float => @intPart(\d+)\.@fracPart(\d+);
hexNum => 0x@value([a-fA-F0-9]+);
====[/number]===========

expressionList => {expression}*;

====[expression:lvalue]=========

num => {number};

neg=> -{expression};

string => "@value([^"]*)";

singleQuoteString => '@value([^']*)';

incrementor => {expression}_\+\+;
decrementor => {expression}_--;
preIncrementor => \+\+_{expression};
preDecrementor => --_{expression};

booleanNot => !_{expression};

typeof => typeof {expression};

delete => delete {expression};

new => new {expression};

throw => throw {expression};

object => \{_{field}*_\};

embeddedElement => {e4xElement};

conditional => {expression} \? {expression} : {expression};

void => void {expression};

operators: ' instanceof ' ' in ' ' = ' ' += ' ' -= ' ' === ' ' !== ' ' == ' ' != ' ' < ' ' > ' ' <= ' ' >= ' r:' ^ ' '_*_' ' / ' ' % ' ' + ' ' - ' ' || ' ' && ' ' | ' ' & ' ' << '

separator: '_, '

====[/expression]===============



====[lvalue]==================

embeddedRegex => {regex};

variable => @name([$_a-zA-Z][$_a-zA-Z0-9]*);

array => \[_{expression}*_\];

paren => \(_{expression}_\);

operators: '_._'

funcDeclaration => function @name([$_a-zA-Z][$_a-zA-Z0-9]*)\(_{parameter}*_\) {fullBody};

function => {lvalue}\(_{expression}*_\);

arrayIndex => {lvalue}\[_{expression}_\];

lambda => function \(_{parameter}*_\) {fullBody};

====[/lvalue]=================



