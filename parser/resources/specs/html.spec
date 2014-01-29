
file =>
{doctype}?

{html}
;

html=>
<html>
  {head}
  {body}
</html>
;

head=>
<head>
  {headElements}*
</head>
;

body=>
<body>
  {widget}*
</body>
;

====[headElements:script]========

title=>
<title>{word}*</title>
;

meta=><meta http-equiv="@httpequiv([^"]*)" content="@content([^"]*)"_/?>;

link=><link rel="@rel([^"]*)" type="@type@content([^"]*)" href="@href([^"]*)"_/?>;

#area, base, br, col, embed, hr, img, input, keygen, param, source, track, wbr

style=>
<style>
  {declarationBlock}*
</style>
;

separator: '\n'
====[/headElements]==============

====[declarationBlock]===============

declarationBlock=>
{selector} 
\{ 
  {declaration}* 
\}
;

separator: '\n'

====[/declarationBlock]==============

====[declaration]===============

transition=>transition: @value([^;]*)\;;
color=>color: @value([^;]*)\;;

separator: '\n'

====[/declaration]==============

====[selector]===============

tagname=>@value;

filter=>{selector}\[{qualifier}\];

operators: ' > '

====[/selector]==============

====[qualifier]===============

hasAtt=>@value;

====[/qualifier]==============


====[widget:script]========

text => {word}+;

button=>
<button_{attribute}*_>{widget}*</button>
;

code=>
<code>{widget}*</code>
;

details=>
<details_{attribute}*_>
  {summary}
  {widget}*
</details>
;

dl=>
<dl>
  ({dt} 
  {dd}
  )+
</dl>
;

h1=>
<h1_{attribute}*_>{widget}*</h1>
;

progress=>
<progress_{attribute}*_>{widget}*</progress>
;

section=>
<section_{attribute}*_>
  {widget}*
</section>
;

separator: '\n'
====[/widget]==============

dt=>
<dt>{widget}*</dt>
;

dd=>
<dd>{widget}*</dd>
;


summary=>
<summary_{attribute}*_>
  {widget}*
</summary>
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

commandBody => \{
  {command}*
\};



====[script]======================

script =><script_>
  {command}*
</script_>;

script =><script src="@src([^"]*)">_</script>;

====[/script]=====================

====[node]======================

element =><@tagName_{attribute}*_/>;

element =><@tagName_{attribute}*_>{node}*</@tagName>;

#left: <@tagName_{attribute}*_>
#  ;

#right: _
#</@tagName>;

separator: '\n'

====[/node]=====================

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

embeddedElement => {element};

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



