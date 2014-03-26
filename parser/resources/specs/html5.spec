
htmlFile =>
{doctype}?

{html}
;

====<subgrammars>=======
<{js}>
<{css}>
====</subgrammars>======


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
<body_{attribute}*_>
  {bodyNode}*
</body>
;

====[headElements:script]========

title=>
<title>{word}*</title>
;

meta=><meta http-equiv="@httpequiv([^"]*)" content="@content([^"]*)"_/?>;

link=><link rel="@rel([^"]*)" type="@type([^"]*)" href="@href([^"]*)"_/?>;

#area, base, br, col, embed, hr, img, input, keygen, param, source, track, wbr

style=>
<style>
  {declarationBlock}*
</style>
;

separator: '\n'
====[/headElements]==============

====[bodyNode:widget]========

text => {word}+;

separator: '\n'

====[/bodyNode]==============


====[widget:script]========

audio=>
<audio_{attribute}*_/>
;

button=>
<button_{attribute}*_>{bodyNode}*</button>
;

br=><br_>;
br=><br_/>;

code=>
<code>{bodyNode}*</code>
;

datalist=>
<datalist_{attribute}*_>
  {option}*
</datalist>
;

form=>
<form_{attribute}*_>
  {bodyNode}*
</form>
;

img=>
<img_{attribute}*_/>
;

input=>
<input_{attribute}*_/>
;

strong=>
<strong>{bodyNode}*</strong>
;

details=>
<details_{attribute}*_>
  {summary}
  {bodyNode}*
</details>
;

table=>
<table_{attribute}*_/>;

table=>
<table_{attribute}*_>
  {tr}*
</table>
;

dl=>
<dl>
  ({dt} 
  {dd}
  )+
</dl>
;

h1=>
<h1_{attribute}*_>{bodyNode}*</h1>
;

label=>
<label_{attribute}*_>
  {bodyNode}*
</label>
;

ol=>
<ol_{attribute}*_>
  {li}* 
</ol>
;

p=>
<p>{bodyNode}*</p>
;

progress=>
<progress_{attribute}*_>_{bodyNode}*_</progress>
;

section=>
<section_{attribute}*_>
  {bodyNode}*
</section>
;

ul=>
<ul_{attribute}*_>
  {li}* 
</ul>
;

video=>
<video_{attribute}*_/>
;

separator: '\n'
====[/widget]==============

dt=>
<dt>{bodyNode}*</dt>
;

dd=>
<dd>{bodyNode}*</dd>
;

li=>
<li>{bodyNode}*</li>
;

====[option]====

option=>
<option_{attribute}*_/>
;

separator: '\n'

====[/option]===

tr=>
<tr>
  {td}*
</tr>
;

td=>
<td_{attribute}*_>
  {bodyNode}*
</td>
;

summary=>
<summary_{attribute}*_>
  {bodyNode}*
</summary>
;




doctype => <!DOCTYPE @value>;

====[attributeCommand:command]===========
#Same as (javascript) commands, but all smashed on one line.
left: '_'
right: '_'
separator: ' '
====[/attributeCommand]==================

jsCommands => {attributeCommand}*;



====[jsQuote]===============
jsDQuote=>"[^"]*";
jsSQuote=>'[^']*';
====[/jsQuote]==============

====[attribute]===============
attribute =>@name([a-zA-Z\-][a-zA-Z0-9\-]*)(="@value([^"]*)")?;

onclick=>onclick_={jsQuote:jsCommands}*;

#booleanAttribute => @name([a-zA-Z\-]+);

separator: ' '
====[/attribute]==============


====[script]======================

script =><script_>{command}*</script_>;

script =><script src="@src([^"]*)">_</script>;

====[/script]=====================

