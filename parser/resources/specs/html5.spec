
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
<body>
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

button=>
<button_{attribute}*_>{bodyNode}*</button>
;

code=>
<code>{bodyNode}*</code>
;

details=>
<details_{attribute}*_>
  {summary}
  {bodyNode}*
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
<h1_{attribute}*_>{bodyNode}*</h1>
;

progress=>
<progress_{attribute}*_>{bodyNode}*</progress>
;

section=>
<section_{attribute}*_>
  {bodyNode}*
</section>
;

separator: '\n'
====[/widget]==============

dt=>
<dt>{bodyNode}*</dt>
;

dd=>
<dd>{bodyNode}*</dd>
;


summary=>
<summary_{attribute}*_>
  {bodyNode}*
</summary>
;




doctype => <!DOCTYPE @value>;

====[attribute]===============
attribute =>@name([a-zA-Z\-]+)(="@value([^"]*)")?;

#booleanAttribute => @name([a-zA-Z\-]+);

separator: ' '
====[/attribute]==============


====[script]======================

script =><script_>
  {command}*
</script_>;

script =><script src="@src([^"]*)">_</script>;

====[/script]=====================

