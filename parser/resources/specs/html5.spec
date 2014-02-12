
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
  {widget}*
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

