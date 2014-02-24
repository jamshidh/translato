
file =>

{element}
;

====[attribute]===============
attribute =>@name([a-zA-Z\-]+)(="@value([^"]*)")?;
attribute =>@name([a-zA-Z\-]+)(='@value([^']*)')?;

separator: ' '
====[/attribute]==============

====[parameter]===============

parameter =>@value;

separator: '_, '

====[/parameter]==============


====[node]======================

element =><@tagName_{attribute}*_/>;

element =>
<@tagName_{attribute}*_>
  {node}*
</@tagName>;


text=>{word}+;

separator: '\n'

====[/node]=====================
