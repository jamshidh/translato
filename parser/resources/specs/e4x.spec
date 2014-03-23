
e4xFile =>
{e4xElement}
;

====[e4xAttribute]===============
e4xAttribute =>@name([a-zA-Z\-]+)(="@value([^"]*)")?;
e4xAttribute =>@name([a-zA-Z\-]+)(='@value([^']*)')?;

separator: ' '
====[/e4xAttribute]==============

====[e4xNode]======================

e4xElement =><@e4xTagName_{e4xAttribute}*_/>;

e4xElement =>
<@e4xTagName_{e4xAttribute}*_>
  {e4xNode}*
</@e4xTagName>;


e4xText=>{word}+;

separator: '\n'

====[/e4xNode]=====================
