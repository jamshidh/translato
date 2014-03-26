
e4xFile =>
{e4xElement}
;

====<subgrammars>===============
<{js}>
====</subgrammars>==============


====[e4xAttribute]===============
e4xAttribute =>@name([a-zA-Z\-]+)(="@value([^"]*)")?;
e4xAttribute =>@name([a-zA-Z\-]+)(='@value([^']*)')?;

e4xExprAttr=>@name([a-zA-Z\-]+)=\{{expression}\};

separator: ' '
====[/e4xAttribute]==============

====[e4xNode]======================

e4xElement =><@e4xTagName_{e4xAttribute}*_/>;

e4xElement =>
<@e4xTagName_{e4xAttribute}*_>
  {e4xNode}*
</@e4xTagName>;


e4xText=>{word}+;

e4xExprNode=>\{{expression}\};

separator: '\n'

====[/e4xNode]=====================
