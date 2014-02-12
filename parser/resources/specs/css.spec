
cssFile =>

{declarationBlock}*

;


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


