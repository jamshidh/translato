
file => {definition}*
;

ident => [a-zA-Z][0-9a-zA-Z]*;

====[definition]===========
var =>@type @name_\;;
function =>@returnType @name_\({param}*\) \{
  {command}*
\};
separator: '\n\n'
====[/definition]==========

====[param]=================
param =>@type @name;
separator: ', '
====[/param]================

====[command:expression]====
assignment=>{lvalue}={expression};
separator: '\n'
====[/command]==============

====[expression]============
functionCall=>@name\({expression}*\);
variable=>@name;
====[/expression]===========

lvalue=>{variable};



