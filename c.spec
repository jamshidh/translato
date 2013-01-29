
definitions => {list(var|function)};

var => 
@returnType @name<ws> \; ;

function =>
@returnType @name({list(param)}) \{
\}
;

param =><ws>@type @name<ws>;
