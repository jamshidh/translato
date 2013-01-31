
definitions =><ws>{list(var|function, '<ws>')}<ws>;

var =>@returnType @name<ws>\;

;

function =>@returnType @name<ws>({list(param, '<ws>, ')}) \{
  {list(command, '<ws>')}
\};

param =>@type @name;

command=>{assignment|expression}<ws>\;;

assignment=>{lvalue}={expression};

lvalue=>{ident};

expression=>{ident|functionCall};

functionCall=>@name({list(ident, ',')});

