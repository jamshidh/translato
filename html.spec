
file =>
{element} ;

attribute =>@name="@value{stringOf(anyCharBut('"'))}";

script =><script_>
  {list(command, '\n')}
</script_>;

script =><script src="@src{eIdent}">_</script>;

element =><@tagName_{list(attribute, ' ')}_>
  {list(element|text|script, '\n')}
</@tagName>;

element =><@tagName {list(attribute, ' ')} />;

text =>{reparse(list1(word, ' '), stringOf(anyCharBut('<')))};

word =>{stringOf(anyCharBut('<> \n\r\t'))};

command =     {commandSc|commandNoSc|comment|if|try};

commandSc = {assignment|expression|varDeclaration}_\;;
 
commandNoSc = {funcDeclaration};

comment => //@value{ident}
;

varDeclaration => var @name;

varDeclaration => var @name = {expression};

funcDeclaration => function @name({list(ident, ' , ')}) {body};

if => if ({expression}) {body};

if => if () {body} {else};

else => else {body};

try => try {body}
catch() {body};

assignment => {lValue} = {expression} ;

lValue => {variable};

function => @name({list(expression, ',')});

lambda => function () \{ \};

body => \{
  {list(command, '_')}
\};

variable = {function|array|label};

variable:operators => '.';

array => @name[{expression}];

label => @name{ident};

expression = {string|num|lambda|variable|element};

expression:operators => ' == ' '+' '*' '-';

num => @value{number};

string => "@value{stringOf(anyCharBut('"'))}";




