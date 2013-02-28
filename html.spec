
file => {element} ;

attribute =>@name="@value{eIdent}";

script =>\<script<ws>\> {list(command, ' ')} \</script\>;

element =>
	\<@tagName {list(attribute, ' ')} \>
	{list(element|text|script, ' ')}
	\</@tagName\>;

element =>
	\<@tagName {list(attribute, ' ')} /\>;

text =>@value{eIdent};

command = {commandSc|commandNoSc|comment|if|try};

commandSc = {assignment|expression|varDeclaration}<ws>\;;

commandNoSc = {funcDeclaration};

comment => //@value{ident}
;

varDeclaration => var @name;

varDeclaration => var @name = {expression};

funcDeclaration => function @name() {body};

if => if () {body};

if => if () {body} {else};

else => else {body};

try => try {body}
catch() {body};

assignment => {lValue} = {expression} ;

lValue => {methodChain};

function => @name{ident}({list(expression, ',')});

lambda => function () \{ \};

body => \{
{list(command, '<ws>')}
\};

methodChain => {list(function|variable, '.')};

variable => @name{ident};

expression = {string|methodChain|num|variable|lambda};

expression:operators => '+' '*';

num => @value{number};

string => "@value{list(anyCharBut('"'), '')}";




