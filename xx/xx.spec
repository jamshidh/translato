
xxFile=>
{xform}+
;

====<subgrammars>======

<{e4x}>

====</subgrammars>=====

====[xform]====

xform=>{xpath}
-----*
{xxCommand}+;

separator: '=====+'

====[/xform]===

====[xpath]====

tagname=>{word};

operators: '/'

====[/xpath]===

====[xxCommand]====

mv=>mv {location} {xpath} {xpath};
rm=>rm {xpath};
mkelem=>mkelem {location} {xpath} {xxelem};

====[/xxCommand]===

====[location]========
before=>before;
after=>after;
first=>first;
last=>last;
====[/location]=======


name=>"{word}";




====[xxelem]====

emptyElem=>{name};

complexElem=>{e4xElement};

====[/xxelem]===
