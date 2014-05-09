
xxFile=>
{xform}+
;

====<subgrammars>======

<{xml}>

====</subgrammars>=====

====[xform]====

xform=>{xpath}
-----*
{command}+;

separator: '=====+'

====[/xform]===

====[xpath]====

tagname=>{word};

operators: '/'

====[/xpath]===

====[command]====

mv=>mv {location} {xpath} {xpath};
rm=>rm {xpath};
mkelem=>mkelem {location} {xpath} {xxelem};

====[/command]===

====[location]========
before=>before;
after=>after;
first=>first;
last=>last;
====[/location]=======


name=>"{word}";




====[xxelem]====

emptyElem=>{name};

complexElem=>{element};

====[/xxelem]===
