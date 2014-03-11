
whitespace:/\*.*\*/;

cssFile =>

{declarationBlock}*

;


====[declarationBlock]===============
declarationBlock=>
{selector}* 
\{ 
  {declaration}* 
\}
;

separator: '\n'
====[/declarationBlock]==============



====[declaration]===============

#TODO= get rid of the moz, webkit

background=>background: @value([^;]*)\;;
backgroundColor=>background-color: @value([^;]*)\;;
backgroundImage=>background-image: url\('@value([^']*)'\)\;;
backgroundPosition=>background-position: @value([^;]*)\;;
backgroundRepeat=>background-repeat: @value([^;]*)\;;

backgroundSize=>background-size: @value([^;]*)\;;
mozBackgroundSize=>-moz-background-size: @value([^;]*)\;;
webkitBackgroundSize=>-webkit-background-size: @value([^;]*)\;;

border=>border: @value([^;]*)\;;

mozBoxSizing=>-moz-box-sizing: @value([^;]*)\;;
boxSizing=>box-sizing: @value([^;]*)\;;

color=>color: @value([^;]*)\;;
content=>content: @value([^;]*)\;;
display=>display: @value([^;]*)\;;
height=>height: @value([^;]*)\;;
position=>position: @value([^;]*)\;;
transition=>transition: @value([^;]*)\;;
verticalAlign=>vertical-align: @value([^;]*)\;;
width=>width: @value([^;]*)\;;



top=>top: @value([^;]*)\;;
right=>right: @value([^;]*)\;;
bottom=>bottom: @value([^;]*)\;;
left=>left: @value([^;]*)\;;







separator: '\n'
====[/declaration]==============


====[selector]===============
tagname=>@value([a-zA-Z0-9\-]+);

filter=>{selector}\[{qualifier}\];

fullSelector=>{selector}:{pseudoElement};

operators: ' > '

separator: '_, '

====[/selector]==============

====[pseudoElement]==============

pseudoElement=>before;
pseudoElement=>after;
pseudoElement=>link;
pseudoElement=>visited;
pseudoElement=>active;
pseudoElement=>hover;
pseudoElement=>focus;
pseudoElement=>first-letter;
pseudoElement=>first-line;
pseudoElement=>first-child;
pseudoElement=>before;
pseudoElement=>after;
pseudoElement=>lang(@value);
pseudoElement=>first-of-type;
pseudoElement=>last-of-type;
pseudoElement=>only-of-type;
pseudoElement=>only-child;
pseudoElement=>nth-child(@value);
pseudoElement=>nth-last-child(@value);
pseudoElement=>nth-of-type(@value);
pseudoElement=>nth-last-of-type(@value);
pseudoElement=>last-child;
pseudoElement=>root;
pseudoElement=>empty;
pseudoElement=>target;
pseudoElement=>enabled;
pseudoElement=>disabled;
pseudoElement=>checked;
pseudoElement=>not({selector});

====[/pseutoElement]=============




====[qualifier]===============
hasAtt=>@value([a-zA-Z0-9\-]+);
====[/qualifier]==============









# .class	.intro	Selects all elements with class="intro"	1
# #id	#firstname	Selects the element with id="firstname"	1
# *	*	Selects all elements	2
# element	p	Selects all <p> elements	1
# element,element	div,p	Selects all <div> elements and all <p> elements	1
# element element	div p	Selects all <p> elements inside <div> elements	1
# element>element	div>p	Selects all <p> elements where the parent is a <div> element	2
# element+element	div+p	Selects all <p> elements that are placed immediately after <div> elements	2
# [attribute]	[target]	Selects all elements with a target attribute	2
# [attribute=value]	[target=_blank]	Selects all elements with target="_blank"	2
# [attribute~=value]	[title~=flower]	Selects all elements with a title attribute containing the word "flower"	2
# [attribute|=value]	[lang|=en]	Selects all elements with a lang attribute value starting with "en"	2
# element1~element2	p~ul	Selects every <ul> element that are preceded by a <p> element	3
# [attribute^=value]	a[src^="https"]	Selects every <a> element whose src attribute value begins with "https"	3
# [attribute$=value]	a[src$=".pdf"]	Selects every <a> element whose src attribute value ends with ".pdf"	3
# [attribute*=value]	a[src*="w3schools"]	Selects every <a> element whose src attribute value contains the substring "w3schools"	3
# ::selection	::selection	Selects the portion of an element that is selected by a user

