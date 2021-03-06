
whitespace:/\*.*\*/;
whitespace://.*[\n];

cssFile =>

{declarationBlock}*

;


====[declarationBlock]===============
declarationBlock=>
{selector}* \{ 
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
borderCollapse=>border-collapse: @value([^;]*)\;;
borderRadius=>border-radius: @value([^;]*)\;;
borderRight=>border-right: @value([^;]*)\;;
borderSpacing=>border-spacing: @value([^;]*)\;;
borderStyle=>border-style: @value([^;]*)\;;

boxOrient=>box-orient: @value([^;]*)\;;
mozBoxOrient=>-moz-box-orient: @value([^;]*)\;;
webkitBoxOrient=>-webkit-box-orient: @value([^;]*)\;;

boxShadow=>box-shadow: @value([^;]*)\;;

mozBoxSizing=>-moz-box-sizing: @value([^;]*)\;;
boxSizing=>box-sizing: @value([^;]*)\;;

color=>color: @value([^;]*)\;;
content=>content: @value([^;]*)\;;
cursor=>cursor: @value([^;]*)\;;
display=>display: @value([^;]*)\;;
emptyCells=>empty-cells: @value([^;]*)\;;
float=>float: @value([^;]*)\;;
font=>font: @value([^;]*)\;;
fontFamily=>font-family: @value([^;]*)\;;
fontSize=>font-size: @value([^;]*)\;;
fontStyle=>font-style: @value([^;]*)\;;
fontWeight=>font-weight: @value([^;]*)\;;
height=>height: @value([^;]*)\;;
listStyle=>list-style: @value([^;]*)\;;
margin=>margin: @value([^;]*)\;;
marginBottom=>margin-bottom: @value([^;]*)\;;
marginLeft=>margin-left: @value([^;]*)\;;
marginRight=>margin-right: @value([^;]*)\;;
marginTop=>margin-top: @value([^;]*)\;;
maxHeight=>max-height: @value([^;]*)\;;
maxWidth=>max-width: @value([^;]*)\;;
overflow=>overflow: @value([^;]*)\;;
overflowY=>overflow-y: @value([^;]*)\;;
padding=>padding: @value([^;]*)\;;
position=>position: @value([^;]*)\;;

select=>select: @value([^;]*)\;;
mozUserSelect=>-moz-user-select: @value([^;]*)\;;

textAlign=>text-align: @value([^;]*)\;;
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
tagname=>@value([a-zA-Z0-9_\-]+)(\.@className([a-zA-Z0-9_\-]+))?;

classname=>\.@className([a-zA-Z0-9_\-]+);

idname=>#@className([a-zA-Z0-9_\-]+);

filter=>{selector}\[{qualifier}\];

fullSelector=>{selector}:{pseudoElement};

operators: ' ' ' > '

separator: '_, '

====[/selector]==============

====[pseudoElement]==============

peBefore=>before;
peAfter=>after;
peLink=>link;
peVisited=>visited;
peActive=>active;
peHover=>hover;
peFocus=>focus;
peFirstLetter=>first-letter;
peFirstLine=>first-line;
peFirstChild=>first-child;
peBefore=>before;
peAfter=>after;
peLang=>lang(@value);
peFirstOfType=>first-of-type;
peLastOfType=>last-of-type;
peOnlyOfType=>only-of-type;
peOnlyChild=>only-child;
peNthChild=>nth-child(@value);
peNthLastChild=>nth-last-child(@value);
peNthOfType=>nth-of-type(@value);
peNthLastOfType=>nth-last-of-type(@value);
peLastChild=>last-child;
peRoot=>root;
peEmpty=>empty;
peTarget=>target;
peEnabled=>enabled;
peDisabled=>disabled;
peChecked=>checked;
peNot=>not({selector});

====[/pseudoElement]=============




====[qualifier]===============
cssString=>'@value';
hasAtt=>@value([a-zA-Z0-9\-]+);
operators: '='
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

