
config =>

{configItem}*

;

====[configItem]===========
browsers=>browsers: {browser}+;
libs=>libs: {libName}*;
dependencies=>dependencies: {shimName}*;
separator: '\n'
====[/configItem]==========

====[libName]===============
lib=>@name([a-zA-Z_.]+);
separator: '_, '
====[/libName]==============

====[shimName]===============
shim=>@name;
separator: '_, '
====[/shimName]==============

====[browser]===============

ie=>ie {versionRange}?;
mozilla=>mozilla {versionRange}?;
webkit=>webkit {versionRange}?;

allBrowsers=>\*;

separator: '_, '

====[/browser]==============

====[versionPart]===============

integer=>[\d]+;

separator: '.'

====[/versionPart]==============

version=> {versionPart}+;

====[versionRange]===============

exact=> {version};
lowerBound=> {version} <=;
upperBound=> < {version};
range=> {lowerBound} {upperBound};
range=> {upperBound} {lowerBound};

====[/versionRange]==============

