
config =>

{browsers}?

;

browsers=>browsers: {browser}+;

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
lowerBound=> >= {version};
upperBound=> <= {version};
range=> {lowerBound} {upperBound};
range=> {upperBound} {lowerBound};

====[/versionRange]==============

