
file =>{processinginstruction} {element} ;

processinginstruction =>\<?@tagName {list(attribute, ' ')} ?\>;

attribute =>@name="@value{eIdent}";

element =>{emptyElement|fullElement};

fullElement =>
	\<@tagName {list(attribute, ' ')} \>
	{list(element, ' ')}
	\</@tagName\>;

emptyElement =>
	\<@tagName {list(attribute, ' ')} /\>;
