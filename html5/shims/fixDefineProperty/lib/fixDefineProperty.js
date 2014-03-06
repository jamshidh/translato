

Object.fixedDefineProperty = function (obj, prop, descriptor) {

    var theRealValue = obj.getAttribute(prop);
    
    var originalSetAttribute = obj.setAttribute;
    var originalGetAttribute = obj.getAttribute;
    var originalRemoveAttribute = obj.removeAttribute;
    
    obj.setAttribute = function (name, value) {
	if (name === prop) theRealValue = value;
	else originalSetAttribute(name, value);
    };
    
    obj.getAttribute = function (name) {
	if (name === prop) return theRealValue;
	else return originalGetAttribute(name, value);
    };
    
    obj.removeAttribute = function (name) {
	if (name === prop) theRealValue = undefined;
	else originalRemoveAttribute(name);
    };
    
    Object.defineProperty(obj, prop, descriptor);
}


