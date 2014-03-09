

Object.fixedDefineProperty = function (obj, prop, descriptor) {

    var theRealValue = obj.getAttribute(prop);
    
    var originalSetAttribute = obj.setAttribute;
    var originalGetAttribute = obj.getAttribute;
    var originalRemoveAttribute = obj.removeAttribute;
    
    //This fixes a nasty bug in IE8....
    //When defineProperty is called, setAttribute is broken.  Although we store the
    //value of the attribute locally in this closure, css changes are not applied.
    //This function undoes the defineProperty to set the value, then reapplies the defineProperty.
    //Note that this trick doesn't seem to work with getAttribute (else we wouldn't need the closure at all),
    //because defineProperty/delete property resets the value of the property to null.
    function setRealValue(obj, prop, value) {
	var descriptor = Object.getOwnPropertyDescriptor(obj, prop);
	delete obj[prop]; 
	originalSetAttribute(prop, value);
	obj.className=obj.className;
	//Without the "setTimeout", the style changes don't apply correctly.
	//I can guess why this is so, but honestly, this was just found through playing around,
	//and I don't really *know* why this is so.  Note that anything that break up the thread control,
	//like an alert, or putting the code outside of this function seems to work.
	setTimeout(function() {Object.defineProperty(obj, prop, descriptor);}, 0);
    }

    obj.setAttribute = function (name, value) {
	if (name === prop) {
	    theRealValue = value;
	    setRealValue(this, name, value);
	}
	else originalSetAttribute(name, value);
	obj.className=obj.className;
    };
    
    obj.getAttribute = function (name) {
	if (name === prop) return theRealValue;
	else return originalGetAttribute(name);
    };
    
    obj.removeAttribute = function (name) {
	if (name === prop) theRealValue = undefined;
	else originalRemoveAttribute(name);
    };
    
    Object.defineProperty(obj, prop, descriptor);
}


