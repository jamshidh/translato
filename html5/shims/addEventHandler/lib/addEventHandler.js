
Object.getPrototypeOf = function(obj) {
    return obj.constructor.prototype;
};

//modified from https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter#Compatibility
Array.prototype.filter = function(fun /*, thisArg */)
{
    var res = [];
    var thisArg = arguments.length >= 2 ? arguments[1] : void 0;
    for (var i = 0; i < this.length; i++)
    {
	if (i in this)
	{
            var val = this[i];
            if (fun.call(thisArg, val, i, this))
		res.push(val);
	}
    }
    
    return res;
};

function isCustomEvent(type) {
    if (type === "click" ||
	type === "load") return false;

    return true;
}

Element.prototype.addEventListener = function (type, listener, useCapture) {
    //TODO- figure out how to deal with useCapture
    if (isCustomEvent(type)) {
	this[type] = 0;

	this.attachEvent("onpropertychange", function(event) {
            if (event.propertyName == type) {
		listener(event);
            }
	});
    }
    else {
	this.attachEvent("on" + type, listener);
    }

}

window.addEventListener = Element.prototype.addEventListener;

//window.removeEventListener = function() {
//this.detachEvent("onpropertychange", arguments.callee);
//}

Element.prototype.dispatchEvent = function (event) {
    //alert("dispatchEvent: event = " + event);
    if (isCustomEvent(event.type)) {
	this[event.type]++;
    }
    else {
	this.fireEvent("on" + event.type, event);
    }
}

document.createEvent = function (type) {
    //alert("createEvent: type = " + type);
    return {"qqqq": "qqqq", 
	    initEvent: function(type, bubbles, cancelable) { 
		//alert("initEvent: type=" + type + ", bubbles=" + bubbles + ", cancelable=" + cancelable); 
		//alert("creating: " + type);
		return {qqqq:"qqqq"};
	    } 
	   };
}
