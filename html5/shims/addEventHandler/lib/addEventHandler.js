
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
	type === "focus" ||
	type === "blur" ||
	type === "keyup" ||
	type === "input" ||
	type === "load") return false;

    return true;
}

Element.prototype.addEventListener = function (type, listener, useCapture) {
    //TODO- figure out how to deal with useCapture
    if (isCustomEvent(type)) {
	this[type] = 0;

	this.attachEvent("onpropertychange", function(event) {
            if (event.propertyName == type + "eventhack") {
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
    if (isCustomEvent(event.type)) {
	this[event.type+"eventhack"]++;
    }
    else {
	this.fireEvent("on" + event.type, event);
    }
}

document.createEvent = function (type) {
    return {
	initEvent: function(type, bubbles, cancelable) { 
	    this.type=type;
	} 
    };
}
