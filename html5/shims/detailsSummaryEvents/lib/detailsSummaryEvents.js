var openEvt = document.createEvent("Event");
openEvt.initEvent("open",true,false);
var closeEvt = document.createEvent("Event");
closeEvt.initEvent("close",true,false);

window.onload = function() {
 
    var detailsElements = document.getElementsByTagName("details")
    
    for(var i = 0; i < detailsElements.length; i++) {
	
	el = detailsElements[i];
	
	Object.defineProperty(detailsElements[i], "open", {
	    set: function(value) {
		if (value) this.setAttribute("open", "");
		else this.removeAttribute("open");
	    },
	    get: function() {
		if (this.getAttribute("open") == undefined) return false;
		else return true;
	    }
	});
	
	el.setAttribute = function(name, value) { 
	    if (name == "open") el.dispatchEvent(openEvt);
	    Object.getPrototypeOf(el).setAttribute.call(el, name, value); 
	}
	
	el.removeAttribute = function(name) { 
	    el.dispatchEvent(closeEvt);
	    Object.getPrototypeOf(el).removeAttribute.call(el, name); 
	}
	
	el.addEventListener("click", function () { 
	    if (this.open) el.dispatchEvent(closeEvt);
	    else el.dispatchEvent(openEvt);
	});
	el.addEventListener("open", function () { eval(el.getAttribute("onopen")) });
	el.addEventListener("close", function () { eval(el.getAttribute("onclose")) });
	
    }

}
