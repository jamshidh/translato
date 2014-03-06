
function getDetailsNotSummary(detailsEl) {
    return detailsE.children(':not(summary)');
}

function closeIt(detailsEl) {
    $detailsNotSummary = getDetailsNotSummary($details);
    $detailsSummary = $('summary', $details).first();
    $details.removeClass('open');
    $detailsSummary.attr('aria-expanded', false);
    $detailsNotSummary.hide();
    el.dispatchEvent(closeEvt);
}

function openIt(detailsEl) {
    $detailsNotSummary = getDetailsNotSummary($details);
    $detailsSummary = $('summary', $details).first();
    $details.addClass('open');
    $detailsSummary.attr('aria-expanded', true);
    $detailsNotSummary.show();
    el.dispatchEvent(openEvt);
};


var qqqq = window.onload;

window.onload = function() {

    alert("setting up detailsSummary");

    var detailsElements = document.getElementsByTagName("details");

    for(var i = 0; i < detailsElements.length; i++) {

	var detailsEl = detailsElements[i];

	var summaryEls = detailsEl.getElementsByTagName("summary");

	if (summaryEls.length != 1) {
           //console.log("You need one summary element");
           alert("You need one summary element");
	}


	var summaryEl = summaryEls[0];

	Object.defineProperty(detailsEl, "open", {
	    enumerable: true,
	    set: function(value) {
		if (value) detailsEl.setAttribute("open", "");
		else this.removeAttribute("open");
	    },
	    get: function() {
		if (detailsEl.getAttribute("open") == undefined) return false;
		else return true;
	    }
	});
	
	this.setAttribute = function(name, value) { 
	    Object.getPrototypeOf(detailsEl).setAttribute.call(detailsEl, name, value); 
	    
	    if (name == "open") {
		var needToOpen = value != null;
		if (needToOpen) openIt($details, detailsEl);
		else closeIt($details);
	    }
	}
	
	this.removeAttribute = function(name) { 
	    Object.getPrototypeOf(detailsEl).removeAttribute.call(detailsEl, name); 
	    if (name == "open") closeIt($details, detailsEl);
	}
	
	detailsEl.addEventListener("open", function () { eval(detailsEl.getAttribute("onopen")) });
	detailsEl.addEventListener("close", function () { eval(detailsEl.getAttribute("onclose")) });
	
	summaryEl.addEventListener('click', function() {
	    //summaryEl.focus();
	    detailsEl.open = !detailsEl.open;
	})

	qqqq();

    }
	
/*
    var proto = $.fn,
    details,
    // :'(
    
    proto.noSelect = function() {
	
	// Since the string 'none' is used three times, storing it in a variable gives better results after minification
	var none = 'none';
	
	// onselectstart and ondragstart for WebKit & IE
	// onmousedown for WebKit & Opera
	return this.bind('selectstart dragstart mousedown', function() {
	    return false;
	}).css({
	    'MozUserSelect': none,
	    'msUserSelect': none,
	    'webkitUserSelect': none,
	    'userSelect': none
	});
	
    };
    
    
    details = proto.details = function() {
	
	// Loop through all `details` elements
	return this.each(function() {
	    
	    // Store a reference to the current `details` element in a variable
	    var $details = $(this),
	    // Store a reference to the `summary` element of the current `details` element (if any) in a variable
	    $detailsSummary = $('summary', $details).first(),
	    // Do the same for the info within the `details` element
	    $detailsNotSummary = $details.children(':not(summary)'),
	    // This will be used later to look for direct child text nodes
	    $detailsNotSummaryContents = $details.contents(':not(summary)'),
	    theDetailsElement = this;
	    
	    // If there is no `summary` in the current `details` element…
	    if (!$detailsSummary.length) {
		// …create one with default text
		$detailsSummary = $('<summary>').text('Details').prependTo($details);
	    }
	    
	    // Look for direct child text nodes
	    if ($detailsNotSummary.length != $detailsNotSummaryContents.length) {
		// Wrap child text nodes in a `span` element
		$detailsNotSummaryContents.filter(function() {
		    // Only keep the node in the collection if it’s a text node containing more than only whitespace
		    // http://www.whatwg.org/specs/web-apps/current-work/multipage/common-microsyntaxes.html#space-character
		    return this.nodeType == 3 && /[^ \t\n\f\r]/.test(this.data);
		}).wrap('<span>');
		// There are now no direct child text nodes anymore — they’re wrapped in `span` elements
		$detailsNotSummary = $details.children(':not(summary)');
	    }
	    
	    // Hide content unless there’s an `open` attribute
	    $details.prop('open', typeof $details.attr('open') == 'string');
	    closeIt($details, this);
	    
	    // Add `role=button` and set the `tabindex` of the `summary` element to `0` to make it keyboard accessible
	    $detailsSummary.attr('role', 'button').noSelect().prop('tabIndex', 0).on('click', function() {
		// Focus on the `summary` element
		$detailsSummary.focus();
		// Toggle the `open` and `aria-expanded` attributes and the `open` property of the `details` element and display the additional info
		theDetailsElement.open = ! theDetailsElement.open;
	    }).keyup(function(event) {
		if (32 == event.keyCode || (13 == event.keyCode && !isOpera)) {
		    // Space or Enter is pressed — trigger the `click` event on the `summary` element
		    // Opera already seems to trigger the `click` event when Enter is pressed
		    event.preventDefault();
		    $detailsSummary.click();
		}
	    });
	    
	    
    };
    
*/

}