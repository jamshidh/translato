/*! http://mths.be/details v0.1.0 by @mathias | includes http://mths.be/noselect v1.0.3 */
;(function(document, $) {
    
    var openEvt = document.createEvent("Event");
    openEvt.initEvent("open.details",true,true);
    var closeEvt = document.createEvent("Event");
    closeEvt.initEvent("close.details",true,true);
    
    
    var proto = $.fn,
    details,
    // :'(
    isOpera = Object.prototype.toString.call(window.opera) == '[object Opera]',
    getDetailsNotSummary = function($details) {
	return $details.children(':not(summary)');
    },
    closeIt = function($details, el) {
	$detailsNotSummary = getDetailsNotSummary($details);
	$detailsSummary = $('summary', $details).first();
	$details.removeClass('open');
	$detailsSummary.attr('aria-expanded', false);
	$detailsNotSummary.hide();
	el.dispatchEvent(closeEvt);
    },
    openIt = function($details, el) {
	$detailsNotSummary = getDetailsNotSummary($details);
	$detailsSummary = $('summary', $details).first();
	$details.addClass('open');
	$detailsSummary.attr('aria-expanded', true);
	$detailsNotSummary.show();
	el.dispatchEvent(openEvt);
    };
    
    /* http://mths.be/noselect v1.0.3 */
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
	    
	    
	    Object.defineProperty(this, "open", {
		configurable: true,
		set: function(value) {
		    if (value) this.setAttribute("open", "");
		    else this.removeAttribute("open");
		},
		get: function() {
		    if (this.getAttribute("open") == undefined) return false;
		    else return true;
		}
	    });
	    
	    this.setAttribute = function(name, value) { 
		this.__proto__.setAttribute.call(this, name, value); 
		if (name == "open") {
		    var needToOpen = value != null;
		    if (needToOpen) openIt($details, this);
		    else closeIt($details);
		}
	    }
	    
	    this.removeAttribute = function(name) { 
		this.__proto__.removeAttribute.call(this, name); 
		if (name == "open") closeIt($details, this);
	    }

	    this.addEventListener("open.details", function () { eval(this.getAttribute("onopen.details")) });
	    this.addEventListener("close.details", function () { eval(this.getAttribute("onclose.details")) });
	    
	});
	
    };
    


}(document, jQuery));