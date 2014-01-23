/*! http://mths.be/details v0.1.0 by @mathias | includes http://mths.be/noselect v1.0.3 */
;(function(document, $) {

    var proto = $.fn,
            details,
            // :'(
            isOpera = Object.prototype.toString.call(window.opera) == '[object Opera]',
    getDetailsNotSummary = function($details) {
	return $details.children(':not(summary)');
    },
    closeIt = function($details) {
	$detailsNotSummary = getDetailsNotSummary($details);
	$detailsSummary = $('summary', $details).first();
	$details.removeClass('open').triggerHandler('close.details');
	$detailsSummary.attr('aria-expanded', false);
	$detailsNotSummary.hide();
    },
    openIt = function($details) {
	$detailsNotSummary = getDetailsNotSummary($details);
	$detailsSummary = $('summary', $details).first();
	$details.addClass('open').triggerHandler('open.details');
	$detailsSummary.attr('aria-expanded', true);
	$detailsNotSummary.show();
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
				closeIt($details);

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

/*			      Object.defineProperty(this, 'open2', {
				  configurable: true,
				  enumerable: true,
				  set: function(value) {
				    internalOpen = value;
				    if (value) openIt($details);
				    else closeIt($details);
				  },
				  get: function() {
				    return internalOpen;
				  }
			      });
*/


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
        if (name == "open") {
	    var needToOpen = value != null;
	    if (needToOpen) openIt($details);
	    else closeIt($details);
        }
        this.__proto__.setAttribute.call(this, name, value); 
      }

      this.removeAttribute = function(name) { 
        if (name == "open") closeIt($details);
        this.__proto__.removeAttribute.call(this, name); 
      }


			});

		};



}(document, jQuery));