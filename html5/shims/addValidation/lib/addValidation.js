

//TODO- fix the bug that caused this to be needed, then remove this code.
function isBlank(input) {
    if (input.placeholder === undefined) {
	if (input.value === undefined) return true;
	return input.value.length == 0;
    }

    return input.value === input.placeholder;
}

function validateForm(form) {

    var inputs = form.getElementsByTagName("input");

    for(var i = 0; i < inputs.length; i++) {
	var input = inputs[i];
	if (input.required !== undefined && isBlank(input)) {
	    showError(input, "This field is required.");
	    return false;
	}

	if (!isBlank(input) && input.getAttribute('type') === "email") {
	    if (!input.value.match(/.*@.*\..*/)) {
		showError(input, "This must be a valid email.");
		return false;
	    }
	}
    }

    return true; 
}

function showError(target, message) {
    targetRect = getElementRectangle(target);
    validationWarningBubble.style.left = targetRect.left + 10 + "px";
    validationWarningBubble.style.top = targetRect.bottom + "px"
    validationWarningBubble.style.display = "block";
    validationWarningBubbleMessage.innerHTML = message;
}

function hideValidationWarning() {
    validationWarningBubble.style.display = "none";
}