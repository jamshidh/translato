
//TODO- It is sloppy to keep all this stuff in the global namespace....  Fix this up when you get a chance.

var global_target;
var global_isPM = false;

function hideTelInput() {
    timeInput.style.display="none";
}

function showTimeInput(target) {
    global_target = target;
    var rect = getElementRectangle(target);

    var parsedTime = global_target.value.match(/(\d+):(\d+):(\d+)(AM|PM)/i);

    if (parsedTime !== null) {
	hours.innerHTML = parsedTime[1];
	minutes.innerHTML = parsedTime[2];
	seconds.innerHTML = parsedTime[3]; 
	if (parsedTime[4].toUpperCase() == "PM") setPM();
	else setAM();
    }
    else {
	hours.innerHTML = "12";
	minutes.innerHTML = "00";
	seconds.innerHTML = "00";
	setPM();
    }
	
    timeInput.style.left = scrollX + rect.left + 10 + "px";
    timeInput.style.top = scrollY + rect.bottom + "px";
    timeInput.style.display="block";
}

function makeTimeControlAppear(target) {
    setTimeout(function () {
	showTimeInput(target);
    });
}

function checkOnBlur() {
    setTimeout(function () {
	var node = document.activeElement;
	while(node) {
	    if (node == timeInput) { 
	        global_target.focus();
	        return;
	    }
	    node = node.parentElement;
	}
	
	
	hideTelInput();
	
    });
}

function onPress(val) {
    global_target.value += val;
}


function putTimeInInput() {
    global_target.value = 
	hours.innerHTML.replace(/\s/g,'') + ":" +
	minutes.innerHTML.replace(/\s/g,'') + ":" +
	seconds.innerHTML.replace(/\s/g,'') + (global_isPM?"PM":"AM");
}


function setAM() {
    amButton.className = "selectedAMPM";
    pmButton.className = "notselectedAMPM";
    global_isPM = false;
    putTimeInInput();
}

function setPM() {
    amButton.className = "notselectedAMPM";
    pmButton.className = "selectedAMPM";
    global_isPM = true;
    putTimeInInput();
}

function flipAMPM() {
    if (global_isPM) setAM();
    else setPM();
}

function addHour() {
    var newVal = parseInt(hours.innerHTML) + 1;

    if (newVal == 12) {
	flipAMPM();
    }

    if (newVal > 12) {
	newVal=1;
    }

    hours.innerHTML = (newVal<10?"0":"") + newVal;

    putTimeInInput();
}

function subtractHour() {
    var newVal = parseInt(hours.innerHTML) - 1;
    
    if (newVal == 11) {
	flipAMPM();
    }

    if (newVal < 1) {
	newVal=12;
    }

    hours.innerHTML = (newVal<10?"0":"") + newVal;

    putTimeInInput();
}

function addMinute() {
    var newVal = parseInt(minutes.innerHTML) + 1;

    if (newVal >= 60) {
	addHour();
	newVal=0;
    }

    minutes.innerHTML = (newVal<10?"0":"") + newVal;

    putTimeInInput();
}

function subtractMinute() {
    var newVal = parseInt(minutes.innerHTML) - 1;

    if (newVal < 0) {
	subtractHour();
	newVal=59;
    }

    minutes.innerHTML = (newVal<10?"0":"") + newVal;

    putTimeInInput();
}

function addSecond() {
    var newVal = parseInt(seconds.innerHTML) + 1;

    if (newVal >= 60) {
	addMinute();
	newVal=0;
    }

    seconds.innerHTML = (newVal<10?"0":"") + newVal;

    putTimeInInput();
}

function subtractSecond() {
    var newVal = parseInt(seconds.innerHTML) - 1;

    if (newVal < 0) {
	subtractMinute();
	newVal=59;
    }

    seconds.innerHTML = (newVal<10?"0":"") + newVal;

    putTimeInInput();
}

