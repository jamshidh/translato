var global_gripper;
var global_bar;
var global_input;

function getRect(target) {
    var rect = target.getBoundingClientRect();
    
    //TODO- standardize this and put it in a shim
    var scrollX = (window.pageXOffset !== undefined) ? window.pageXOffset : (document.documentElement || document.body.parentNode || document.body).scrollLeft;
    var scrollY = (window.pageYOffset !== undefined) ? window.pageYOffset : (document.documentElement || document.body.parentNode || document.body).scrollTop;
    
    rect.left += scrollX;
    rect.right += scrollX;
    rect.top += scrollY;
    rect.bottom += scrollY;
    
    return rect;
}

function valOrDefault(x, def) {
    if (!x) return def;
    return x;
}

function mouseDown(theId, event) {
    document.onmousemove = mouseMove;
    document.onmouseup = mouseUp;
    global_gripper = document.getElementById(theId+"_gripper");
    global_bar = document.getElementById(theId+"_bar");
    global_input = document.getElementById(theId);
}
function barPress(theId, event) {
    global_gripper = document.getElementById(theId+"_gripper");
    global_bar = document.getElementById(theId+"_bar");
    global_input = document.getElementById(theId);
    mouseMove(event);
}
function mouseMove(e) {
    var min = parseFloat(global_input.min);
    var max = parseFloat(global_input.max);
    var step = parseFloat(valOrDefault(global_input.step, "1"));
    
    var event = e || window.event;
    barRect = getRect(global_bar);
    
    global_input.value = (max - min) * (event.clientX - barRect.left)/(barRect.right - barRect.left) + min;
    
    if (global_input.value < min) global_input.value = min;
    if (global_input.value > max) global_input.value = max;
    
    global_input.value = Math.round(global_input.value/step)*step;
    
    placeGripper();

    var e = document.createEvent('HTMLEvents');
    e.initEvent('change', false, false);
    global_input.dispatchEvent(e);
    
}

function placeThisGripper(theId) {
    global_gripper = document.getElementById(theId+"_gripper");
    global_bar = document.getElementById(theId+"_bar");
    global_input = document.getElementById(theId);
    placeGripper();
}

function placeGripper() {

    placeGripperUsingItems(global_input, global_bar, global_gripper);
    
}

function placeGripperUsingItems(input, bar, gripper) {
    if (input.style.display !== "none") {
	bar.style.width = input.offsetWidth + "px";
	input.style.display = "none";
    }
    else if (input.style.width !== "") {
	bar.style.width = input.style.width;
    }
    
    barRect = getRect(bar);
    gripperRect = getRect(gripper)
    
    gripperHeight = gripperRect.bottom - gripperRect.top;
    gripperWidth = gripperRect.right - gripperRect.left;
    
    barMiddleY = (barRect.top + barRect.bottom)/2;
    
    
    gripper.style.top = (barMiddleY - 0.8*gripperHeight/2.0) + "px";
    
    gripper.style.left = (input.value - input.min)/(input.max - input.min) * (barRect.right - barRect.left) + barRect.left - gripperWidth/2 + "px";
    
    
    
}


function resizeAllRangeInputs() {

    //var grippers = document.getElementsByClassName("rangeInputGripper");
    var grippers = document.querySelectorAll(".rangeInputGripper");
    for(var i = 0; i < grippers.length; i++) {


	var input = document.getElementById(grippers[i].id.replace(/_gripper$/, ""));
	var bar = document.getElementById(grippers[i].id.replace(/_gripper$/, "_bar"));

	placeGripperUsingItems(input, bar, grippers[i]);

    }
}

function mouseUp() {
    document.onmousemove = null;
}

