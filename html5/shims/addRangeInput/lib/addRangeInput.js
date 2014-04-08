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
    
}

function placeThisGripper(theId) {
    global_gripper = document.getElementById(theId+"_gripper");
    global_bar = document.getElementById(theId+"_bar");
    global_input = document.getElementById(theId);
    placeGripper();
}

function placeGripper() {

    global_bar.style.width = global_input.offsetWidth;
    global_input.style.display = "none";
    
    barRect = getRect(global_bar);
    gripperRect = getRect(global_gripper)
    
    gripperHeight = gripperRect.bottom - gripperRect.top;
    gripperWidth = gripperRect.right - gripperRect.left;
    
    barMiddleY = (barRect.top + barRect.bottom)/2;
    
    
    global_gripper.style.top = (barMiddleY - 0.8*gripperHeight/2.0) + "px";
    
    global_gripper.style.left = (global_input.value - global_input.min)/(global_input.max - global_input.min) * (barRect.right - barRect.left) + barRect.left - gripperWidth/2 + "px";
    
    
    
}
function mouseUp() {
    document.onmousemove = null;
}

