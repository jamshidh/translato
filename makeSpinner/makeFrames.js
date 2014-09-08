

function p(coord) {
    return coord.x+","+coord.y;
}

function makeArc(r1, r2, a, fo) {
    var p1={x:50, y:50-r2};
    var p2={x:50+r2*Math.sin(a), y:50-r2*Math.cos(a)};
    var p3={x:50+r1*Math.sin(a), y:50-r1*Math.cos(a)};
    var p4={x:50, y:50-r1};
    return "<path style='fill-opacity:" + fo + "' d='M " + p(p1) + " A " + r2 + "," + r2 + " 0 0 1 " + p(p2) +" L " + p(p3) + " A " + r1 + "," + r1 + " 0 0 0 " + p(p4) + " L " + p(p1) + "' />";
}

function makeDoubleArc(r1, r2, r3, ap, rotAngle) {
    return "<g transform='rotate(" + rotAngle + " 50 50)' style='stroke:#000000;stroke-width:1;stroke-linecap:square;stroke-linejoin:round;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none'>" + 
	makeArc(r1, r2, ap * 2*Math.PI, 0.3) + 
	makeArc(r2, r3, ap * 2*Math.PI, 1) + 
	"</g>";
}

function makeSVG(t) {
    return "<?xml version='1.0' encoding='UTF-8' standalone='no'?>\n\n<svg xmlns='http://www.w3.org/2000/svg' width='120' height='100' version='1.1'>" +
	makeDoubleArc(20, 30, 40, 0.4, t*360) +
	makeDoubleArc(15, 25, 30, 0.3, -2*t*360) +
	makeDoubleArc(10, 50, 50, 0.05, 3*t*360) +
	"</svg>";
}

function pad(num, size) {
    ret = num+"";
    while(ret.length < size) ret = "0" + ret;
    return ret;
}

var fs = require("fs");

var numFrames = 100;

for(var f = 0; f < numFrames; f++) {
    fs.writeFileSync("tempFrames/frame" + pad(f, (numFrames+"").length) + ".svg", makeSVG(f/numFrames));
}
