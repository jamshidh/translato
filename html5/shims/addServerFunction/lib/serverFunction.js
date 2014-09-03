
function callServer(funcName, params) {
    var request = new XMLHttpRequest();

    var content = JSON.stringify({funcName:funcName, params:[].slice.apply(params)});

    request.open('POST', location.href.substr(0, location.href.lastIndexOf(".")) + ".sjs", false);  // `false` makes the request synchronous
    request.setRequestHeader("Content-length", content.length);
    request.send(content);
    
    if (request.status === 200) {
	console.log(request.responseText);
	return request.responseText;
    }
    
    throw new Error("server response error");

}
