
function callServer(funcName, params) {
    var request = new XMLHttpRequest();

    var content = JSON.stringify({funcName:funcName, params:[].slice.apply(params)});

    request.open('POST', location.href.substr(0, location.href.lastIndexOf(".")) + ".sjs", false);  // `false` makes the request synchronous
    request.send(content);
    
    if (request.status === 200) {
	return request.responseText;
    }
    
    throw new Error("server response error");

}
