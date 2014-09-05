
function callServer(funcName, params) {
    var request = new XMLHttpRequest();

    var content = JSON.stringify({funcName:funcName, params:[].slice.apply(params)});

    var locNoQuery = window.location.href.split('?')[0]

    request.open('POST', locNoQuery.substr(0, locNoQuery.lastIndexOf(".")) + ".sjs", false);  // `false` makes the request synchronous
    request.send(content);
    
    if (request.status === 200) {
	return request.responseText;
    }
    
    throw new Error("server response error");

}
