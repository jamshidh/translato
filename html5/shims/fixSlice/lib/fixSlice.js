
(function() {


    //Taken from http://stackoverflow.com/questions/2735067/how-to-convert-a-dom-node-list-to-an-array-in-javascript
    function toArray(obj) {
	var array=[];
	// iterate backwards ensuring that length is an UInt32
	
	for (var i=0; i < obj.length; i++) {
            array[i] = obj[i];
	}
	return array;
    }   




    var originalSlice = Array.prototype.slice;

    Array.prototype.slice = function(x, y) {

	if (!('slice' in this)) {
	    var theArray = toArray(this);

	    return originalSlice.apply(theArray, arguments);
	}

	return originalSlice.apply(this, arguments);
    }



})();
