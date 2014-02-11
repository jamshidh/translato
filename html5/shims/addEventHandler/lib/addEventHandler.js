

function addEventListenerFunc(x, y, z) {
    alert("Setting event listener: " + x + ", " + y + ", " + z);
}

function removeEventListenerFunc(x, y, z) {
    alert("Removing event listener");
}

//HTMLDocument.prototype.addEventListener = addEventListenerFunc;
//HTMLDocument.prototype.removeEventListener = removeEventListenerFunc;

//Element.prototype.addEventListener = addEventListenerFunc;
//Element.prototype.removeEventListener = removeEventListenerFunc;

window.addEventListener = addEventListenerFunc;
window.removeEventListener = removeEventListenerFunc;

