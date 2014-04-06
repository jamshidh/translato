
function getElementRectangle(target) {
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

