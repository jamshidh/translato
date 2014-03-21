/**
 * Add dataset support to elements
 * No globals, no overriding prototype with non-standard methods, 
 *   handles CamelCase properly, attempts to use standard 
 *   Object.defineProperty() (and Function bind()) methods, 
 *   falls back to native implementation when existing
 * Inspired by http://code.eligrey.com/html5/dataset/ 
 *   (via https://github.com/adalgiso/html5-dataset/blob/master/html5-dataset.js )
 * Depends on Function.bind and Object.defineProperty/Object.getOwnPropertyDescriptor (shims below)
 * Licensed under the X11/MIT License
*/

// Removed a few JSLint options as Notepad++ JSLint validator complaining and 
//   made comply with JSLint; also moved 'use strict' inside function
/*jslint white: true, undef: true, plusplus: true,
  bitwise: true, regexp: true, newcap: true, maxlen: 90 */


// Begin dataset code

var propDescriptor = {
    enumerable: true,
    get: function () {
        'use strict';
        var i, 
        that = this,
        HTML5_DOMStringMap, 
        attrVal, attrName, propName,
        attribute,
        attributes = this.attributes,
        attsLength = attributes.length,
        toUpperCase = function (n0) {
            return n0.charAt(1).toUpperCase();
        },
        getter = function () {
            return this;
        },
        setter = function (attrName, value) {
            return (typeof value !== 'undefined') ? 
                this.setAttribute(attrName, value) : 
                this.removeAttribute(attrName);
        };
	
        HTML5_DOMStringMap = {};
	
        for (i = 0; i < attsLength; i++) {
            attribute = attributes[i];
            // Fix: This test really should allow any XML Name without 
            //         colons (and non-uppercase for XHTML)
            if (attribute && attribute.name && 
                (/^data-\w[\w\-]*$/).test(attribute.name)) {
                attrVal = attribute.value;
                attrName = attribute.name;
                // Change to CamelCase
                propName = attrName.substr(5).replace(/-./g, toUpperCase);

                Object.defineProperty(HTML5_DOMStringMap, propName, {
                    enumerable: this.enumerable,
                    get: getter.bind(attrVal || ''),
                    set: setter.bind(that, attrName)
                });

            }
        }
        return HTML5_DOMStringMap;
    }
};

Object.defineProperty(
    Element.prototype, 
    'dataset', 
    propDescriptor);


