/**
 * Based on code from https://gist.github.com/brettz9/4093766#file_html5_dataset.js
*/

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
	
        HTML5_DOMStringMap = document.createElement('div'); // {};
	
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


