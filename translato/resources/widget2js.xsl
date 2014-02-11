<?xsl version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text" />

  <xsl:param name="widgetName" />

  <xsl:template match="widget">

    <xsl:copy-of select="code/text()" />

    <xsl:apply-templates select="event" />

    window.addEventListener("load", function() {
 
      var elements = document.getElementsByTagName("<xsl:value-of select="$widgetName" />")

      for(var i = 0; i &lt; elements.length; i++) {

	var element = elements[i];

        <xsl:apply-templates select="property" />
	<xsl:apply-templates select="attribute" />

	<xsl:apply-templates select="constructor" />

	<xsl:apply-templates select="eventHandler" />

      }

    });

  </xsl:template>

  <xsl:template match="event">
    var <xsl:value-of select="@name" />Evt = document.createEvent("Event");
    <xsl:value-of select="@name" />Evt.initEvent("<xsl:value-of select="@name" />",true,false);
  </xsl:template>

  <xsl:template match="property">
	Object.defineProperty(element, "<xsl:value-of select="@name" />", {
	    set: function(value) {
	      <xsl:value-of select="setter/text()" />
	    },
	    get: function() {
	      <xsl:value-of select="getter/text()" />
	    }
	});
  </xsl:template>

  <xsl:template match="attribute">
    element.setAttribute = function(name, value) { 
      if (name == "<xsl:value-of select="@name" />") {
        <xsl:value-of select="setter/text()" />
      }
      Object.getPrototypeOf(element).setAttribute.call(element, name, value); 
    }
    element.removeAttribute = function(name) { 
      if (name == "<xsl:value-of select="@name" />") {
        <xsl:value-of select="remover/text()" />
      }
      Object.getPrototypeOf(element).removeAttribute.call(element, name); 
    }
  </xsl:template>

  <xsl:template match="eventHandler">
    element.addEventListener('<xsl:value-of select="@name" />', function() {
      <xsl:value-of select="text()" />
    });
  </xsl:template>

</xsl:stylesheet>