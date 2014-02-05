<?xsl version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text" />

  <xsl:template match="widget">
    <xsl:apply-templates select=".//event" />
    
    window.onload = function() {
 
      var element = document.getElementsByTagName("details")

      for(var i = 0; i &lt; detailsElements.length; i++) {

	el = detailsElements[i];

        <xsl:apply-templates select="property" />
	<xsl:apply-templates select="attribute" />

	<xsl:apply-templates select="constructor" />

      }

  </xsl:template>

  <xsl:template match="event">
    var openEvt = document.createEvent("Event");
    openEvt.initEvent("<xsl:value-of select="@name" />",true,false);
  </xsl:template>

  <xsl:template match="property">
	Object.defineProperty(detailsElements[i], "<xsl:value-of select="@name" />", {
	    set: function(value) {
	      <xsl:value-of select="getter/text()" />
	    },
	    get: function() {
	      <xsl:value-of select="setter/text()" />
	    }
	});
  </xsl:template>

  <xsl:template match="attribute">
    el.setAttribute = function(name, value) { 
      <xsl:value-of select="setter/text()" />
      Object.getPrototypeOf(el).setAttribute.call(el, name, value); 
    }
    el.removeAttribute = function(name) { 
      <xsl:value-of select="remover/text()" />
      Object.getPrototypeOf(el).removeAttribute.call(el, name); 
    }
  </xsl:template>

</xsl:stylesheet>