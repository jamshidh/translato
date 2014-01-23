<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="html" /> 


  <xsl:template match="@*|node()|text()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="head">

    <xsl:copy>
      
      <xsl:apply-templates select="*" />

      <link rel="stylesheet" type="text/css" href="css/detailsSummary.css"/>
      <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.10.2/jquery.min.js"></script>
      <script src="js/jquery.details.js"></script>
      
    </xsl:copy>
  </xsl:template>

  <xsl:template match="body">

    <xsl:copy>
      <xsl:apply-templates select="*" />

      <script>

	  window.console || (window.console = { 'log': alert });

	  jQuery(function() {
	  
	    // Add conditional classname based on support
	    jQuery('html').addClass(jQuery.fn.details.support ? 'details' : 'no-details');
	  
	    // Show a message based on support
	    jQuery('body').prepend(jQuery.fn.details.support ? 'Native support detected; the plugin will only add ARIA annotations and fire custom open/close events.' : 'Emulation active; you are watching the plugin in action!');
	  
	    // Emulate &lt;details&gt; where necessary and enable open/close event handlers
	    jQuery('details').details();
	  
	    // Bind some example event handlers
	    jQuery('details').on({
	      'open.details': function() {
	        console.log('opened');
	      },
	      'close.details': function() {
	        console.log('closed');
	      }
	    });
	  
	  });

      </script>
    </xsl:copy>
  </xsl:template>


</xsl:stylesheet>