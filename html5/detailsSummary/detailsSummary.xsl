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

	  jQuery(function() {
	  
	    // Add conditional classname based on support
	    jQuery('html').addClass(jQuery.fn.details.support ? 'details' : 'no-details');
	  
	    // Emulate &lt;details&gt; where necessary and enable open/close event handlers
	    jQuery('details').details();
	  
	  });

      </script>
    </xsl:copy>
  </xsl:template>


</xsl:stylesheet>