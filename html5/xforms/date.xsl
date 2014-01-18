<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="*|@*|text()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="script">
    <xsl:copy>
      <xsl:apply-templates mode="script" select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>


  <xsl:template match="element[@tagName='head']">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
      <parse using="element"><![CDATA[<link rel="stylesheet" href="http://code.jquery.com/ui/1.10.3/themes/smoothness/jquery-ui.css"/>]]></parse>
      <parse using="script"><![CDATA[<script src="http://code.jquery.com/jquery-1.9.1.js"></script>]]></parse>
      <parse using="script"><![CDATA[<script src="http://code.jquery.com/ui/1.10.3/jquery-ui.js"></script>]]></parse>
      <parse using="element"><![CDATA[<link rel="stylesheet" href="/resources/demos/style.css"/>]]></parse>
      <!--parse using="script"><![CDATA[<script>
	$(function() {
	$( "#datepicker" ).datepicker();
	});
      </script>]]></parse-->
      <parse using="script"><![CDATA[<script>
	function() {
	$( "#datepicker" ).datepicker();
	}
      </script>]]></parse>

    </xsl:copy>
  </xsl:template>

  <xsl:template match="element[@tagName='input' and ./attribute[@name='type' and @value='date']]">
    <parse using="element"><![CDATA[<input type="text" id="datepicker"/>]]></parse>
  </xsl:template>

</xsl:stylesheet>
