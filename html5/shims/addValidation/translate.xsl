<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="input[attribute[@name='type']]">
    <xsl:copy>
      <xsl:apply-templates select="@*" />
      <attribute name="onfocus" value="hideValidationWarning();" />
      <xsl:apply-templates select="*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="form">
    <shimLib name="addValidation" />
    <xsl:copy>
      <attribute name="onsubmit" value="return validateForm(this);" />
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="body">
    <xsl:copy>
      <xsl:apply-templates select="*" />
      <div>
	<attribute name="id" value="validationWarningBubble" />
	<table>
	  <attribute name="cellspacing" value="0" />
	  <attribute name="cellpadding" value="0" />
	  <attribute name="border" value="0" />
	  <tr>
	    <td>
	      <img>
		<attribute name="src" value="/res/addValidation/pinkTriangle.png" />
	      </img>
	    </td>
	  </tr>
	  <tr>
	    <td><attribute name="id" value="validationWarningBubbleMessage" /><text><word>Error</word><word>Message</word><word>Placeholder</word></text></td>
	  </tr>
	</table>
      </div>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>