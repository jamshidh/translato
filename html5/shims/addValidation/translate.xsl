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
	<attribute name="onblur" value="hideValidationWarning();" />
	<attribute name="tabindex" value="-1" />
	<attribute name="id" value="validationWarningBubble" />
	<table>


	  <tr>
	    <td><attribute name='style' value="background-image: url('/res/addValidation/topLeft.png'); background-repeat: no-repeat; background-position: bottom; height: 22px"/></td>
	    <td>
	      <attribute name='style' value="background-image: url('/res/addValidation/top.png'); background-repeat: repeat-x; background-position: bottom;"/>
	      <img><attribute name='src' value='/res/addValidation/pointy.png' /></img>
	    </td>
	    <td><attribute name='style' value="background-image: url('/res/addValidation/topRight.png'); background-repeat: no-repeat; background-position: bottom;"/></td>
	  </tr>


	  <tr>
	    <td><attribute name='style' value="background-repeat: repeat-y; background-image: url('/res/addValidation/left.png');"/></td>
	    <td>
	      <attribute name='style' value="background: rgb(255, 127, 182)"/>
	      <attribute name="id" value="validationWarningBubbleMessage" />
	      <text><word>Error</word><word>Message</word><word>Placeholder</word></text>
	    </td>
	    <td><attribute name='style' value="background-repeat: repeat-y; background-image: url('/res/addValidation/right.png');"/></td>
	  </tr>


	  <tr>
	    <td>
	      <img>
		<attribute name="style" value="width: 12px;" />
		<attribute name="src" value="/res/addValidation/bottomLeft.png" />
	      </img>
	    </td>
	    <td>
	      <img>
		<attribute name='style' value="width: 100%; height: 12px;" />
		<attribute name="src" value="/res/addValidation/bottom.png" />
	      </img>
	    </td>

	    <td>
	      <img>
		<attribute name='style' value="width: 12px;" />
		<attribute name="src" value="/res/addValidation/bottomRight.png" />
	      </img>
	    </td>

	  </tr>




      	</table>
      </div>
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>