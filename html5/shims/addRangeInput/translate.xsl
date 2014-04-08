<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="input[attribute[@name='type' and @value='range']]">
    <xsl:variable name="theId">
      <xsl:choose>
	<xsl:when test="attribute[@name='id']"><xsl:value-of select="attribute[@name='id']/@value" /></xsl:when>
	<xsl:otherwise><xsl:value-of select="generate-id()" /></xsl:otherwise>
      </xsl:choose>
    </xsl:variable>


    <shimLib name="addRangeInput" />
    <xsl:copy>
      <attribute name="id" value="{$theId}" />
      <!--attribute name="style" value="display: none;" /-->
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
    <div>
      <div>
	<attribute name="id" value="{concat($theId,'_bar')}" />
	<attribute name="class" value="rangeInputBar" />
	<attribute name="onmousedown" value='{concat(concat("barPress(&apos;", $theId), "&apos;, event);")}' />
      </div>
      <img>
	<attribute name="id" value="{concat($theId,'_gripper')}" />
	<attribute name="class" value="rangeInputGripper" />
	<attribute name="onmousedown" value='{concat(concat("mouseDown(&apos;", $theId), "&apos;, event); return false;")}' />
	<attribute name="src" value="/res/addRangeInput/arrow15.png" />
      </img>
    </div>
    <script>
      <expressionCommand>
	<function>
	  <variable name="placeThisGripper" />
	  <string value="{$theId}" />
	</function>
      </expressionCommand>
    </script>
  </xsl:template>

</xsl:stylesheet>