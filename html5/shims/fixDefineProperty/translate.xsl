<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="variable[@name = 'defineProperty']">
    <shimLib name="fixDefineProperty" />
    <xsl:copy>
      <xsl:attribute name="name">fixedDefineProperty</xsl:attribute>
      <xsl:apply-templates select="*|text()" />
    </xsl:copy>
  </xsl:template>

  <!--This only works on objects created empty-->
  <xsl:template match="object[count(*) = 0]">
    <function>
      <lvaluedot>
	<variable name="document" />
	<variable name="createElement" />
      </lvaluedot>
      <string value="div"/>
    </function>
  </xsl:template>

</xsl:stylesheet>