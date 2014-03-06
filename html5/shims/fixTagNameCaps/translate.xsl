<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="dot/variable[position() = 2 and @name = 'tagName']">
    <dot>
      <xsl:copy>
	<xsl:apply-templates select="*|@*|text()" />
      </xsl:copy>
      <function>
	<variable name="toUpperCase" />
      </function>
    </dot>
  </xsl:template>

</xsl:stylesheet>