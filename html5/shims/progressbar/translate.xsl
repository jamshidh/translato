<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="progress">
    <library src="progress.js" />
    <styleLib src="progress.css" />
    <xsl:copy>
      <xsl:apply-templates select="attribute" />
    </xsl:copy>
  </xsl:template>

</xsl:stylesheet>