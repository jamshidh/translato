<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="head">
    <xsl:copy>
      <xsl:for-each select="//library">
	<script src="{concat('/lib/', @src)}" />
      </xsl:for-each>
      <xsl:for-each select="//styleLib">
	<link rel="stylesheet" type="text/css" href="{concat('/lib/', @src)}" />
      </xsl:for-each>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="library">
  </xsl:template>

  <xsl:template match="styleLib">
  </xsl:template>

</xsl:stylesheet>