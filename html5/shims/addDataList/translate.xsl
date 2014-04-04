<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="datalist">
    <shimLib name="addDataList" />
    <select>
      <xsl:apply-templates mode="changeAttribute" select="attribute" />
      <xsl:apply-templates select="option" />
    </select>
  </xsl:template>

  <xsl:template mode="changeAttribute" match="attribute[@name='id']">
    <attribute name="id" value="{@value}" />
    <attribute name="data-datalist" value="{@value}" />
  </xsl:template>

</xsl:stylesheet>