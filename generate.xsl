<?xml version="1.0" encoding="utf-8"?>

<xsl:stylesheet version="1.0"
		xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
		xmlns:o="my:dummyNS" exclude-result-prefixes="o">

  <xsl:output omit-xml-declaration="yes" indent="yes"/>
  
  <xsl:namespace-alias result-prefix="xsl" stylesheet-prefix="o"/>

  <xsl:template match="text">
    <xsl:value-of select="text()" />
  </xsl:template>

  <xsl:template match="whitespace">
    <o:text>
      <xsl:value-of select="text()" />
    </o:text>
  </xsl:template>

  <xsl:template match="expression">
    <xsl:apply-templates select="*" />
  </xsl:template>

  <xsl:template match="list">
    <o:apply-templates select="*" />
  </xsl:template>

  <xsl:template match="or">
    <o:apply-templates select="*" />
  </xsl:template>

  <xsl:template match="lrule">
    <o:apply-templates select="*" />
  </xsl:template>

  <xsl:template match="attribute">
    <o:value-of>
      <xsl:attribute name="select">@<xsl:value-of select="text()" /></xsl:attribute>
    </o:value-of>
  </xsl:template>
  

  <xsl:template match="rule">
    <o:template match="{@tag}">
      <xsl:apply-templates select="*" />
    </o:template>
  </xsl:template>
  
  <xsl:template match="/">

    <o:stylesheet version="1.0"
		    xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

      <o:output method="text" />

      <xsl:apply-templates select="/*/rule" />

      <o:template match="/">
	<o:apply-templates select="*"/>
      </o:template>
      
    </o:stylesheet>

  </xsl:template>

</xsl:stylesheet>
