<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text"/>

  <xsl:template match="*">
    <xsl:message terminate="yes">xform_g.xsl: Error, unknown tag "<xsl:value-of select="name()"/>".
    </xsl:message>
  </xsl:template>

  <xsl:template match="jsFile">
    <xsl:apply-templates select="expressionCommand/funcDeclaration"/>
    
    int main(int argc, char **argv) {

      <xsl:apply-templates select="expressionCommand[name(*[1]) != 'funcDeclaration']"/>
    }
  </xsl:template>

  <xsl:template match="expressionCommand">
    <xsl:apply-templates select="*"/>;
  </xsl:template>

  <xsl:template match="funcDeclaration">
    void <xsl:value-of select="@name"/>(<xsl:apply-templates select="parameter"/>) {
    <xsl:apply-templates select="fullBody/*"/>
    }
  </xsl:template>

  <xsl:template match="parameter">
    <xsl:for-each select="*">
      <xsl:value-of select="text()"/>
      <xsl:if test="position() != last()">, </xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template match="function">
    <xsl:apply-templates select="*[1]"/>(<xsl:for-each select="*[position != 1]">
    <xsl:apply-templates select="."/>
  </xsl:for-each>)</xsl:template>

  <xsl:template match="variable">
    <xsl:value-of select="@name"/>
  </xsl:template>

  <xsl:template match="return">
    return <xsl:apply-templates select="*"/>;
  </xsl:template>

  <xsl:template match="varDeclarationExpression">
    <xsl:apply-templates select="varDeclaration"/>;
  </xsl:template>

  <xsl:template match="varDeclaration">
    int <xsl:value-of select="@name" />
    <xsl:if test="count(*) != 0"> = <xsl:apply-templates select="*"/></xsl:if>
  </xsl:template>


</xsl:stylesheet>
