<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="serverFuncDeclaration">
    <shimLib name="addServerFunction"/>
    <funcDeclaration name="{@name}">
      <xsl:copy-of select="parameter"/>
      <fullBody>
	<return>
	  <function>
	    <variable name="callServer"/>
	    <string value="{@name}"/>
	    <variable name="arguments"/>
	  </function>
	</return>
      </fullBody>
    </funcDeclaration>
  </xsl:template>

</xsl:stylesheet>
