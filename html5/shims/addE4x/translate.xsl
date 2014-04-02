<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>



  <xsl:template match="embeddedElement">
    <xsl:apply-templates select="*" />
  </xsl:template>

  <xsl:template match="e4xElement">
    <function>
      <lambda>
	<fullBody>
	  <varDeclarationExpression>
	    <varDeclaration name="el">
	      <function>
		<lvaluedot>
		  <variable name="document" />
		  <variable name="createElement" />
		</lvaluedot>
		<string value="{@e4xTagName}" />
	      </function>
	    </varDeclaration>
	  </varDeclarationExpression>
	  <xsl:for-each select="e4xExprAttr">
	    <expressionCommand>
	      <function>
		<lvaluedot>
		  <variable name="el" />
		  <variable name="setAttribute" />
		</lvaluedot>
		<string value="{@name}" />
		<xsl:copy-of select="*" />
	      </function>
	    </expressionCommand>
	  </xsl:for-each>
	  <xsl:for-each select="e4xAttribute">
	    <expressionCommand>
	      <function>
		<lvaluedot>
		  <variable name="el" />
		  <variable name="setAttribute" />
		</lvaluedot>
		<string value="{@name}" />
		<string value="{@value}" />
	      </function>
	    </expressionCommand>
	  </xsl:for-each>
	  <xsl:for-each select="*[name() != 'e4xAttribute' and name() != 'e4xExprAttr']">
	    <expressionCommand>
	      <function>
		<lvaluedot>
		  <variable name="el" />
		  <variable name="appendChild" />
		</lvaluedot>
		<xsl:apply-templates select="." />
	      </function>
	    </expressionCommand>
	  </xsl:for-each>
	  <return>
	    <variable name="el" />
	  </return>
	</fullBody>
      </lambda>
    </function>
  </xsl:template>

  <xsl:template match="e4xAttrNode">
    <function>
      <lvaluedot>
	<variable name="document" />
	<variable name="createTextNode" />
      </lvaluedot>
      <function>
	<lvaluedot>
	  <paren>
	    <xsl:apply-templates select="*" />
	  </paren>
	  <variable name="toString" />
	</lvaluedot>
      </function>
    </function>
  </xsl:template>

  <xsl:template match="e4xExprNode">
    <function>
      <lvaluedot>
	<variable name="document" />
	<variable name="createTextNode" />
      </lvaluedot>
      <function>
	<lvaluedot>
	  <paren>
	    <xsl:apply-templates select="*" />
	  </paren>
	  <variable name="toString" />
	</lvaluedot>
      </function>
    </function>
  </xsl:template>
    
  <xsl:template match="e4xText">
    <function>
      <lvaluedot>
	<variable name="document" />
	<variable name="createTextNode" />
      </lvaluedot>
      <string>
	<xsl:attribute name="value">
	  <xsl:for-each select="./word">
	    <xsl:value-of select="text()" />
	    <xsl:if test="not(position() = last())"><xsl:text> </xsl:text></xsl:if>
	  </xsl:for-each>
	</xsl:attribute>
      </string>
    </function>
  </xsl:template>

</xsl:stylesheet>