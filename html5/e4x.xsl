<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:template match="*|@*|text()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="script">
    <xsl:copy>
      <xsl:apply-templates mode="script" select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="element[@tagName='body']">
    <parse using="script"><![CDATA[<script> 

      function json2Xml(json) {
        if (json.type == "text") {
          return document.createTextNode(json.value);
        }

        var ret = document.createElement(json.tag);
	var attNames = Object.keys(json.attributes);
        for(var i = 0; i < attNames.length; i++) {
          ret.setAttribute(attNames[i], json.attributes[attNames[i]]);
        }
        for(var i = 0; i < json.body.length; i++) {
          ret.appendChild(json2Xml(json.body[i]));
        }
        return ret;
      }

    </script>]]></parse>
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template mode="script" match="*|@*|text()">
    <xsl:copy>
      <xsl:apply-templates mode="script" select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template mode="script" match="embeddedElement">
    <function>
      <variable name="json2Xml" />
      <object>
	<field><string value="tag" /><string value="{@tagName}" /></field>
	<field>
	  <string value="attributes" />
	  <object><xsl:apply-templates mode="script" select="attribute" /></object>
	</field>
	<field>
	  <string value="body" />
	  <array><xsl:apply-templates mode="script" select="element" /></array>
	</field>
      </object>
    </function>
  </xsl:template>

  <xsl:template mode="script" match="element">
    <object>
      <field><string value="tag" /><string value="{@tagName}" /></field>
      <field>
	<string value="attributes" />
	<object><xsl:apply-templates mode="script" select="attribute" /></object>
      </field>
      <field>
	<string value="body" />
	<array><xsl:apply-templates mode="script" select="element" /></array>
      </field>
    </object>
  </xsl:template>

  <xsl:template mode="script" match="attribute">
    <field>
      <string value="{@name}" /><string value="{@value}" />
    </field>
  </xsl:template>

</xsl:stylesheet>
