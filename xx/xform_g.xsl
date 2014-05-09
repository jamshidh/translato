<?xml version="1.0"?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text"/>

  <xsl:template match="*">
    <xsl:message terminate="yes">xform_g.xsl: Error, unknown tag "<xsl:value-of select="name()"/>".
    data:
    <xsl:copy-of select="."/>
    </xsl:message>
  </xsl:template>

  <xsl:template match="error">
    <xsl:message terminate="yes">xform_g.xsl: Error: <xsl:value-of select="text()"/></xsl:message>
  </xsl:template>

  <xsl:template mode="xpath" match="*">
    <xsl:message terminate="yes">xform_g.xsl: Error, unknown tag "<xsl:value-of select="name()"/>" in xpath mode.</xsl:message>
  </xsl:template>

  <xsl:template mode="c-xpath" match="*">
    <xsl:message terminate="yes">xform_g.xsl: Error, unknown tag "<xsl:value-of select="name()"/>" in xpath mode.</xsl:message>
  </xsl:template>

  <xsl:template mode="elName" match="*">
    <xsl:message terminate="yes">xform_g.xsl: Error, unknown tag "<xsl:value-of select="name()"/>" in elName mode.</xsl:message>
  </xsl:template>

  <xsl:template match="/">

    <xsl:apply-templates select="//error"/>

    <xsl:apply-templates select="//xform"/>

int main(int argc, char **argv) {

<xsl:for-each select="//xform/*[1]">
  const char *xpath<xsl:value-of select="position()"/>[] = {<xsl:apply-templates mode="c-xpath" select="."/>, NULL};
  xformTemplate template<xsl:value-of select="position()"/> = {xpath<xsl:value-of select="position()"/>, &amp;action<xsl:value-of select="position()"/>};
</xsl:for-each>



  xformTemplate *templates[] = {<xsl:for-each select="//xform">&amp;template<xsl:value-of select="position()"/></xsl:for-each>, NULL};

  return readDocAndModify(argc, argv, templates);

}

  </xsl:template>

  <xsl:template mode="c-xpath" match="tagname">"<xsl:value-of select="word/text()"/>"</xsl:template>

  <xsl:template mode="c-xpath" match="xpathdivide">
    <xsl:apply-templates mode="c-xpath" select="*[1]"/>, <xsl:apply-templates mode="c-xpath" select="*[2]"/>
  </xsl:template>

  <xsl:template mode="xpath" match="tagname">
    <xsl:value-of select="word/text()"/>
  </xsl:template>

  <xsl:template mode="xpath" match="xpathdivide">
    <xsl:apply-templates mode="xpath" select="*[1]"/>/<xsl:apply-templates mode="xpath" select="*[2]"/>
  </xsl:template>



  <xsl:template match="xform">

void action<xsl:value-of select="position()"/>(xmlDocPtr doc, map&lt;string, xmlNodePtr> vars) {
  <xsl:apply-templates select="*[position() &gt; 1]"/>
}

  </xsl:template>

  <xsl:template match="mv[first]">
  moveFirst(doc, vars, "<xsl:apply-templates mode="xpath" select="*[2]"/>", "<xsl:apply-templates mode="xpath" select="*[3]"/>");
  </xsl:template>

  <xsl:template match="mv[last]">
  moveLast(doc, vars, "<xsl:apply-templates mode="xpath" select="*[2]"/>", "<xsl:apply-templates mode="xpath" select="*[3]"/>");
  </xsl:template>

  <xsl:template match="mv[before]">
  moveBefore(doc, vars, "<xsl:apply-templates mode="xpath" select="*[2]"/>", "<xsl:apply-templates mode="xpath" select="*[3]"/>");
  </xsl:template>

  <xsl:template match="mv[after]">
  moveAfter(doc, vars, "<xsl:apply-templates mode="xpath" select="*[2]"/>", "<xsl:apply-templates mode="xpath" select="*[3]"/>");
  </xsl:template>

  <xsl:template match="rm">
  deleteNode(doc, vars, "<xsl:apply-templates mode="xpath" select="*[1]"/>");
  </xsl:template>

  <xsl:template match="delete">
  deleteNode(doc, vars, vars["pet"]);
  </xsl:template>


  <xsl:template match="mkelem">
    xmlNodePtr elem<xsl:apply-templates mode="elName" select="complexElem"/> = getNode(doc, vars, "<xsl:apply-templates mode="xpath" select="*[2]"/>", "action");
  <xsl:apply-templates select="*[position() &gt; 2]"/>
  </xsl:template>

  <xsl:template match="name">
    <xsl:value-of select="word/text()"/>
  </xsl:template>



  <xsl:template mode="addAttr" match="attribute">
    xmlNewProp(elem<xsl:apply-templates mode="elName" select=".."/>, BAD_CAST "<xsl:value-of select="@name"/>", BAD_CAST "<xsl:value-of select="@value"/>");
  </xsl:template>

  <xsl:template match="element">
    xmlNodePtr elem<xsl:apply-templates mode="elName" select="."/> = xmlNewNode(elem<xsl:apply-templates mode="elName" select=".."/>->ns, BAD_CAST "<xsl:value-of select="@tagName"/>");

    <xsl:apply-templates mode="addAttr" select="attribute"/>

    <xsl:choose>
      <xsl:when test="../../before">xmlAddPrevSibling</xsl:when>
      <xsl:when test="../../after">xmlNextSibling</xsl:when>
      <xsl:when test="../../last">xmlAddChild</xsl:when>
      <xsl:when test="../../first">addFirst</xsl:when>
      <xsl:otherwise>xmlAddChild</xsl:otherwise>
    </xsl:choose>(elem<xsl:apply-templates mode="elName" select=".."/>, elem<xsl:apply-templates mode="elName" select="."/>);

  </xsl:template>

  <xsl:template match="complexElem">
    <xsl:apply-templates select=".//element"/>
  </xsl:template>

  <xsl:template match="mkattr">
  addAttribute(vars, "/child/job", "title", "supreme being");
  </xsl:template>

  <xsl:template mode="elName" match="element">
    <xsl:apply-templates mode="elName" select=".."/>_<xsl:value-of select="position()"/>
  </xsl:template>

  <xsl:template mode="elName" match="complexElem">
    <xsl:value-of select="count(preceding::complexElem)"/>
  </xsl:template>


</xsl:stylesheet>
