<?xml version="1.0" encoding="utf-8" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="text" />

  <xsl:strip-space elements="*" />

  <!--xsl:template match="*|@*|text">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template-->

  <xsl:template match="*|@*|text">
    <xsl:message terminate="yes">
      Error: unknown tag <xsl:value-of select="name()" />.
    </xsl:message>
  </xsl:template>

  <xsl:template match="/">
    <xsl:apply-templates select="htmlFile/html/body" />
  </xsl:template>

  <xsl:template match="body">

#include &lt;gtk/gtk.h&gt;
#include &lt;stdlib.h&gt;

#include "widgets.cc"
#include "css.cc"

#include "flowbox.h"

static void destroy( GtkWidget *widget, gpointer   data ) {
    gtk_main_quit ();
}








int main(int argc, char **argv) {
  GtkWidget *window;
  GtkWidget *flowBox;

  
  gtk_init (&amp;argc, &amp;argv);
  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);

  gtk_window_set_default_size(GTK_WINDOW(window), 800, 300);

  gtk_window_set_title(GTK_WINDOW(window), "<xsl:apply-templates mode="text" select="/htmlFile/html/head/title" />");
  
  g_signal_connect (window, "destroy", G_CALLBACK (destroy), NULL);

  applyCSSToWindow(<xsl:apply-templates mode="style" select="//style" />);

  gtk_container_set_border_width (GTK_CONTAINER (window), 10);

  flowBox = flow_box_new();

  gtk_container_add (GTK_CONTAINER(window), flowBox);

  <xsl:apply-templates select="*" />

  gtk_widget_show (flowBox);

  gtk_widget_show (window);

  gtk_main ();
  
  return 0;
  
}

  </xsl:template>

  <xsl:template match="h1[text]">
    addH1(FLOW_BOX(flowBox), "<xsl:apply-templates mode="text" select="text"/>");
  </xsl:template>

  <xsl:template match="button[text]">
    addButton(FLOW_BOX(flowBox), "<xsl:value-of select="text"/>");
  </xsl:template>

  <xsl:template match="button">
    addButton(FLOW_BOX(flowBox), "[unknown label]");
  </xsl:template>

  <xsl:template match="img">
    addImage(FLOW_BOX(flowBox), "<xsl:value-of select="attribute[@name='src']/@value"/>");
  </xsl:template>

  <xsl:template match="text">
    addLabel(FLOW_BOX(flowBox), "<xsl:apply-templates mode="text" select="." />");
  </xsl:template>

  <xsl:template match="input">
    addEntry(FLOW_BOX(flowBox));
  </xsl:template>

  <xsl:template match="progress">
    addProgressBar(FLOW_BOX(flowBox));
  </xsl:template>

  <xsl:template match="br">
    addBreak(FLOW_BOX(flowBox));
  </xsl:template>

  <xsl:template match="table">
    GtkGrid *grid = addGrid(FLOW_BOX(flowBox));
    <xsl:apply-templates mode="grid" select="*" />
  </xsl:template>

  <xsl:template match="ul">
    GtkGrid *grid = addGrid(FLOW_BOX(flowBox));
    <xsl:apply-templates mode="ul" select="*" />
  </xsl:template>

  <xsl:template match="ol">
    GtkGrid *grid = addGrid(FLOW_BOX(flowBox));
    <xsl:apply-templates mode="ol" select="*" />
  </xsl:template>

  <xsl:template mode="text" match="text">
    <xsl:for-each select="word">
      <xsl:value-of select="." />
      <xsl:if test="position() != last()"><xsl:text> </xsl:text></xsl:if>
    </xsl:for-each>
  </xsl:template>

  <xsl:template mode="text" match="title">
    <xsl:for-each select="word">
      <xsl:value-of select="." />
      <xsl:if test="position() != last()"><xsl:text> </xsl:text></xsl:if>
    </xsl:for-each>
  </xsl:template>




  <xsl:template mode="grid" match="*|@*|text">
    <xsl:copy>
      <xsl:apply-templates mode="grid" select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>




  <xsl:template mode="grid" match="attribute">
  </xsl:template>

  <xsl:template mode="grid" match="text()">
    addLabelToGrid(grid, "<xsl:value-of select="." />", <xsl:value-of select="count(../../../../preceding-sibling::*)" />, <xsl:value-of select="count(../../../preceding-sibling::*)"/>);
  </xsl:template>

  <xsl:template mode="ul" match="attribute">
  </xsl:template>

  <xsl:template mode="ul" match="text()">
    addLabelToUl(grid, "<xsl:value-of select="." />", <xsl:value-of select="count(../../../preceding-sibling::*)"/>);
  </xsl:template>

  <xsl:template mode="ol" match="attribute">
  </xsl:template>

  <xsl:template mode="ol" match="text()">
    addLabelToOl(grid, "<xsl:value-of select="." />", <xsl:value-of select="count(../../../preceding-sibling::*)"/>);
  </xsl:template>

  <xsl:template mode="style" match="style">"<xsl:apply-templates mode="style" select="declarationBlock" />"</xsl:template>

  <xsl:template mode="style" match="declarationBlock">
    <xsl:apply-templates mode="styleSelector" select="*[1]" />{<xsl:apply-templates mode="styleValue" select="*[position() != 1]" />}</xsl:template>

  <xsl:template mode="styleSelector" match="tagname[@value='body']">GtkWindow</xsl:template>
  <xsl:template mode="styleSelector" match="tagname[@value='table']">GtkGrid</xsl:template>

  <xsl:template mode="styleValue" match="backgroundColor">background-color: <xsl:value-of select="@value" />;</xsl:template>

  <xsl:template mode="styleValue" match="border">border: <xsl:value-of select="@value" />;</xsl:template>
  <xsl:template mode="styleValue" match="padding">padding: <xsl:value-of select="@value" />;</xsl:template>

  

</xsl:stylesheet>
