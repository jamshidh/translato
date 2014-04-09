<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
  <xsl:template match="@*|node()">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="input[attribute[@name='type' and @value='time']]">
    <shimLib name="addTimeInput" />
    <xsl:copy>
      <xsl:apply-templates select="attribute[@name != 'type']" />
      <attribute name='type' value='time2'/>
      <attribute name='onfocus' value='makeTimeControlAppear(this);'/>
      <attribute name='onblur' value='checkOnBlur()'/>
      <xsl:apply-templates select="*[name() != attribute]|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="body">
    <xsl:copy>
      <xsl:apply-templates select="*|@*|text()" />
      <div>
	<attribute name="tabindex" value="-1"/>
	<attribute name="id" value="timeInput"/>
	<table>
	  <tr>
	    <td><img>
	      <attribute name="onclick" value="addHour()" />
	      <attribute name="src" value="/res/addTimeInput/up.png" />
	    </img></td>
	    <td></td>
	    <td><img>
	      <attribute name="onclick" value="addMinute()" />
	      <attribute name="src" value="/res/addTimeInput/up.png" />
	    </img></td>
	    <td></td>
	    <td><img>
	      <attribute name="onclick" value="addSecond()" />
	      <attribute name="src" value="/res/addTimeInput/up.png" /></img>
	    </td>
	    <td></td>
	  </tr>
	  <tr>
	    <td>
	      <attribute name="id" value="hours"/>
	      <attribute name="class" value="timeInputNumber"/>
	      <text><word>12</word></text>
	    </td>
	    <td><text><word>:</word></text></td>
	    <td>
	      <attribute name="id" value="minutes"/>
	      <attribute name="class" value="timeInputNumber"/>
	      <text><word>45</word></text>
	    </td>
	    <td><text><word>:</word></text></td>
	    <td>
	      <attribute name="id" value="seconds"/>
	      <attribute name="class" value="timeInputNumber"/>
	      <text><word>00</word></text>
	    </td>
	    <td>
	      <button>
		<attribute name="id" value="amButton"/>
		<attribute name="class" value="selectedAMPM"/>
		<attribute name="type" value="button"/>
		<attribute name="onclick" value="setAM();"/>
		<text><word>AM</word></text>
		</button><br/>
		<button>
		  <attribute name="id" value="pmButton"/>
		  <attribute name="class" value="notselectedAMPM"/>
		  <attribute name="type" value="button"/>
		  <attribute name="onclick" value="setPM();"/>
		  <text><word>PM</word></text>
		</button>
	    </td>
	  </tr>
	  <tr>
	    <td><img>
	      <attribute name="onclick" value="subtractHour()" />
	      <attribute name="src" value="/res/addTimeInput/down.png" />
	    </img></td>
	    <td></td>
	    <td><img>
	      <attribute name="onclick" value="subtractMinute()" />
	      <attribute name="src" value="/res/addTimeInput/down.png" />
	    </img></td>
	    <td></td>
	    <td><img>
	      <attribute name="onclick" value="subtractSecond()" />
	      <attribute name="src" value="/res/addTimeInput/down.png" /></img>
	    </td>
	    <td></td>
	  </tr>
	</table>
      </div>
    </xsl:copy>
  </xsl:template>


</xsl:stylesheet>