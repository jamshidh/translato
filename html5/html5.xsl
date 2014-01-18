<?xml version="1.0" ?>

<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

  <xsl:output method="html" /> 


  <xsl:template match="@*|node()|text()">
    <xsl:copy>
      <xsl:apply-templates select="@*|node()|text()" />
    </xsl:copy>
  </xsl:template>

  <xsl:template match="head">
    <xsl:variable name="progressbar" select="count(//progress) > 0" />
    <xsl:variable name="detailssummary" select="count(//details) > 0" />
    <xsl:variable name="audio" select="count(//details) > 0" />
    <xsl:variable name="video" select="count(//video) > 0" />
    <xsl:variable name="dateinput" select="count(//input[@type='date']) > 0" />
    <xsl:variable name="datetimeinput" select="count(//input[@type='datetime']) > 0" />
    <xsl:variable name="telephoneinput" select="count(//input[@type='tel']) > 0" />
    <xsl:variable name="timeinput" select="count(//input[@type='time']) > 0" />
    <xsl:variable name="colorinput" select="count(//input[@type='color']) > 0" />
    <xsl:variable name="validate" select="count(//@required) > 0" />
    <xsl:variable name="placeholder" select="count(//@placeholder) > 0" />
    <xsl:variable name="datalist" select="count(//datalist) > 0" />
    <xsl:variable name="rangeinput" select="count(//input[@type='range']) > 0" />
    <xsl:variable name="canvas" select="count(//canvas) > 0" />
    <xsl:variable name="article" select="count(//article) > 0" />

    <xsl:copy>
      
      <xsl:apply-templates select="*" />

      <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"></script>
      
      <xsl:if test="$article">
	<xsl:comment>[if lt IE 9]&gt;&lt;script src="http://html5shiv.googlecode.com/svn/trunk/html5.js"&gt;&lt;/script&gt;&lt;![endif]</xsl:comment>
      </xsl:if>
      
      <xsl:if test="$canvas">
	<xsl:comment>[if IE]&gt;&lt;script src="lib/excanvas.js"&gt;&lt;/script&gt;&lt;![endif]</xsl:comment>
      </xsl:if>
      
      <xsl:if test="$detailssummary">
	<style>
	  details, summary { display: block; border: 1px solid #666; padding: 1em; }
	  /* Apply a pointer cursor and style the background upon hover to indicate &lt;summary&gt; is a clickable element. */
	  /* These styles can be applied regardless of whether the fallback is needed */
	  summary { cursor: pointer; }
	  summary:hover, summary:focus { background: #ddd; }
	  
	  /* The following styles are not really needed, since the jQuery script takes care of hiding/displaying the elements. */
	  /* However, we’re still gonna use CSS as well to prevent FOUC in browsers that understand these selectors. */
	  /* Remember: by default (and probably most of the time), the contents of the &lt;details&gt; element are hidden. */
	  .no-details details > * { display: none; }
	  /* This doesn’t work very well in Firefox 3.6.x */
	  /* .no-details details[open] > * { display: block; } */
	  /* The following doesn’t toggle correctly in WebKit:
	  .no-details details > summary:before { content: '► '; }
	  .no-details details[open] > summary:before { content: '▼ '; }
	  */
	  /* And yes, it should really be ::before, but that doesn’t work in IE8 */
	  .no-details details > summary:before { float: left; width: 20px; content: '► '; }
	  .no-details details.open > summary:before { content: '▼ '; }
	  /* For IE6 and IE7, who don’t support generated content, you could use padding-left + a background image instead */
	  /* I really couldn’t be bothered though. */
	  /*
	  .no-details details > summary { padding-left: 20px; background: url(img/arrow-sprite.png) no-repeat 0 0; }
	  .no-details details.open > summary { background-position: 0 -20px; }
	  */
	  /* Make sure summary remains visible */
	  .no-details details summary { display: block; }
	</style>
      </xsl:if>

      <xsl:if test="$datalist">
	<style>
	  
	  div.autocomplete {
	  position: absolute;
	  width: 250px; /* will be adjusted by script.aculo.us */
	  background-color: white; border: 1px solid #888;
	  margin: 0px; padding: 0px;
	  }
	  div.autocomplete ul {
	  list-style-type: none; margin: 0px; padding: 0px;
	  }
	  
	  div.autocomplete ul li.selected { background-color: #ff9;}
	  div.autocomplete ul li {
	  list-style-type: none; display: block;
	  font-family: sans-serif; font-size: small; color: #444;
	  margin: 0; padding: 0.1em;
	  line-height: 1.5em;
	  cursor: pointer;
	  }
	  
	  div.dataListDisabledMsg  { position:absolute;padding:2px;background:#ff9;border:1px solid #888;font:400 8pt Tahoma; }
	  div.dataListActivityIndicator { width:6px;height:6px;background:red;overflow:hidden; }
	  
	</style>
      </xsl:if>
      

      <xsl:if test="$video">
	<script src="lib/flowplayer/flowplayer-3.2.10.min.js"></script>
      </xsl:if>
      
      <xsl:if test="$colorinput">
	<script type="text/javascript" src="lib/jscolor/jscolor.js"></script>
      </xsl:if>

      <xsl:if test="$rangeinput">

	<style>                                                                                                                                                                                                     body {
	   padding:40px;
	 }
	 slider {
	   position:relative;
	   cursor:pointer;
	   height:1px;
	   border:2px solid #00118E;
	   width:658px;
	   -moz-border-radius:2px;
	   border-radius:2px;
	 }                                                                                                                                                                                                          .handle {                                                                                                                                                                                                    border:1px solid #cfcfcf;
	   background-color:#fff;
	   height:20px;
	   width:80px;
	   position:absolute;
	   top:-12px;
	   display:block;
	   cursor:move;
	   -moz-border-radius:14px;
	   -webkit-border-radius:14px;
	   border-radius:14px;
	 }
	 .handle:active {
	   background:blue;
	 }
	 .range {
	   display:none;
	 }
	</style>

	<script type="text/javascript" src="lib/rangeinput.js"></script>
      </xsl:if>

      <!--script src="http://cdn.jquerytools.org/1.2.7/full/jquery.tools.min.js"></script    -->
      <link rel="stylesheet" type="text/css" href="css/skin1.css"/>
      
      <xsl:if test="$progressbar or $datetimeinput">
	<link href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/themes/base/jquery-ui.css" rel="stylesheet" type="text/css"/> <!-- Progress bar -->
      </xsl:if>

      <!--script src="http://ajax.googleapis.com/ajax/libs/jquery/1.5/jquery.min.js"></script -->
      <!--script src="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8/jquery-ui.min.js"></script--> <!-- Progress bar and slider -->
      <link rel="stylesheet" href="css/default.css" type="text/css" media="screen" />

      <xsl:if test="$dateinput">
	<style type="text/css">
	  @import "css/jquery.datepick.css";

	</style>
	
	<script type="text/javascript" src="lib/jquery.datepick.js"></script>
      </xsl:if>

      <xsl:if test="$datetimeinput">
	<script type="text/javascript" src="lib/jquery-ui-timepicker-addon.js"></script>
      </xsl:if>

      <script src="lib/jquery.dataset.js"></script>

      <xsl:if test="$telephoneinput or $timeinput">
	<script src="lib/jquery.maskedinput-1.3.min.js"></script>
      </xsl:if>

      <xsl:if test="$validate or placeholder">
	<script src="lib/jquery.validate.js"></script>

	<link href="css/style01.css" media="screen" rel="stylesheet" type="text/css" />
	<script src="lib/jquery.h5validate.js"></script> 
      </xsl:if>
 

      <xsl:if test="$datalist">
	<script src="lib/prototype.js" type="text/javascript"></script>
	<script src="lib/effects.js" type="text/javascript"></script>
	<script src="lib/controls.js" type="text/javascript"></script>
	<script src="lib/data.js" type="text/javascript"></script>
	<script src="lib/code_highlighter.js" type="text/javascript"></script>
	<script src="lib/datalist.js" type="text/javascript"></script>
      </xsl:if>


      <script>
	jQuery(document).ready(function() {     
	  <xsl:if test="$datetimeinput">
	    jQuery(".datetimepicker").datetimepicker();
          
	  </xsl:if>
	  <xsl:if test="$telephoneinput">
	    jQuery(".tel").mask("(999) 999-9999");

	  </xsl:if>
	  <xsl:if test="$timeinput">
	    jQuery(".time").mask("99:99");

	  </xsl:if>
	  <xsl:if test="$progressbar">
	    jQuery(".progressbar").progressbar({ value: 37 });

	  </xsl:if>
	  <!--xsl:if test="$rangeinput">
	    jQuery(":range").rangeinput();

	  </xsl:if    -->
	  <xsl:if test="$validate or $placeholder">
	    jQuery('form.h5-defaults').h5Validate();

	  </xsl:if>
	});
                                                    
      </script>

      <xsl:for-each select="//datalist">
	<script type="text/javascript">
	  var <xsl:value-of select="@id" />={<xsl:for-each select="option"> "<xsl:value-of select="@value" />":"<xsl:value-of select="@value" />", </xsl:for-each>};

	</script>
      </xsl:for-each>

    </xsl:copy>
  </xsl:template>

  <xsl:template match="audio">
    <object type="application/x-shockwave-flash" data="lib/player_mp3_maxi.swf" width="200" height="20">
      <param name="movie" value="lib/player_mp3_maxi.swf" />
      <param name="FlashVars" value="{concat('mp3=', @mp3)}" />
    </object>
  </xsl:template>

  <xsl:template match="video">
    <a href="{@src}"
       style="display:block;
width:425px;
height:300px;
"
       id="player">
    </a>
    <script language="JavaScript">
      flowplayer("player", "lib/flowplayer/flowplayer-3.2.11.swf");

    </script>
  </xsl:template>

  <xsl:template match="input[@type='date']">
      <input type="text" id="{generate-id()}" class="datepicker" />
      <script>
	jQuery('#<xsl:value-of select="generate-id()" />').datepick({minDate:'<xsl:value-of select="@min" />', maxDate:'<xsl:value-of select="@max" />'});

      </script>
  </xsl:template>

  <xsl:template match="input[@type='datetime']">
      <input type="text" class="datetimepicker" />
  </xsl:template>

  <xsl:template match="input[@type='datetime-local']"><!-- todo -->
    <div class="demo">

      <input type="text" class="datepicker" />

      </div><!-- End demo -->
  </xsl:template>

  <xsl:template match="input[@type='month']"><!-- todo -->
    <div class="demo">

      <input type="text" class="datepicker" />

      </div><!-- End demo -->
  </xsl:template>

  <xsl:template match="input[@type='email']"><!-- todo -->
    <div class="demo">

      <input type="text" class="datepicker" />

      </div><!-- End demo -->
  </xsl:template>

  <xsl:template match="input[@type='range']">
      <input type="range" id="{generate-id()}" />
      <script>
	jQuery('#<xsl:value-of select="generate-id()" />').rangeinput();

      </script>
  </xsl:template>

  <xsl:template match="input[@type='number']"><!-- todo -->
    <div class="demo">

      <input type="text" class="datepicker" />

      </div><!-- End demo -->
  </xsl:template>

  <xsl:template match="input[@type='search']"><!-- todo -->
    <div class="demo">

      <input type="text" class="datepicker" />

      </div><!-- End demo -->
  </xsl:template>

  <xsl:template match="input[@type='tel']">
      <input type="text" class="tel" />
  </xsl:template>

  <xsl:template match="input[@type='time']">
      <input type="text" class="time" />
  </xsl:template>

  <xsl:template match="input[@type='url']"><!-- todo -->
    <div class="demo">

      <input type="text" class="datepicker" />

      </div><!-- End demo -->
  </xsl:template>

  <xsl:template match="input[@type='week']"><!-- todo -->
    <div class="demo">

      <input type="text" class="datepicker" />

      </div><!-- End demo -->
  </xsl:template>
                                          
  <xsl:template match="input[@type='color']">
    <input class="color" />
  </xsl:template>



  <xsl:template match="progress">
    <div class="progressbar" min="{@min}" max="{@max}"></div>
  </xsl:template>


  <xsl:template match="input[@list]">

    <div id="parentContainer">
      <p>
	<input type="text" class="datalist" value="" /><img src="resources/dropdown_simple.gif" />
      </p>
      <div style="display:none;
" class="autocomplete" id="autocomplete_choices"></div>
    </div>
    
    <script type="text/javascript">
      

      document.observe( 'dom:loaded',function(){
      jQuery('.datalist').each(function(index, item) {
      item.datalist(<xsl:value-of select="@list" />,{ },{ width: '250px' });

      });

      DatalistMouseWheel.on('alt_autocomplete_choices');

      });




    </script>


  </xsl:template>

  <xsl:template match="body">
    <xsl:variable name="detailssummary" select="count(//details) > 0" />

    <xsl:copy>
      <xsl:apply-templates select="*" />
      <!--script src="http://ajax.googleapis.com/ajax/libs/jquery/1/jquery.min.js"></script -->
      
      <xsl:if test="$detailssummary">
	<script src="lib/jquery.details.min.js?v=0.0.6"></script>
	<script>
	  window.console || (window.console = { 'log': alert });
	  jQuery(function() {
	  
	  // Add conditional classname based on support
	  jQuery('html').addClass(jQuery.fn.details.support ? 'details' : 'no-details');
	  
	  // Show a message based on support
	  jQuery('body').prepend(jQuery.fn.details.support ? 'Native support detected; the plugin will only add ARIA annotations and fire custom open/close events.' : 'Emulation active; you are watching the plugin in action!');
	  
	  // Emulate &lt;details&gt; where necessary and enable open/close event handlers
	  jQuery('details').details();
	  
	  // Bind some example event handlers
	  jQuery('details').on({
	  'open.details': function() {
	  console.log('opened');
	  },
	  'close.details': function() {
	  console.log('closed');
	  }
	  });
	  
	  });
	</script>
      </xsl:if>
    </xsl:copy>
  </xsl:template>


</xsl:stylesheet>