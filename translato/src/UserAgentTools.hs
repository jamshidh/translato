
module UserAgentTools (
  getRealUserAgent
) where


--This is just a convenience wrapper to keep me from having to type long user agents in on the command line"
getRealUserAgent::String->String
getRealUserAgent "ie8" = "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0)"
getRealUserAgent x = x
