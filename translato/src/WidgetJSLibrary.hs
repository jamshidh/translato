
module WidgetJSLibrary (
    widget2js
) where

import System.Process

import Paths_translato

import Widget



widget2js::String->String->IO String
widget2js widgetName widgetString = do
    stylesheet <- getDataFileName "widget2js.xsl"
    doXsltXform stylesheet widgetName widgetString


doXsltXform::FilePath->String->String->IO String
doXsltXform xsltFilePath widgetName input = do
  ret <- readProcess "xmlstarlet" ["tr", xsltFilePath, "-s", "widgetName=" ++ widgetName, "-"] input
  return ret


main = undefined
