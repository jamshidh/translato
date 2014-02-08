{-# OPTIONS_GHC -Wall #-}

module WidgetLibGenerator (
    getWidgetNames,
    getWidgetLibContent
) where

import Data.Foldable hiding (concat)
import Data.Functor
import Data.List
import Data.String
import System.FilePath
import System.FilePath.Find as Find hiding (fold)
import qualified Text.XML as XML

import Format
import Widget
import WidgetFormatter ()
import WidgetJSLibrary
import WidgetMerger ()
import WidgetParser

import Debug.Trace

getWidgetNames::FilePath->IO [String]
getWidgetNames shimDir = do
    nub <$> sort <$> map takeBaseName <$> trace "abcdabcd" <$>
        Find.find (depth <=? 2) (depth ==? 2 &&? extension ==? ".widget") shimDir

getWidgetLibContent::FilePath->String->IO (Maybe String, Maybe String)
getWidgetLibContent shimDir widgetName = do
    widgetFiles <- trace ("shimDir: " ++ shimDir ++ ", widgetName = " ++ widgetName) $
        Find.find (depth <=? 2) (depth ==? 2 &&? fileName ==? (widgetName <.> "widget")) shimDir
    trace ("widgetFiles: " ++ show widgetFiles) $ return ()
    contents <- sequence $ XML.readFile XML.def <$> fromString <$> widgetFiles
    let widget = fold $ reverse $ xml2Widget <$> XML.documentRoot <$> contents
    content <- widget2js widgetName $ format $ widget
    let cssContent = style widget
    return (if length widgetFiles == 0 then Nothing else Just content, cssContent)











