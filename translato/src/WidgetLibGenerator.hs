{-# OPTIONS_GHC -Wall #-}

module WidgetLibGenerator (
    getWidgetNames,
    getWidgetLibContent
) where

import Control.Monad
import Data.Foldable hiding (concat)
import Data.Functor
import Data.List
import Data.String
import qualified Data.Text.Lazy as T
import System.Directory
import System.FilePath
import System.FilePath.Find as Find hiding (fold)
import qualified Text.XML as XML

import Format
import Widget
import WidgetConfig
import WidgetFormatter ()
import WidgetJSLibrary
import WidgetMerger ()
import WidgetParser

import Debug.Trace

getShimNames::FilePath->IO [FilePath]
getShimNames shimDir =
    filterM doesDirectoryExist
        =<< map (shimDir </>)
        <$> filter (not . ("." `isPrefixOf`))
        <$> getDirectoryContents shimDir

filterIneligibleShims::String->[FilePath]->IO [FilePath]
filterIneligibleShims _ [] = return []
filterIneligibleShims userAgent (first:rest) = do
    configFile <- getConfigFile (first </> "config")
    putStrLn "----------------"
    putStrLn $ T.unpack (XML.renderText XML.def configFile)
    putStrLn "----------------"
    remainder <- filterIneligibleShims userAgent rest
    return $ first:remainder

getWidgetNames::FilePath->IO [String]
getWidgetNames shimDir = do
    nub <$> sort <$> map takeBaseName <$> trace "abcdabcd" <$>
        Find.find (depth <=? 2) (depth ==? 2 &&? extension ==? ".widget") shimDir

getDirectoryFilePathContents::FilePath->IO [FilePath]
getDirectoryFilePathContents x = map (x </>) <$> getDirectoryContents x

getWidgetLibContent::FilePath->String->String->IO (Maybe String, Maybe String)
getWidgetLibContent shimDir widgetName userAgent = do
    putStrLn userAgent
    neededShims <- filterIneligibleShims userAgent =<< getShimNames shimDir
    widgetFiles <-
        filter ((".widget" ==) . takeExtension)
            <$> concat
            <$> (sequence $ getDirectoryFilePathContents <$> neededShims)
    trace ("widgetFiles: " ++ show widgetFiles) $ return ()
    contents <- sequence $ XML.readFile XML.def <$> fromString <$> widgetFiles
    let widget = fold $ reverse $ xml2Widget <$> XML.documentRoot <$> contents
    content <- widget2js widgetName $ format $ widget
    let cssContent = style widget
    return (if length widgetFiles == 0 then Nothing else Just content, cssContent)











