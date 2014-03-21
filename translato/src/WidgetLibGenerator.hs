{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module WidgetLibGenerator (
    getWidgetNames,
    getNeededShims,
    getWidgetLibContent
    ) where

import Control.Monad
import qualified Data.ByteString.UTF8 as B
import Data.Foldable hiding (concat, and, or)
import Data.Functor
import Data.List
import Data.String
import qualified Data.Text as T
import System.Directory
import System.FilePath
import System.FilePath.Find as Find hiding (fold)
import qualified Text.XML as XML
import Web.UAParser

import Format
import Widget
import ShimConfig
import WidgetFormatter ()
import WidgetJSLibrary
import WidgetMerger ()
import WidgetParser

--import Debug.Trace


getShimNames::FilePath->IO [ShimName]
getShimNames shimDir = do
  map (ShimName . T.pack) <$> (  
    filterM (doesDirectoryExist . (shimDir </>))
        =<< filter (not . ("." `isPrefixOf`))
        <$> getDirectoryContents shimDir)

versionInRange::Version->VersionRange->Bool
versionInRange v1 (Exact v2) = v1 == v2
versionInRange v1 (LowerBound v2) = v1 >= v2
versionInRange v1 (UpperBound v2) = v1 < v2
versionInRange v1 (Range v2 v3) = v1 >= v2 && v1 <= v3
versionInRange _ AllVersions = True

uaResultToVersion::UAResult->Version
uaResultToVersion UAResult{uarV1=Just v1, uarV2=Just v2, uarV3=Just v3} =
    read <$> T.unpack <$> [v1, v2, v3]
uaResultToVersion UAResult{uarV1=Just v1, uarV2=Just v2, uarV3=Nothing} =
    read <$> T.unpack <$> [v1, v2]
uaResultToVersion UAResult{uarV1=Just v1, uarV2=Nothing, uarV3=Nothing} =
    read <$> T.unpack <$> [v1]
uaResultToVersion uaResult =
    error ("Error: An odd parameter was passed to uaResultToVersion: " ++ show uaResult)

uaInBrowserRange::UAResult->Browser->Bool
uaInBrowserRange _ AllBrowsers = True
uaInBrowserRange uaResult@UAResult{uarFamily="Firefox"} (Mozilla versionRange) =
    versionInRange (uaResultToVersion uaResult) versionRange
uaInBrowserRange uaResult@UAResult{uarFamily="Chrome"} (Webkit versionRange) =
    versionInRange (uaResultToVersion uaResult) versionRange
uaInBrowserRange uaResult@UAResult{uarFamily="IE"} (IE versionRange) =
    versionInRange (uaResultToVersion uaResult) versionRange
uaInBrowserRange _ _ = False

isShimEligible::FilePath->UAResult->ShimName->IO Bool
isShimEligible shimDir userAgent shimName = do
    config <- getShimConfig shimDir shimName
    return $ or $ uaInBrowserRange userAgent <$> browsers config

getWidgetNames::FilePath->IO [String]
getWidgetNames shimDir = do
    nub <$> sort <$> map takeBaseName <$> 
        Find.find (depth <=? 2) (depth ==? 2 &&? extension ==? ".widget") shimDir

getNeededShims::String->FilePath->IO [ShimName]
getNeededShims userAgentString shimDir = do
  uaParser <- loadUAParser
  
  let userAgent = 
          case parseUA uaParser $ B.fromString userAgentString of
            Just x -> x
            Nothing -> error $ "Malformed userAgent: " ++ userAgentString
  
  filterM (isShimEligible shimDir userAgent) =<< getShimNames shimDir

rootShimFiles::FilePath->ShimName->IO [FilePath]
rootShimFiles shimDir (ShimName shim) = do
  let shimNameString = T.unpack shim
  map ((shimDir </> shimNameString)  </>) <$> getDirectoryContents (shimDir </> shimNameString)

getWidgetLibContent::FilePath->String->String->IO (Maybe String, Maybe String)
getWidgetLibContent shimDir userAgentString widgetName = do
    neededShims <- getNeededShims userAgentString shimDir
    widgetFiles <-
        filter ((takeBaseName widgetName ++ ".widget" ==) . takeFileName)
            <$> concat
            <$> (sequence $ rootShimFiles shimDir <$>  neededShims)
    case widgetFiles of
      [] -> return (Nothing, Nothing)
      _ -> do
            contents <- sequence $ XML.readFile XML.def <$> fromString <$> widgetFiles
            let widget = fold $ reverse $ xml2Widget <$> XML.documentRoot <$> contents
            content <- widget2js (takeBaseName widgetName) $ format $ widget
            let cssContent = style widget
            return (if length widgetFiles == 0 then Nothing else Just content, cssContent)











