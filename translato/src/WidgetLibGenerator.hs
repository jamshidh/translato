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

versionInRange::Version->VersionRange->Bool
versionInRange v1 (Exact v2) = v1 == v2
versionInRange v1 (LowerBound v2) = v1 >= v2
versionInRange v1 (UpperBound v2) = v1 <= v2
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

isShimEligible::UAResult->FilePath->IO Bool
isShimEligible userAgent shimFilePath = do
    config <- getConfigFile shimFilePath
    --putStrLn $ show config
    return $ or $ uaInBrowserRange userAgent <$> browsers config

getWidgetNames::FilePath->IO [String]
getWidgetNames shimDir = do
    nub <$> sort <$> map takeBaseName <$> trace "abcdabcd" <$>
        Find.find (depth <=? 2) (depth ==? 2 &&? extension ==? ".widget") shimDir

getDirectoryFilePathContents::FilePath->IO [FilePath]
getDirectoryFilePathContents x = map (x </>) <$> getDirectoryContents x

getNeededShims::String->FilePath->IO [FilePath]
getNeededShims userAgentString shimDir = do
    uaParser <- loadUAParser
    let userAgent = 
          case parseUA uaParser $ B.fromString userAgentString of
            Just x -> x
            Nothing -> error $ "Malformed userAgent: " ++ userAgentString
    filterM (isShimEligible userAgent) =<< getShimNames shimDir

getWidgetLibContent::FilePath->String->String->IO (Maybe String, Maybe String)
getWidgetLibContent shimDir userAgentString widgetName = do
    neededShims <- getNeededShims userAgentString shimDir
    putStrLn ("Needed shims: " ++ show (takeBaseName <$> neededShims))
    widgetFiles <-
        filter ((widgetName ++ ".widget" ==) . takeFileName)
            <$> concat
            <$> (sequence $ getDirectoryFilePathContents <$> neededShims)
    trace ("widgetFiles: " ++ show widgetFiles) $ return ()
    case widgetFiles of
        [] -> return (Nothing, Nothing)
        _ -> do
            contents <- sequence $ XML.readFile XML.def <$> fromString <$> widgetFiles
            let widget = fold $ reverse $ xml2Widget <$> XML.documentRoot <$> contents
            content <- widget2js widgetName $ format $ widget
            let cssContent = style widget
            return (if length widgetFiles == 0 then Nothing else Just content, cssContent)











