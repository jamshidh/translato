{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module WidgetConfig (
    WidgetConfig(..),
    Browser(..),
    Version,
    VersionRange(..),
    getConfigFile
) where

import Control.Exception.Base
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Conduit
import Data.Functor
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.FilePath
import Text.XML.Stream.Parse
import Data.XML.Types
import qualified Text.XML as XML

--import Control.Monad.Trans.Resource

import Paths_translato


import GrammarTools
import Parser

data WidgetConfig =
    WidgetConfig {
        browsers::[Browser]
    } deriving (Show)

data Browser =
    AllBrowsers
        |IE VersionRange
        | Mozilla VersionRange
        | Webkit VersionRange deriving (Show)

data VersionRange = AllVersions | Exact Version | LowerBound Version | UpperBound Version | Range Version Version deriving (Show)

type Version = [Int]

parseConfig::Sink Event (Either SomeException) (Maybe WidgetConfig)
parseConfig = tagNoAttr "config" $ do
    browserList <- parseBrowsers
    case browserList of
        Just x -> return WidgetConfig { browsers=x }
        _ -> return WidgetConfig { browsers=[] }

parseBrowsers::Sink Event (Either SomeException) (Maybe [Browser])
parseBrowsers = tagNoAttr "browsers" $ many parseBrowser

parseBrowser::ConduitM Event o (Either SomeException) (Maybe Browser)
parseBrowser = choose [parseAllBrowsers, parseIE, parseMozilla, parseWebkit]

parseAllBrowsers::ConduitM Event o (Either SomeException) (Maybe Browser)
parseAllBrowsers = tagNoAttr "allBrowsers" $ do
    return AllBrowsers

parseIE::ConduitM Event o (Either SomeException) (Maybe Browser)
parseIE = tagNoAttr "ie" $ do
    maybeVersionRange <- parseVersionRange
    case maybeVersionRange of
        Just versionRange -> return $ IE versionRange
        Nothing -> return $ IE AllVersions

parseMozilla::ConduitM Event o (Either SomeException) (Maybe Browser)
parseMozilla = tagNoAttr "mozilla" $ do
    maybeVersionRange <- parseVersionRange
    case maybeVersionRange of
        Just versionRange -> return $ Mozilla versionRange
        Nothing -> return $ Mozilla AllVersions

parseWebkit::ConduitM Event o (Either SomeException) (Maybe Browser)
parseWebkit = tagNoAttr "webkit" $ do
    maybeVersionRange <- parseVersionRange
    case maybeVersionRange of
        Just versionRange -> return $ Webkit versionRange
        Nothing -> return $ Webkit AllVersions

parseVersionRange::ConduitM Event o (Either SomeException) (Maybe VersionRange)
parseVersionRange = choose [parseUpperBound, parseLowerBound, parseExact, parseRange]

parseUpperBound::ConduitM Event o (Either SomeException) (Maybe VersionRange)
parseUpperBound = tagNoAttr "upperBound" $ do
    ver <- force "version required" $ parseVersion
    return $ UpperBound ver

parseLowerBound::ConduitM Event o (Either SomeException) (Maybe VersionRange)
parseLowerBound = tagNoAttr "lowerBound" $ do
    ver <- force "version required" $ parseVersion
    return $ LowerBound ver

parseExact::ConduitM Event o (Either SomeException) (Maybe VersionRange)
parseExact = tagNoAttr "exact" $ do
    ver <- force "version range required" $ parseVersion
    return $ Exact ver

parseRange::ConduitM Event o (Either SomeException) (Maybe VersionRange)
parseRange = tagNoAttr "exact" $ do
    LowerBound lower <- force "lower bound required" $ parseLowerBound
    UpperBound upper <- force "upper bound required" $ parseUpperBound
    return $ Range lower upper

parseVersion::ConduitM Event o (Either SomeException) (Maybe Version)
parseVersion = tagNoAttr "version" $ many parseInteger

parseInteger::ConduitM Event o (Either SomeException) (Maybe Int)
parseInteger = tagNoAttr "integer" $ do
    intValue <- content
    return $ read $ T.unpack intValue

getConfigFile::FilePath->IO WidgetConfig
getConfigFile shimDir = do
    grammar <- loadGrammarAndSimplifyForParse =<< getDataFileName "config.spec"
    maybeDoc <- XML.parseText XML.def <$> TL.pack <$> createParser grammar <$> readFile (shimDir </> "config")
    case maybeDoc of
        Right doc -> do
            let doc' = B.pack $ TL.unpack $ XML.renderText XML.def doc
            --runResourceT $
            case parseLBS def doc' $$ force "config required" parseConfig of
                Left err ->
                    error $ ("There is an error: " ++ show err ++ ", file content is " ++ show (XML.renderText XML.def doc))
                Right config -> return config
        Left err -> error $ show err
