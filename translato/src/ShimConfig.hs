{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module ShimConfig (
  ShimName(..),
  ShimConfig(..),
  Browser(..),
  Version,
  VersionRange(..),
  getAllShimConfigs,
  getShimFile,
  getShimConfig
) where

import Control.Exception.Base
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Conduit
import Data.Functor
import Data.List
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Directory
import System.FilePath
import Text.XML.Stream.Parse
import Data.XML.Types
import qualified Text.XML as XML

--import Control.Monad.Trans.Resource

import Paths_translato


import GrammarTools
import Libs
import Parser

newtype ShimName = ShimName T.Text deriving (Show, Ord, Eq)

data ShimConfig =
    ShimConfig {
      name::ShimName,  
      browsers::[Browser],
      dependencies::[ShimName],
      libs::[Lib]
      } deriving (Show)

data Browser =
    AllBrowsers
        |IE VersionRange
        | Mozilla VersionRange
        | Webkit VersionRange deriving (Show)

data VersionRange = AllVersions | Exact Version | LowerBound Version | UpperBound Version | Range Version Version deriving (Show)

type Version = [Int]

parseConfig::Sink Event (Either SomeException) (Maybe ShimConfig)
parseConfig = tagNoAttr "config" $ do
    browserList <- parseBrowsers
    dependList <- parseDependencies
    libList <- parseLibs
    return ShimConfig { 
      name=ShimName "", --name is just a placeholder, the real value is filled in getConfigFile
      browsers=concat $ maybeToList browserList, 
      libs=fmap name2Lib $ concat $ maybeToList libList,
      dependencies=map (ShimName . T.pack) (concat $ maybeToList dependList)
      } 

parseBrowsers::Sink Event (Either SomeException) (Maybe [Browser])
parseBrowsers = tagNoAttr "browsers" $ many parseBrowser

parseBrowser::ConduitM Event o (Either SomeException) (Maybe Browser)
parseBrowser = choose [parseAllBrowsers, parseIE, parseMozilla, parseWebkit]

parseLibs::Sink Event (Either SomeException) (Maybe [String])
parseLibs = tagNoAttr "libs" $ many parseLib

parseLib::ConduitM Event o (Either SomeException) (Maybe String)
parseLib = tagName "lib" (requireAttr "name") $ \attr -> do
  return $ T.unpack attr

parseDependencies::Sink Event (Either SomeException) (Maybe [String])
parseDependencies = tagNoAttr "dependencies" $ many parseDependency

parseDependency::ConduitM Event o (Either SomeException) (Maybe String)
parseDependency = tagName "shim" (requireAttr "name") $ \attr -> do
  return $ T.unpack attr

{-
parseDepends::Sink Event (Either SomeException) (Maybe [String])
parseDepends = tagNoAttr "dependencies" $ many parseLib

parseDepend::ConduitM Event o (Either SomeException) (Maybe String)
parseDepend = tagName "dependency" (requireAttr "name") $ \attr -> do
  return $ T.unpack attr
-}

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

getShimFile::FilePath->ShimName->String->FilePath
getShimFile shimDir (ShimName shim) filename = shimDir </> T.unpack shim </> filename

getShimConfig::FilePath->ShimName->IO ShimConfig
getShimConfig shimDir shimName = do
    grammar <- loadGrammarAndSimplifyForParse =<< getDataFileName "config.spec"
    let filename = getShimFile shimDir shimName "config"
    parserOutput <- TL.pack <$> createParser grammar <$> TL.readFile filename
    case XML.parseText XML.def parserOutput of
        Right doc -> do
            let doc' = B.pack $ TL.unpack $ XML.renderText XML.def doc
            --runResourceT $
            case parseLBS def doc' $$ force "config required" parseConfig of
                Left err ->
                    error $ ("There is an error: " ++ show err ++ ", file content is\n-------\n" ++ TL.unpack (XML.renderText XML.def doc) ++ "\n-------\n")
                Right config -> return config{name=shimName}
        Left err -> error $ "Error parsing config file '" ++ filename ++ "'\n------\n" ++ show err ++ "\n----------\nparser output:\n" ++ TL.unpack parserOutput

getAllShimNames::FilePath->IO [ShimName]
getAllShimNames shimDir = do
  map (ShimName . T.pack) 
    <$> (filterM (doesDirectoryExist . (shimDir </>)) =<< filter (not . ("." `isPrefixOf`)) <$> getDirectoryContents shimDir)

getAllShimConfigs::FilePath->IO [ShimConfig]
getAllShimConfigs shimDir = do
  shimNames <- getAllShimNames shimDir
  sequence $ getShimConfig shimDir <$> shimNames
  
  