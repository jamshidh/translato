{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}

module WidgetConfig (
    WidgetConfig(..),
    getConfigFile
) where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Functor
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.XML.Stream.Parse
import qualified Text.XML as XML

import Control.Monad.Trans.Resource
import Data.Conduit (($$))
import Data.Text (Text, unpack)
import Text.XML.Stream.Parse
import Paths_translato

import GrammarTools
import Parser

data WidgetConfig =
    WidgetConfig {
        browsers::[Browser]
    }

data Browser = IE VersionRange | Mozilla VersionRange | Webkit VersionRange deriving (Show)

data VersionRange = Exact Version | LowerBound Version | UpperBound Version | Range Version Version deriving (Show)

type Version = [Int]

parseConfig = tagNoAttr "config" parseBrowsers

parseBrowsers = tagNoAttr "browsers" $ many parseBrowser

parseBrowser = choose [parseIE, parseMozilla, parseWebkit]

parseIE = tagNoAttr "ie" $ do
    Just versionRange <- parseVersionRange
    return $ IE versionRange

parseMozilla = tagNoAttr "mozilla" $ do
    Just versionRange <- parseVersionRange
    return $ Mozilla versionRange

parseWebkit = tagNoAttr "webkit" $ do
    Just versionRange <- parseVersionRange
    return $ Webkit versionRange

parseVersionRange = choose [parseUpperBound, parseLowerBound, parseExact, parseRange]

parseUpperBound = tagNoAttr "upperBound" $ do
    Just version <- parseVersion
    return $ UpperBound version

parseLowerBound = tagNoAttr "lowerBound" $ do
    Just version <- parseVersion
    return $ LowerBound version

parseExact = tagNoAttr "exact" $ do
    Just version <- parseVersion
    return $ Exact version

parseRange = tagNoAttr "exact" $ do
    Just (LowerBound lower) <- parseLowerBound
    Just (UpperBound upper) <- parseUpperBound
    return $ Range lower upper

parseVersion = tagNoAttr "version" $ many parseInteger

parseInteger = tagNoAttr "integer" $ do
    intValue <- content
    return $ read $ T.unpack intValue

xml2WidgetConfig::XML.Document->WidgetConfig
xml2WidgetConfig doc =
  WidgetConfig {
    browsers=[]
  }

getConfigFile::FilePath->IO XML.Document
getConfigFile configFileName = do
    grammar <- loadGrammarAndSimplifyForParse =<< getDataFileName "config.spec"
    maybeDoc <- XML.parseText XML.def <$> TL.pack <$> createParser grammar <$> readFile configFileName
    case maybeDoc of
        Right doc -> do
            let content = TL.unpack (XML.renderText XML.def doc)
            putStrLn content
            config <- runResourceT $
                parseLBS def (B.pack content) $$ force "config required" parseConfig
            print config

    return $
        case maybeDoc of
            Right doc -> doc
            Left err -> error $ show err
