{-# LANGUAGE DeriveGeneric #-}

module WidgetConfig (
    WidgetConfig(..),
    Filter(..),
    getConfigFile
) where

import Data.Aeson
import Data.Data
import Data.Functor
import qualified Data.Text.Lazy as T
import Data.Typeable
import GHC.Generics
import qualified Text.XML as XML

import Paths_translato

import GrammarTools
import Parser

data Filter = UserAgent String | Dog Int deriving (Generic)

data WidgetConfig =
    WidgetConfig {
        filters::[Filter]
    } deriving (Generic)

instance FromJSON Filter

instance FromJSON WidgetConfig

instance ToJSON Filter
instance ToJSON WidgetConfig

getConfigFile::FilePath->IO XML.Document
getConfigFile configFileName = do
    grammar <- loadGrammarAndSimplifyForParse =<< getDataFileName "config.spec"
    maybeDoc <- XML.parseText XML.def <$> T.pack <$> createParser grammar <$> readFile configFileName
    return $
        case maybeDoc of
            Right doc -> doc
            Left err -> error $ show err
