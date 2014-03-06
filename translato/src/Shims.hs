{-# OPTIONS_GHC -Wall #-}

module Shims (
  applyShims
) where

import Control.Monad
import Data.Functor
import System.Directory
import System.FilePath
import System.Process

import Parser
import WidgetLibGenerator

applyShims::FilePath->String->String->String->IO String
applyShims shimDir specName userAgent input = do
  neededShimDirs <- getNeededShims userAgent shimDir

  translators <- map doXsltXform <$> (filterM doesFileExist $ (</> "translate.xsl") <$> neededShimDirs)
    
  --This chains together functions in a list [a, b, c, d]
  --using (>>=), like this ">>= a >>= b >>= c >>= d"
  let allTranslatorsTogether =
        if length translators == 0
        then id
        else foldl1 (.) $ (=<<) <$> translators
    
  allTranslatorsTogether $ parseUsingSpecName specName input

doXsltXform::FilePath->String->IO String
doXsltXform xsltFilePath = readProcess "xmlstarlet" ["tr", xsltFilePath, "-"]

