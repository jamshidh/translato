{-# OPTIONS_GHC -Wall #-}

module Translator (
  applyShims
) where

import Control.Monad
import Data.Functor
import System.Directory
import System.FilePath
import System.Process

import Paths_translato

import Parser
import WidgetLibGenerator

applyShims::String->String->String->IO String
applyShims specName userAgent input = do
  
  shimDir <- (</> "GlowApps" </> "html5" </> "shims") <$> getHomeDirectory

  neededShimDirs <- getNeededShims userAgent shimDir

  translators <- map doXsltXform <$> (filterM doesFileExist $ (</> "translate.xsl") <$> neededShimDirs)
    
  --putStrLn ("There are " ++ show (length translators) ++ " translators")
    
  --This chains together functions in a list [a, b, c, d]
  --using (>>=), like this ">>= a >>= b >>= c >>= d"
  let allTranslatorsTogether =
        if length translators == 0
        then id
        else foldl1 (.) $ (=<<) <$> translators
    
  reorganizerFile <- getDataFileName "reorganize.xsl"

  (allTranslatorsTogether $ (parseUsingSpecName specName input))
        >>= (doXsltXform reorganizerFile)
        >>= readProcess "parser" ["generate", specName]

doXsltXform::FilePath->String->IO String
doXsltXform xsltFilePath = readProcess "xmlstarlet" ["tr", xsltFilePath, "-"]

