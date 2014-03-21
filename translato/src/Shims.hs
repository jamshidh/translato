{-# OPTIONS_GHC -Wall #-}

module Shims (
  applyShims
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Functor
import System.Directory
import System.FilePath
import System.Process

import Parser
import ShimConfig
import WidgetLibGenerator

import Debug.Trace

applyShims::FilePath->String->String->String->EitherT String IO String
applyShims shimDir specName userAgent input = do
  neededShims <- liftIO $ getNeededShims userAgent shimDir

  translators <- liftIO $ map doXsltXform <$> (filterM doesFileExist $ fmap ((flip $ getShimFile shimDir) "translate.xsl") $ neededShims)

  --This chains together functions in a list [a, b, c, d]
  --using (>>=), like this ">>= a >>= b >>= c >>= d"
  {--let allTranslatorsTogether =
        if length translators == 0
        then id
        else foldl1 (.) $ (=<<) <$> translators
    
  allTranslatorsTogether $ return input -- $ parseUsingSpecName specName input-}

  foldr (=<<) (return input) translators


doXsltXform::FilePath->String->EitherT String IO String
doXsltXform xsltFilePath input = do
  (_, output, stderr) <- liftIO $ readProcessWithExitCode "xmlstarlet" ["tr", xsltFilePath, "-"] input
  if not $ null stderr
           then left stderr
           else right output

