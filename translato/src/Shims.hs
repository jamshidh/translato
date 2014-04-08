{-# OPTIONS_GHC -Wall #-}

module Shims (
  applyShims
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import Data.Functor
import Data.List
import qualified Data.Text.Lazy as TL
import System.Directory
import System.Process
import qualified Text.XML as XML

import ShimConfig
import ShimLibs
import WidgetLibGenerator

--import Debug.Trace

applyShims::FilePath->String->String->String->EitherT String IO String
applyShims shimDir specName userAgent input = do
  neededShims <- liftIO $ getNeededShims userAgent shimDir -- neededShims holds shims allowed by browser constraints

  triggeredShims <- applyTriggers shimDir specName neededShims input -- triggeredShims holds shims allowed by trigger.xsl files, after the browser filter has been applied

  translators <- liftIO $ map doXsltXform <$> (filterM doesFileExist $ fmap ((flip $ getShimFile shimDir) "translate.xsl") $ triggeredShims)

  foldr (=<<) (return input) translators

applyTriggers::FilePath->String->[ShimName]->String->EitherT String IO [ShimName]
applyTriggers shimDir specName neededShims input = do

  translators <- liftIO $ map doXsltXform <$> (filterM doesFileExist $ fmap ((flip $ getShimFile shimDir) "trigger.xsl") $ neededShims)

  output <- TL.pack <$> foldr (=<<) (return input) translators

  case XML.parseText XML.def output of
    Left err -> left (show err)
    Right doc -> right $ nub $ documentToLibs doc


doXsltXform::FilePath->String->EitherT String IO String
doXsltXform xsltFilePath input = do
  (_, output, stderr) <- liftIO $ readProcessWithExitCode "xmlstarlet" ["tr", xsltFilePath, "-"] input
  if not $ null stderr
           then left stderr
           else right output

