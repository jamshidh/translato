{-# OPTIONS_GHC -Wall #-}

module Translator (
  translate
) where

import Data.Functor
import qualified Data.Text.Lazy as TL
import System.Directory
import System.FilePath
import System.Process

import Reorganizer
import Shims
  
translate::String->String->String->IO String
translate specName userAgent input = do
  
  shimDir <- (</> "GlowApps" </> "html5" </> "shims") <$> getHomeDirectory

  result <- reorganize shimDir userAgent . TL.pack =<< applyShims shimDir specName userAgent input
  case result of
    Right reorganized -> readProcess "parser" ["generate", specName] . TL.unpack $ reorganized
    Left err -> error $ show err
    
