{-# LANGUAGE OverloadedStrings #-}

module LibContent (
  libContent,
  Lib(..)
  ) where

import Control.Monad
import Data.Functor
import Data.List
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import System.Directory
import System.FilePath

import Shims
import WidgetLibGenerator

--import Debug.Trace

libContent::FilePath->String->Lib->IO (Maybe String)
libContent shimDir userAgent lib = do
    
  staticLibFiles <-
    filterM doesFileExist
        =<< map ((</> T.unpack (libname lib)) . (</> "lib") . (shimDir </>))
        <$> filter (not . ("." `isPrefixOf`))
        <$> getDirectoryContents shimDir

  case (staticLibFiles, lib) of
    ([], JSLib _) -> do
      jsContent <- fst <$> getWidgetLibContent shimDir userAgent (T.unpack $ libname lib)
      case jsContent of
        Nothing -> error ("neither '<shimDir>/*/lib/" ++ T.unpack (libname lib) ++ "' nor '<shimDir>/*/" ++ takeBaseName (T.unpack $ libname lib) ++ ".widget' exists")
        x -> return x
    ([], CSSLib _) -> do
      cssContent <- snd <$> getWidgetLibContent shimDir userAgent (T.unpack $ libname lib)
      case cssContent of
        Nothing -> error ("neither '<shimDir>/*/lib/" ++ T.unpack (libname lib) ++ "' nor '<shimDir>/*/" ++ takeBaseName (T.unpack $ libname lib) ++ ".widget' exists")
        x -> return x
    ([staticFile], _) -> Just <$> readFile staticFile
    _ -> error ("Library '" ++ T.unpack (libname lib) ++ "' appears in more than one place")