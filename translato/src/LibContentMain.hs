{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

import Data.Functor
import qualified Data.Text as T
import System.Console.CmdArgs
import System.FilePath

import LibContent hiding (libname)
import UserAgentTools


data Options = Options{libname::String, userAgent::String} deriving (Show, Data, Typeable)

main = do
  options <- cmdArgs_ $
      record Options{} [
        libname := def += typ "LIBNAME" += argPos 0,
        userAgent := def += typ "USERAGENT" += argPos 1
        ]
      += summary "Get the library content (untransformed) either from <shimDir>/*/lib/<libname>, or created from <shimDir>/*/<libname>.widget"

  let shimDir = "/home/jim/GlowApps/html5/shims"
  
  maybeContent <- libContent shimDir (getRealUserAgent $ userAgent options) $ name2Lib (libname options)
  case maybeContent of
    Just output -> putStrLn output
    _ -> error "error"

