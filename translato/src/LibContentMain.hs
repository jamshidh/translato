{-# LANGUAGE OverloadedStrings #-}

import Data.Functor
import qualified Data.Text as T
import System.Environment
import System.FilePath

import LibContent



name2Lib::FilePath->Lib
name2Lib n | takeExtension n == ".js" = JSLib $ T.pack n
name2Lib n | takeExtension n == ".css" = CSSLib $ T.pack n
name2Lib n = error ("Unknown lib type in name2Lib: " ++ n)

main = do
  [libname, userAgent] <- getArgs
  let shimDir = "/home/jim/GlowApps/html5/shims"
  
  maybeContent <- libContent shimDir userAgent $ name2Lib libname
  case maybeContent of
    Just output -> putStrLn output
    _ -> error "error"

