{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}

import System.Console.CmdArgs
import System.FilePath

import Translator

data Options = Options{filename::FilePath, userAgent::String} deriving (Show, Data, Typeable)

main::IO ()
main = do
  options <- cmdArgs_ $
      record Options{} [
        filename := def += typ "FILENAME" += argPos 0,
        userAgent := def += typ "USERAGENT" += argPos 1
        ]
      += summary "Apply shims, reorganize, and generate to the input"

  let specName = 
          case takeExtension (filename options) of
            ('.':ext) -> ext
            _ -> error ("translato filename has no extension: " ++ (filename options))
    
  readFile (filename options) >>= translate specName (getRealUserAgent $ userAgent options) >>= putStrLn

--This is just a convenience wrapper to keep me from having to type long user agents in on the command line"
getRealUserAgent::String->String
getRealUserAgent "ie8" = "Mozilla/4.0 (compatible; MSIE 8.0; Windows NT 5.1; Trident/4.0)"
getRealUserAgent x = x
