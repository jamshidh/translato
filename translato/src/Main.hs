{-# OPTIONS_GHC -Wall #-}

import System.Environment
import System.FilePath

import Translator


main::IO ()
main = do
    args <- getArgs

    let [filename, userAgent] =
            case args of
                [f, u] -> [f, u]
                x -> error ("Incorrect parameters passed to translato: " ++ show x)

    let specName = 
          case takeExtension filename of
            ('.':ext) -> ext
            _ -> error ("translato filename has no extension: " ++ filename)
    
    readFile filename >>= applyShims specName userAgent >>= putStrLn
