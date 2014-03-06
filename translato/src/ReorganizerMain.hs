
import Reorganizer

import Data.Functor
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Environment

import Paths_translato

main = do
  [userAgent] <- getArgs
  specFilePath <- getDataFileName "spec"
  let shimDir = "/home/jim/GlowApps/html5/shims"
  result <- reorganize shimDir userAgent =<< TL.getContents
  case result of
    Right reorganized -> putStrLn $ TL.unpack reorganized
    Left err -> error (show err)
    
