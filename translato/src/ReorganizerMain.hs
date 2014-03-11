
import Reorganizer

import Data.Functor
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Environment

import Paths_translato

main = do
  args <- getArgs
  case args of
    [userAgent] -> do
      specFilePath <- getDataFileName "spec"
      let shimDir = "/home/jim/GlowApps/html5/shims"
      result <- reorganize shimDir userAgent =<< TL.getContents
      case result of
        Right reorganized -> putStrLn $ TL.unpack reorganized
        Left err -> error (show err)
    _ -> error "Usage:\n  reorganizo <userAgent>"
