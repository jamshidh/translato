
import Data.Functor
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import Text.JSON

import Paths_translato

main = do
    args <- getArgs

    let [filename, version] =
            case args of
                [f, v] -> [f, v]
                x -> error ("Incorrect parameters passed to translato: " ++ show x)

    shimDir <- ((</> "shims") . (</> "html5") . (</> "GlowApps")) <$> getHomeDirectory

    resourceDir <- getDataFileName ""

    readProcess "parser" ["parse", filename] ""
        >>= doXsltXform (shimDir </> "detailsSummaryEvents" </> "addDetailsSummaryEvents.xsl")
        >>= doXsltXform (resourceDir </> "reorganize.xsl")
        >>= readProcess "parser" ["generate", "html5"]
        >>= putStrLn

doXsltXform::FilePath->String->IO String
doXsltXform xsltFilePath input = do
  ret <- readProcess "xmlstarlet" ["tr", xsltFilePath, "-"] input
  return ret

