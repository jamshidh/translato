
import Control.Monad
import Data.Functor
import System.Directory
import System.Environment
import System.FilePath
import System.Process
import Text.JSON

import Paths_translato

import WidgetLibGenerator

main = do
    args <- getArgs

    let [filename, userAgent] =
            case args of
                [f, v] -> [f, v]
                x -> error ("Incorrect parameters passed to translato: " ++ show x)

    shimDir <- (</> "GlowApps" </> "html5" </> "shims") <$> getHomeDirectory

    resourceDir <- getDataFileName ""

    neededShimDirs <- getNeededShims userAgent shimDir

    translators <-
        map doXsltXform <$>
            (filterM doesFileExist $ (</> "translate.xsl") <$> neededShimDirs)
    --putStrLn ("There are " ++ show (length translators) ++ " translators")

    --This chains together functions in a list [a, b, c, d]
    --using (>>=), like this ">>= a >>= b >>= c >>= d"
    let allTranslatorsTogether =
            if length translators == 0
                then id
                else foldl1 (.) $ (=<<) <$> translators

    (allTranslatorsTogether $ readProcess "parser" ["parse", filename] "")
        >>= (doXsltXform (resourceDir </> "reorganize.xsl"))
        >>= readProcess "parser" ["generate", "html5"]
        >>= putStrLn

doXsltXform::FilePath->String->IO String
doXsltXform xsltFilePath input = do
  ret <- readProcess "xmlstarlet" ["tr", xsltFilePath, "-"] input
  return ret

