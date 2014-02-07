
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

    translators <-
        map doXsltXform <$>
            (filterExists
                =<< map (</> "translate.xsl")
                <$> map (shimDir </>)
                <$> getDirectoryContents shimDir)

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

filterExists::[FilePath]->IO [FilePath]
filterExists [] = return []
filterExists (first:rest) = do
    exists <- doesFileExist first
    filteredRest <- filterExists rest
    return $
        if exists then first:filteredRest else filteredRest

doXsltXform::FilePath->String->IO String
doXsltXform xsltFilePath input = do
  ret <- readProcess "xmlstarlet" ["tr", xsltFilePath, "-"] input
  return ret

