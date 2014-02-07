
import Control.Arrow
import Data.Foldable hiding (concat)
import Data.Functor
import Data.List
import qualified Data.Map as M
import Data.String
import System.Directory
import System.Environment
import System.FilePath
import qualified Text.XML as XML

import Format
import Widget
import WidgetFormatter
import WidgetJSLibrary
import WidgetMerger
import WidgetParser

getFullPathDirectoryContents::FilePath->IO [FilePath]
getFullPathDirectoryContents filepath = map (filepath </>) <$> getDirectoryContents filepath

getWidgetFilePaths::FilePath->IO [FilePath]
getWidgetFilePaths dirName = do
    subDirs <- filterDirectories =<< filter (not . ("." `isPrefixOf`) . takeBaseName) <$> getFullPathDirectoryContents dirName
    filter ((== ".widget") . takeExtension) <$> (filterFiles =<< concat <$> (sequence $ map getFullPathDirectoryContents subDirs))

filterDirectories::[FilePath]->IO [FilePath]
filterDirectories [] = return []
filterDirectories (first:rest) = do
    isDir <- doesDirectoryExist first
    remainingDirs <- filterDirectories rest
    return $
        if isDir then first:remainingDirs else remainingDirs

filterFiles::[FilePath]->IO [FilePath]
filterFiles [] = return []
filterFiles (first:rest) = do
    isDir <- doesFileExist first
    remainingFiles <- filterFiles rest
    return $
        if isDir then first:remainingFiles else remainingFiles

main = do
    args <- getArgs
    let [inDir, outDir] =
            case args of
                [x, y] -> [x, y]
                _ -> error ("Error: need two input parameters")
    widgetFiles <- M.fromListWith (++) <$> map (takeBaseName &&& (:[])) <$> getWidgetFilePaths inDir

    contents <- sequence $ map (createLib outDir) $ M.toList widgetFiles
    sequence
        $ (\(widgetName, content) -> writeFile (outDir </> widgetName ++ ".js") content)
        <$> fmap fst
        <$> contents

    sequence
        $ (\(widgetName, content) -> writeFile (outDir </> widgetName ++ ".css") content)
        <$> [(widgetName, cssContent)|(widgetName, (_, Just cssContent))<-contents]

createLib::FilePath->(String, [FilePath])->IO (String, (String, Maybe String))
createLib outDir (widgetName, widgetFiles) = do
    contents <- sequence $ XML.readFile XML.def <$> fromString <$> widgetFiles
    let widgets = xml2Widget <$> XML.documentRoot <$> contents
    content <- widget2js widgetName $ format $ fold widgets
    let cssContent = style $ fold widgets
    return (widgetName, (content, cssContent))
