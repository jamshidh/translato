
import Control.Arrow
import Data.Traversable hiding (sequence)
import System.Environment
import System.FilePath

import WidgetLibGenerator

tupleSequence = map (uncurry (fmap . (,)))

main = do
    args <- getArgs
    let [shimDir, outDir] =
            case args of
                [x, y] -> [x, y]
                _ -> error ("Error: need two input parameters")

    widgetNames <- getWidgetNames shimDir

    contents <-
        sequence
            $ tupleSequence
            $ map (id &&& getWidgetLibContent shimDir) widgetNames

    forM contents $ \(widgetName, (maybeContent, maybeCSSContent)) -> do
        case maybeContent of
            Just content -> writeFile (outDir </> widgetName ++ ".js") content
            _ -> return ()
        case maybeCSSContent of
            Just cssContent -> writeFile (outDir </> widgetName ++ ".css") cssContent
            _ -> return ()

