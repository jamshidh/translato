
import Data.Foldable
import Data.Functor
import Data.String
import Text.XML as XML
import System.Environment

import Format
import WidgetFormatter
import WidgetJSLibrary
import WidgetMerger
import WidgetParser

main = do
    args <- getArgs
    contents <- sequence $ XML.readFile def <$> fromString <$> args
    let widgets = xml2Widget <$> documentRoot <$> contents
    putStrLn $ format $ fold widgets


