-------------------------------------
----------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Data.List
import qualified Data.Map as M
import Data.Sequence hiding (drop)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
--import qualified Data.Text as TL
--import qualified Data.Text.IO as TL
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec as P hiding (try)
import Text.XML as XML
import Text.XML.Cursor

import Filesystem.Path hiding (concat)
import Filesystem.Path.CurrentOS hiding (concat)

--import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Multiline.TextView

import Colors
import Generator
import GrammarParser
import GrammarTools
import LText as L hiding (head, drop)
--import ManyWorldsParser as MWor
import qualified Parser2 as P2
--import qualified Parser3 as P3
import qualified Parser4 as P4
import OperatorNames
--import Parser
import ParseElements
--import ParseError


{--makeWindow::IO ()
makeWindow = do
    initGUI
    window <- windowNew
    button <- textViewNew
    set window [ containerBorderWidth := 10,
        containerChild := button, windowDefaultWidth := 400, windowDefaultHeight := 300 ]
    --set button [ buttonLabel := "Hello World" ]
    --onClicked button (putStrLn "Hello World")
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI--}

prepareGrammar =
    fullySimplifyGrammar
    . expandOperators
    . stripWhitespaceFromGrammar
    . addEOFToGrammar
    . fullySimplifyGrammar
    . stripWhitespaceFromGrammar

{--showElement::XML.Node->String
showElement (NodeElement element) = TL.unpack (renderText def (element2Document (element)))
showElement (NodeContent text) = show text--}

outputGrammar::Grammar->IO ()
outputGrammar g = do
    putStrLn $ show g
    putStrLn $ show (prepareGrammar g)

outputParse::Grammar->IO ()
outputParse g = do
    let modifiedG = fullySimplifyGrammar $ expandOperators $ stripWhitespaceFromGrammar $ addEOFToGrammar g
    contents<-TL.getContents
--    let states=createParser g
--    case (MWor.parse "file" states (TL.unpack contents)) of
--    let result = P2.parse g "file" (P2.Parse [Link (startSymbol g)] [P2.Text (L.text2LText (TL.toStrict contents))])
    --let result = head $ (drop 0) (iterate (P4.parse modifiedG "file")
    --        (singleton (P4.Parse (P4.text2Tree (TL.toStrict contents)) [Link (startSymbol modifiedG)])))
    let result = P4.parse modifiedG "file"
                    (P4.text2Tree (TL.toStrict contents))
    putStrLn $ P4.treeShow result
    case P4.getErrors result of
        [] -> putStrLn "OK"
        _ -> putStrLn $ "\n" ++ red ("There were errors:\n") ++ P4.showErrors result

outputParseElements::Grammar->IO ()
outputParseElements g = do
    contents<-TL.getContents
    let doc=try(parseText def contents)
    putStrLn (TL.unpack (renderText def (parseElements g (fromDocument doc))))

outputString::Grammar->IO ()
outputString g = do
    contents<-TL.getContents
    let doc=try(parseText def contents)
    case generate g (fromDocument doc) of
        Right s -> putStrLn s
        Left err -> error (show err)

data Task = OutputGrammar | Parse | ParseElements | Generate

try::(Show err)=>Either err a->a
try (Left err) = error ("Error:" ++ show err)
try (Right a) = a

data Opts = Opts { grammarFilename::String, task::Task }

defaults = Opts { grammarFilename="grammar.spec", task=Parse }

args2Opts::[String]->Opts->Opts
args2Opts ("--outputGrammar":rest) o = args2Opts rest (o { task=OutputGrammar })
args2Opts ("--generate":rest) o = args2Opts rest (o { task=Generate })
args2Opts ("--parseElements":rest) o = args2Opts rest (o { task=ParseElements })
args2Opts (filename:rest) o = args2Opts rest (o { grammarFilename=filename })
args2Opts [] o = o









main = do
    --makeWindow
    args <- getArgs
    let opts = args2Opts args defaults

    specHandle<-openFile (grammarFilename opts) ReadMode
    grammarFile<-TL.hGetContents specHandle

    let grammar = try (P.parse grammarParser "grammar" (TL.unpack grammarFile))

    case task opts of
      OutputGrammar -> outputGrammar grammar
      Parse -> outputParse $ prepareGrammar grammar
      ParseElements -> outputParseElements $ prepareGrammar grammar
      Generate -> outputString grammar
