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

import Data.Functor
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
import Context
import Generator
import Grammar hiding (main)
import GrammarParser
import GrammarTools
import LText as L hiding (head, drop)
--import ManyWorldsParser as MWor
--import qualified Parser2 as P2
--import qualified Parser3 as P3
--import qualified Parser4 as P4
--import OperatorNames
import Parser
import ParseElements
--import ParseError
import SequenceMap

import JDebug

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

{--prepareContext g =
    grammar2Context modifiedG
        where modifiedG = stripWhitespaceFromGrammar $ addEOFToGrammar g--}

{--showElement::XML.Node->String
showElement (NodeElement element) = TL.unpack (renderText def (element2Document (element)))
showElement (NodeContent text) = show text--}

outputGrammar::Grammar->IO ()
outputGrammar g = do
    putStrLn $ show g

outputSequenceMap::Grammar->IO ()
outputSequenceMap g = do
    putStrLn $ formatSequenceMap (sequenceMap g)

outputParse::Grammar->IO ()
outputParse g = do
    interact (createParser g)

outputParseElements::Grammar->IO ()
outputParseElements cx = do
    contents<-TL.getContents
    let doc=try(parseText def contents)
    putStrLn (TL.unpack (renderText def (parseElements cx (fromDocument doc))))

outputString::Grammar->IO ()
outputString g = do
    contents<-TL.getContents
    let doc=try(parseText def contents)
    case generate g (fromDocument doc) of
        Right s -> putStrLn s
        Left err -> error (show err)

data Task = OutputGrammar | OutputSequenceMap | Parse | ParseElements | Generate

try::(Show err)=>Either err a->a
try (Left err) = error ("Error:" ++ show err)
try (Right a) = a

data Opts = Opts { grammarFilename::String, task::Task }

defaults = Opts { grammarFilename="grammar.spec", task=Parse }

options = M.fromList
    [
        ("optputGrammar", OutputGrammar),
        ("outputSequenceMap", OutputSequenceMap ),
        ("generate", Generate),
        ("parseElements", ParseElements)
    ]

usage::String
usage = "parser [option] " ++ underline("PARSEFILE") ++ "\n"
    ++ "Options:\n"
    ++ concat ((++ "\n") <$> ("\t--" ++) <$> fst <$> M.toList options)

args2Opts::[String]->Opts->Opts
args2Opts (('-':'-':word):rest) o =
    case M.lookup word options of
        Just option -> args2Opts rest (o { task=option })
        Nothing -> error ("Unknown option: " ++ word ++ "\nUsage:\n" ++ usage)
args2Opts (filename:rest) o = args2Opts rest (o { grammarFilename=filename })
args2Opts [] o = o









main = do
    --makeWindow
    args <- getArgs
    let opts = args2Opts args defaults

    specHandle<-openFile (grammarFilename opts) ReadMode
    grammarFile<-TL.hGetContents specHandle

    let grammar = try (P.parse parseGrammar "grammar" (TL.unpack grammarFile))

    case task opts of
      OutputGrammar -> outputGrammar grammar
      OutputSequenceMap -> outputSequenceMap grammar
      Parse -> outputParse grammar
      ParseElements -> outputParseElements grammar
      Generate -> outputString grammar
