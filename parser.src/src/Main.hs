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
import Data.Tree
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec as P hiding (try)
import Text.XML as XML
import Text.XML.Cursor

import Filesystem.Path hiding (concat)
import Filesystem.Path.CurrentOS hiding (concat)

--import Graphics.UI.Gtk
--import Graphics.UI.Gtk.Multiline.TextView

import Atom
import Colors
import Context
import ETreeTools
import Generator
import Grammar hiding (main)
import qualified Grammar as G (main)
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
import TreeTools

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

prepareContext g =
    grammar2Context modifiedG
        where modifiedG = stripWhitespaceFromGrammar $ addEOFToGrammar g

{--showElement::XML.Node->String
showElement (NodeElement element) = TL.unpack (renderText def (element2Document (element)))
showElement (NodeContent text) = show text--}

outputGrammar::Context->IO ()
outputGrammar cx = do
    putStrLn $ show (grammar cx)

outputRules::Grammar->IO ()
outputRules g = do
    putStrLn $ linksShow $ sequences (prepareContext g)
--    putStrLn $ postSeqShow (prepareContext g)

outputExp::Grammar->IO ()
outputExp g = do
    let exp = name2Exp g (G.main g)
    --jtrace ("qqqq: " ++ show (rootLabel (head exp))) $ putStrLn ((forestTake 5 exp) >>= expShow)
    putStrLn ((forestTake 8 exp) >>= expShow)
    --putStrLn (exp >>= expShow)

outputParse::Grammar->IO ()
outputParse g = do
    interact (createParser g)

outputParseElements::Grammar->IO ()
outputParseElements g = do
    contents<-TL.getContents
    let doc=try(parseText def contents)
    putStrLn (TL.unpack (renderText def (parseElements g (fromDocument doc))))

outputString::Context->IO ()
outputString cx = do
    contents<-TL.getContents
    let doc=try(parseText def contents)
    case generate cx (fromDocument doc) of
        Right s -> putStrLn s
        Left err -> error (show err)

data Task = OutputGrammar | OutputRules | OutputExp | Parse | ParseElements | Generate

try::(Show err)=>Either err a->a
try (Left err) = error ("Error:" ++ show err)
try (Right a) = a

data Opts = Opts { grammarFilename::String, task::Task }

defaults = Opts { grammarFilename="grammar.spec", task=Parse }

args2Opts::[String]->Opts->Opts
args2Opts ("--outputGrammar":rest) o = args2Opts rest (o { task=OutputGrammar })
args2Opts ("--outputRules":rest) o = args2Opts rest (o { task=OutputRules })
args2Opts ("--outputExp":rest) o = args2Opts rest (o { task=OutputExp })
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

    let grammar = try (P.parse parseGrammar "grammar" (TL.unpack grammarFile))

    case task opts of
      OutputGrammar -> outputGrammar $ prepareContext grammar
      OutputRules -> outputRules grammar
      OutputExp -> outputExp grammar
      Parse -> outputParse grammar
      ParseElements -> outputParseElements grammar
      Generate -> outputString $ prepareContext grammar
