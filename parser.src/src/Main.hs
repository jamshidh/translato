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
--    Context { grammar=modifiedG, ruleMap=M.mapWithKey (\name val -> leftFactor val) (grammar2RuleMap g) }
    Context {
        grammar=modifiedG,
        attributes=[],
        currentAttribute=Nothing,
        allSubstitutionsWithName=(grammar2RulesMap modifiedG),
        conditions=[],
        seq2Separator=grammarSeq2Separator modifiedG
        }
        where modifiedG = stripWhitespaceFromGrammar $ addEOFToGrammar g
--    . fullySimplifyGrammar

{--showElement::XML.Node->String
showElement (NodeElement element) = TL.unpack (renderText def (element2Document (element)))
showElement (NodeContent text) = show text--}

outputGrammar::Context->IO ()
outputGrammar cx = do
    putStrLn $ show (grammar cx)

outputRules::Grammar->IO ()
outputRules g = do
    putStrLn "qqqq"
--    putStrLn $ ruleMapShow $ allSubstitutionsWithName (prepareContext g)

outputParse::Context->IO ()
outputParse cx = do
    interact (createParser cx)

outputParseElements::Context->IO ()
outputParseElements cx = do
    contents<-TL.getContents
    let doc=try(parseText def contents)
    putStrLn (TL.unpack (renderText def (parseElements cx (fromDocument doc))))

outputString::Context->IO ()
outputString cx = do
    contents<-TL.getContents
    let doc=try(parseText def contents)
    case generate cx (fromDocument doc) of
        Right s -> putStrLn s
        Left err -> error (show err)

data Task = OutputGrammar | OutputRules | Parse | ParseElements | Generate

try::(Show err)=>Either err a->a
try (Left err) = error ("Error:" ++ show err)
try (Right a) = a

data Opts = Opts { grammarFilename::String, task::Task }

defaults = Opts { grammarFilename="grammar.spec", task=Parse }

args2Opts::[String]->Opts->Opts
args2Opts ("--outputGrammar":rest) o = args2Opts rest (o { task=OutputGrammar })
args2Opts ("--outputRules":rest) o = args2Opts rest (o { task=OutputRules })
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
      Parse -> outputParse $ prepareContext grammar
      ParseElements -> outputParseElements $ prepareContext grammar
      Generate -> outputString $ prepareContext grammar
