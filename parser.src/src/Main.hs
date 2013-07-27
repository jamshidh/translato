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

import Colors
import Context
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
import SequenceMap
import TreeTools

import JDebug

outputGrammar::Grammar->IO ()
outputGrammar g = do
    putStrLn $ formatGrammar g

outputSequenceMap::Grammar->IO ()
outputSequenceMap g = do
    putStrLn $ formatSequenceMap (sequenceMap g)

outputParseTree::Grammar->IO ()
outputParseTree g = do
    putStrLn $ drawForest (map (fmap formatExpression) (forestTake 16 (parseTree g (G.main g))))

test::Grammar->IO ()
test g = do
    putStrLn (safeDrawEForest $ seq2ParseTree M.empty [Or [[TextMatch "a"], []], TextMatch "c"])

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

try::(Show err)=>Either err a->a
try (Left err) = error ("Error:" ++ show err)
try (Right a) = a

data Opts = Opts { grammarFilename::String, task::Grammar->IO() }

defaults = Opts { grammarFilename="grammar.spec", task=outputParse }

options = M.fromList
    [
        ("outputGrammar", outputGrammar),
        ("outputSequenceMap", outputSequenceMap),
        ("outputParseTree", outputParseTree),
        ("test", test),
        ("generate", outputString),
        ("parseElements", outputParseElements)
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
    args <- getArgs
    let opts = args2Opts args defaults
    grammar<-loadGrammar (grammarFilename opts)
    task opts (fixG grammar)

