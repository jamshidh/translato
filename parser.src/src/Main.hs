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
import qualified Data.Map as M
import System.Environment

import Colors
--import Generator
import Grammar hiding (main)
import qualified Grammar as G (main)
import GrammarTools
import LeftFactoring
import Parser
--import ParseElements
import SequenceMap

import JDebug

outputGrammar::Grammar->IO ()
outputGrammar g = do
    putStrLn $ formatGrammar g

outputSequenceMap::Grammar->IO ()
outputSequenceMap g = do
--    putStrLn $ formatSequenceMap (sequenceMap g)
    putStrLn $ formatSequenceMap (leftFactorSequenceMap $ sequenceMap g)

outputParseTree::Grammar->IO ()
outputParseTree g = do
    putStrLn $ safeDrawEForest (parseTree g (G.main g))

test::Grammar->IO ()
test g = do
    putStrLn ""

outputParse::Grammar->IO ()
outputParse g = do
    interact (createParser g)

outputParseElements::Grammar->IO ()
outputParseElements cx = do
    putStrLn "To be added"
    {--contents<-TL.getContents
    let doc=try(parseText def contents)
    putStrLn (TL.unpack (renderText def (parseElements cx (fromDocument doc))))--}

outputString::Grammar->IO ()
outputString g = do
    putStrLn "To be added"
    {--contents<-TL.getContents
    let doc=try(parseText def contents)
    case generate g (fromDocument doc) of
        Right s -> putStrLn s
        Left err -> error (show err)--}

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

