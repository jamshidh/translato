-----------------------------------------------------------------------------
--
-- Module      :  Shower
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

module Shower (
    showGrammarMain,
    showSimplifiedSequenceMapMain,
    showSequenceMapMain,
    showParseTreeMain
) where

import System.Console.GetOpt

import Grammar
import GrammarTools
import LeftFactoring
import Parser
import SequenceMap

data Options = Options { specFileName::String }
defaultOptions = Options { specFileName = "file.spec" }

optionDefs::[OptDescr Options]
optionDefs = []

args2Grammar::[String]->IO Grammar
args2Grammar args = loadGrammar filename
    where (options, filename) =
            case getOpt Permute optionDefs args of
                ([o], [f], _) -> (o, f)

showGrammarMain::[String]->IO ()
showGrammarMain args=do
    grammar <- args2Grammar args
    putStrLn $ formatGrammar grammar

showSimplifiedSequenceMapMain::[String]->IO ()
showSimplifiedSequenceMapMain args = do
    grammar <- args2Grammar args
    putStrLn $ formatSequenceMap (leftFactorSequenceMap $ sequenceMap grammar)

showSequenceMapMain::[String]->IO ()
showSequenceMapMain args = do
    grammar <- args2Grammar args
    putStrLn $ formatSequenceMap (sequenceMap grammar)

showParseTreeMain::[String]->IO ()
showParseTreeMain args = do
    grammar <- args2Grammar args
    putStrLn $ safeDrawEForest (parseTree grammar (main grammar))


