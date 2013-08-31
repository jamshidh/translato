{-# Language TemplateHaskell #-}
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

import ArgOpts
import Grammar
import GrammarTools
import LeftFactoring
import Parser
import SequenceMap

data Options = Options { specFileName::String }
deflt = Options { specFileName = "file.spec" }

showGrammarMain::[String]->IO ()
showGrammarMain args=do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar<-loadGrammar (specFileName options)
    putStrLn $ formatGrammar grammar

showSimplifiedSequenceMapMain::[String]->IO ()
showSimplifiedSequenceMapMain args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar<-loadGrammar (specFileName options)
    putStrLn $ formatSequenceMap (leftFactorSequenceMap $ sequenceMap grammar)

showSequenceMapMain::[String]->IO ()
showSequenceMapMain args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar<-loadGrammar (specFileName options)
    putStrLn $ formatSequenceMap (sequenceMap grammar)

showParseTreeMain::[String]->IO ()
showParseTreeMain args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar<-loadGrammar (specFileName options)
    putStrLn $ safeDrawEForest (parseTree grammar (main grammar))


