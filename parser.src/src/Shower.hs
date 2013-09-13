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
    showSimplifiedGrammarMain,
    showGeneratorGrammarMain,
    showParseTreeMain
) where

import Control.Lens

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
    grammar<-loadUnsimplifiedGrammar (specFileName options)
    putStrLn $ formatGrammar grammar

showSimplifiedGrammarMain::[String]->IO ()
showSimplifiedGrammarMain args=do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar<-loadGrammarAndSimplifyForParse (specFileName options)
    putStrLn $ formatGrammar grammar

showGeneratorGrammarMain::[String]->IO ()
showGeneratorGrammarMain args=do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar<-loadGrammarAndSimplifyForGenerate (specFileName options)
    putStrLn $ formatGrammar grammar

showParseTreeMain::[String]->IO ()
showParseTreeMain args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar<-loadGrammarAndSimplifyForParse (specFileName options)
    putStrLn $ safeDrawEForest (parseTree grammar (grammar^.main))
