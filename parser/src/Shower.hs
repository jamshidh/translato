{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

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

import FieldMarshal

import ArgOpts
import Grammar
import GrammarTools
import LeftFactoring
import Parser
import SequenceMap

data Options = Options { specFileName::String }

$(deriveFieldMarshal ''Options ''String)

deflt = Options { specFileName = "file.spec" }

showGrammarMain::[String]->IO ()
showGrammarMain args=do
    let options = args2Opts args ["specFileName"] deflt
    grammar<-loadUnsimplifiedGrammar (specFileName options)
    putStrLn $ formatGrammar grammar

showSimplifiedGrammarMain::[String]->IO ()
showSimplifiedGrammarMain args=do
    let options = args2Opts args ["specFileName"] deflt
    grammar<-loadGrammarAndSimplifyForParse (specFileName options)
    putStrLn $ formatGrammar grammar

showGeneratorGrammarMain::[String]->IO ()
showGeneratorGrammarMain args=do
    let options = args2Opts args ["specFileName"] deflt
    grammar<-loadGrammarAndSimplifyForGenerate (specFileName options)
    putStrLn $ formatGrammar grammar

showParseTreeMain::[String]->IO ()
showParseTreeMain args = do
    let options = args2Opts args ["specFileName"] deflt
    grammar<-loadGrammarAndSimplifyForParse (specFileName options)
    putStrLn $ safeDrawEForest (parseTree grammar (grammar^.main))
