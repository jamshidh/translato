{-# Language TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  SMShower
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

module SMShower (
    showSequenceMapMain,
    showSimplifiedSequenceMapMain
) where

import Data.Map as M

import ArgOpts
import Grammar
import GrammarTools
import LeftFactoring
import Parser
import SequenceMap



data Options = Options { specFileName::String, ruleName::Maybe String }
deflt = Options { specFileName = "file.spec", ruleName=Nothing }

showSequenceMapMain'::Bool->[String]->IO ()
showSequenceMapMain' simplify args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar<-loadGrammarAndSimplifyForParse (specFileName options)
    let rawSequenceMap = sequenceMap grammar
    let sequenceMap = if simplify
                        then leftFactorSequenceMap rawSequenceMap
                        else rawSequenceMap
    case ruleName options of
        Nothing->putStrLn $ formatSequenceMap sequenceMap
        Just ruleName'->case M.lookup ruleName' sequenceMap of
                            Just sequence -> putStrLn $ formatSequence sequence
                            Nothing -> error ("Error: '" ++ ruleName' ++ "' isn't in the sequenceMap")

showSimplifiedSequenceMapMain::[String]->IO ()
showSimplifiedSequenceMapMain = showSequenceMapMain' True

showSequenceMapMain::[String]->IO ()
showSequenceMapMain = showSequenceMapMain' False
