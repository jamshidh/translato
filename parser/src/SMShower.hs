{-# Language TemplateHaskell #-}

module SMShower (
    showSequenceMapMain,
    showSimplifiedSequenceMapMain,
    showSimplifiedGeneratorSequenceMapMain,
    showGeneratorSequenceMapMain

) where

import Data.Map as M

import ArgOpts
import Format
import Grammar
import GrammarTools
import LeftFactoring
import Parser
import SequenceMap
import SequenceTools



data Options = Options { specFileName::String, ruleName::Maybe String }
deflt = Options { specFileName = "file.spec", ruleName=Nothing }

showGeneratorSequenceMap::Bool->[String]->IO ()
showGeneratorSequenceMap simplify args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar<-loadGrammarAndSimplifyForGenerate (specFileName options)
    let rawSequenceMap = sequenceMap grammar
    let sequenceMap = if simplify
                        then leftFactorSequenceMap False rawSequenceMap
                        else rawSequenceMap
    case ruleName options of
        Nothing->putStrLn $ formatSequenceMap sequenceMap
        Just ruleName'->case M.lookup ruleName' sequenceMap of
                            Just sequence -> putStrLn $ format sequence
                            Nothing -> error ("Error: '" ++ ruleName' ++ "' isn't in the sequenceMap")

showSequenceMap::Bool->[String]->IO ()
showSequenceMap simplify args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar<-loadGrammarAndSimplifyForParse (specFileName options)
    let rawSequenceMap = sequenceMap grammar
    let sequenceMap = if simplify
                        then fillInWSSeqs grammar $ leftFactorSequenceMap True $ fmap removeDefaultWS $ rawSequenceMap
                        else rawSequenceMap
    case ruleName options of
        Nothing->putStrLn $ formatSequenceMap sequenceMap
        Just ruleName'->case M.lookup ruleName' sequenceMap of
                            --Just sequence -> putStrLn $ format $ removeWSAndOut sequence
                            Just sequence -> putStrLn $ format sequence
                            Nothing -> error ("Error: '" ++ ruleName' ++ "' isn't in the sequenceMap")

showSimplifiedSequenceMapMain::[String]->IO ()
showSimplifiedSequenceMapMain = showSequenceMap True

showSequenceMapMain::[String]->IO ()
showSequenceMapMain = showSequenceMap False

showSimplifiedGeneratorSequenceMapMain::[String]->IO ()
showSimplifiedGeneratorSequenceMapMain = showGeneratorSequenceMap True

showGeneratorSequenceMapMain::[String]->IO ()
showGeneratorSequenceMapMain = showGeneratorSequenceMap False
