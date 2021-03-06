{-# OPTIONS_GHC -Wall #-}
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

import Generator
import OutputParser
import Parser
import ParseElements
import Shower
import SMShower

--import JDebug

commands::M.Map String ([String]->IO())
commands = M.fromList
    [
        ("outputGrammar", showGrammarMain),
        ("outputSimplifiedGrammar", showSimplifiedGrammarMain),
        ("outputGeneratorGrammar", showGeneratorGrammarMain),
        ("outputSequenceMap", showSequenceMapMain),
        ("outputSimplifiedSequenceMap", showSimplifiedSequenceMapMain),
        ("outputGeneratorSequenceMap", showGeneratorSequenceMapMain),
        ("outputGeneratorSimplifiedSequenceMap", showSimplifiedGeneratorSequenceMapMain),
        ("outputParseTree", showParseTreeMain),
        ("generate", generatorMain   ),
        ("parseElements", parseElementsMain),
        ("parse", parseMain),
        ("pparse", pparseMain)
    ]

usage::String
usage = "parser COMMAND [option]\n"
    ++ "Options:\n"
    ++ (concat $ ("\t" ++) <$> (++ "\n") <$> fst <$> M.toList commands)

main::IO()
main = do
    args <- getArgs
    case args of
        [] -> error ("You need to supply a command:\n"
                    ++ (concat $ ("\t" ++) <$> (++ "\n") <$> fst <$> M.toList commands))
        (commandString:argsRest) ->
            case M.lookup commandString commands of
                Just command -> command argsRest
                Nothing -> putStrLn usage







