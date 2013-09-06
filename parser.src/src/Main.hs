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
import Generator
import qualified Editor
import Parser
import ParseElements
import Shower

import JDebug

commands::M.Map String ([String]->IO())
commands = M.fromList
    [
        ("outputGrammar", showGrammarMain),
        ("outputSequenceMap", showSequenceMapMain),
        ("outputSimplifiedSequenceMap", showSimplifiedSequenceMapMain),
        ("outputParseTree", showParseTreeMain),
        ("generate", generatorMain   ),
        ("parseElements", parseElementsMain),
        ("parse", parseMain),
        ("edit", Editor.editMain)
    ]

usage::String
usage = "parser COMMAND [option]\n"
    ++ "Options:\n"
    ++ (concat $ ("\t" ++) <$> (++ "\n") <$> fst <$> M.toList commands)

main = do
    args <- getArgs
    case args of
        [] -> error ("You need to supply a command:\n"
                    ++ (concat $ ("\t" ++) <$> (++ "\n") <$> fst <$> M.toList commands))
        (commandString:argsRest) ->
            case M.lookup commandString commands of
                Just command -> command argsRest
                Nothing -> putStrLn usage







