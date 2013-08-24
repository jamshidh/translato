{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
    createParserForClass,
    createParser,
    parseTree,
    seq2ParseTree,
    parseMain
) where

import Prelude hiding (lookup)
import Data.Char hiding (Space)
import Data.Functor
import Data.Graph.Inductive.Query.Monad
import Data.Maybe
import Data.Tree
import Data.List as L hiding (union, lookup, insert)
import Data.Map hiding (map, foldl, filter)
import System.Console.GetOpt
import System.IO
import Text.Regex


import ArgOpts
import CharSet
--import CmdOptions
import EnhancedString as E
import EStringTools
import Grammar as G
import GrammarTools
import LeftFactoring
import Lookahead
import LString (LString, line, col, string, createLString)
import qualified LString as LS
import SequenceMap
import TreeTools

import JDebug

type Attribute = (String, String)

type EParser = LString->EString
type Parser = String->String

err::LString->String->Forest EChar
err s message = [Node { rootLabel=Error message s, subForest=[]}]

expectErr::LString->String->[EChar]
expectErr s expectation = [ExpectationError [expectation] s]

-------------------------------

seq2ParseTree::SequenceMap->Sequence->Forest Expression
seq2ParseTree sMap (Link name:rest) =
    case lookup name sMap of
        Nothing -> error ("The grammar links to a non-existant rule named '" ++ name ++ "'")
        Just seq -> seq2ParseTree sMap (seq ++ rest)
seq2ParseTree sMap (List 0 seq:rest) =
    seq2ParseTree sMap [Or [seq ++ [List 0 seq] ++ rest, FallBack:rest]]
seq2ParseTree sMap (List count seq:rest) =
    seq2ParseTree sMap (seq ++ [List (count -1) seq] ++ rest)
seq2ParseTree sMap (Or seqs:rest) =
        (((++ rest) <$> seqs) >>= seq2ParseTree sMap)
seq2ParseTree sMap (e:rest) = [Node{rootLabel=e, subForest=seq2ParseTree sMap rest}]
seq2ParseTree sMap [] = []

---------------------------------

rawParse::Forest Expression->LString->[EChar]
rawParse [Node{rootLabel=EOF, subForest=rest}] s | string s == [] = rawParse rest s
rawParse [Node{rootLabel=EOF, subForest=rest}] s = expectErr s "EOF"
rawParse [] s = []

rawParse [Node{rootLabel=TextMatch matchString, subForest=rest}] s | LS.isPrefixOf matchString s =
        rawParse rest (LS.drop (length matchString) s)
rawParse [Node{rootLabel=TextMatch matchString, subForest=_}] s = expectErr s matchString

rawParse [Node{rootLabel=Out (VStart name _:eStringRest), subForest=rest}] s =
    VStart name (Just s):rawParse [Node{rootLabel=Out eStringRest, subForest=rest}] s
rawParse [Node{rootLabel=Out (first:eStringRest), subForest=rest}] s =
    first:rawParse [Node{rootLabel=Out eStringRest, subForest=rest}] s
rawParse [Node{rootLabel=Out [], subForest=rest}] s = rawParse rest s

rawParse [Node{rootLabel=FallBack, subForest=rest}] s = rawParse rest s

rawParse [Node{rootLabel=WhiteSpace _, subForest=rest}] s | LS.null s = rawParse rest s
rawParse forest@[Node{rootLabel=WhiteSpace _, subForest=rest}] s | isSpace (LS.head s) =
    rawParse forest (LS.tail s)
rawParse [Node{rootLabel=WhiteSpace _, subForest=rest}] s = rawParse rest s

rawParse [Node{rootLabel=Character charset, subForest=rest}] s | LS.null s =
    expectErr s (formatCharSet charset)
rawParse forest@[Node{rootLabel=Character charset, subForest=rest}] s | LS.head s `isIn` charset =
    Ch (LS.head s):rawParse rest (LS.tail s)
rawParse [Node{rootLabel=Character charset, subForest=rest}] s =
    expectErr s (formatCharSet charset)

rawParse [x] _ = error ("Missing case in rawParse: " ++ safeDrawTree (fmap show x))

rawParse items s = rawParse [chooseOne items s] s

------------------------

parseTree::Grammar->String->Forest Expression
parseTree g startRule=seq2ParseTree (leftFactorSequenceMap $ sequenceMap g) [Link startRule]

createParserForClass::String->Grammar->Parser
createParserForClass startRule g s =
        enhancedString2String
--        show
            $ expandOperators
            $ fillInAttributes
            $ checkForVarConsistency []
            $ fillInVariableAssignments
            $ fillInFutureItems
            $ (rawParse (parseTree g startRule) (createLString s))

createParser::Grammar->Parser
createParser g = createParserForClass (main g) g

---------

data Options = Options { specFileName::Maybe String, inputFileName::Maybe String }
deflt = Options { specFileName = Nothing, inputFileName=Nothing }

parseMain::[String]->IO ()
parseMain args = do
    let options = $(arg2Opts ''Options) args deflt
    let specFileName' =
            case specFileName options of
                Nothing ->
                    case inputFileName options of
                        Nothing -> error "You have to supply the spec filename"
                        Just fileName -> extension ++ ".spec"
                            where
                                extension =
                                    case matchRegex (mkRegex "\\.([^\\.]+$)") fileName of
                                        Just [x] -> x
                                        _ -> error "You need to supply the spec filename when the inputFileName doesn't have an extension"
                Just x -> x
    grammar<-loadGrammar specFileName'
    case inputFileName options of
        Just fileName -> do
                fileHandle <- openFile fileName ReadMode
                input <- hGetContents fileHandle
                putStr (createParser (fixG grammar) input)
        Nothing -> interact (createParser (fixG grammar))






