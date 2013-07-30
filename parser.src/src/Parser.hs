{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
    createParserForClass,
    createParser,
    parseTree,
    seq2ParseTree
) where

import Prelude hiding (lookup)
import Data.Char hiding (Space)
import Data.Functor
import Data.Graph.Inductive.Query.Monad
import Data.Maybe
import Data.Tree
import Text.XML
import Data.List as L hiding (union, lookup, insert)
import Data.Map hiding (map, foldl, filter)

import CharSet
import Context
import EnhancedString as E
import EStringTools
import Grammar as G
import GrammarTools
import LeftFactoring
import Lookahead
import LString (LString, line, col, string, createLString)
import qualified LString as LS
import OperatorNames
import SequenceMap
import TreeTools
import VarAssignment
import XPath

--import Debug.Trace
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
rawParse [Node{rootLabel=EOF, subForest=rest}] s | string s == [] = Ch '\n':rawParse rest s
rawParse [Node{rootLabel=EOF, subForest=rest}] s = expectErr s "EOF"
rawParse [] s = [Ch '\n']

rawParse [Node{rootLabel=TextMatch matchString, subForest=rest}] s | LS.isPrefixOf matchString s =
        rawParse rest (LS.drop (length matchString) s)
rawParse [Node{rootLabel=TextMatch matchString, subForest=_}] s = expectErr s matchString

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
            $ fillInAttributes
            $ fillInVariableAssignments
            $ fillInFutureItems
--            $ assignVariables
            $ (rawParse (parseTree g startRule) (createLString s))

createParser::Grammar->Parser
createParser g = createParserForClass (main g) g
