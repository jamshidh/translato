{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# OPTIONS_GHC -Wall #-}

module Parser (
    createParserForClass,
    createParser,
    createEParserWithErrors,
    createParserWithErrors,
    parseTree,
    seq2ParseTree,
    parseUsingSpecName,
    parseMain
) where

import Prelude hiding (lookup)

import Control.Lens
import Control.Monad
import Data.Char hiding (Space)
import Data.Functor
import Data.Graph.Inductive.Query.Monad
import Data.Maybe
import Data.Tree
import qualified Data.Map as M
import System.FilePath
import System.IO

import ArgOpts
import CharSet
import EnhancedString as E
import EStringTools
import ExpressionMatcher
import Grammar as G
import GrammarTools
import LeftFactoring
import Lookahead
import LString (LString, string, createLString)
import qualified LString as LS
import ParseError
import SequenceMap
import SequenceTools
import TreeTools

--import JDebug

type Attribute = (String, String)

type EParser = String->EString
type Parser = String->String

--err::LString->String->Forest EChar
--err s msg =
--    [Node { rootLabel=Fail $ Error [singleCharacterRangeAt s] msg, subForest=[]}]

expectErr::LString->String->ParseError
expectErr s expectation = ExpectationError [singleCharacterRangeAt s] [expectation] s




-------------------------------

addName::String->Sequence->Sequence
addName _ [] = []
addName theName (TextMatch text _:rest) = TextMatch text (Just theName):rest
addName theName (Character charset _:rest) = Character charset (Just theName):rest
addName theName (c:rest) = c:addName theName rest

seq2ParseTree::SequenceMap->Sequence->Forest Expression
seq2ParseTree sMap (Link theName:rest) =
    case M.lookup theName sMap of
        Nothing -> error ("The grammar links to a non-existant rule named '" ++ theName ++ "'")
        Just sq -> seq2ParseTree sMap (addName theName (sq ++ rest))
seq2ParseTree sMap (List 0 sq:rest) =
    seq2ParseTree sMap [Or [sq ++ [List 0 sq] ++ rest, Priority Low:rest]]
seq2ParseTree sMap (List count sq:rest) =
    seq2ParseTree sMap (sq ++ [List (count -1) sq] ++ rest)
seq2ParseTree sMap (Or seqs:rest) =
        seq2ParseTree sMap =<< (++ rest) <$> seqs
seq2ParseTree sMap (expr:rest) = [Node{rootLabel=expr, subForest=seq2ParseTree sMap rest}]
seq2ParseTree _ [] = []

---------------------------------

fillInLString s (VStart theName _) = VStart theName $ Just s
fillInLString s (FutureItem _) = FutureItem $ Just s
fillInLString s x = x

rawParse::Forest Expression->LString->EString

rawParse [] s = []

rawParse [x] s = 
  case matchOne (rootLabel x) s of
    Left err -> [Fail $ err]
    Right (output, nextInput) -> (fillInLString s <$> output) ++ rawParse (subForest x) nextInput

rawParse items s = case chooseOne s items of
    Left err -> [Fail err]
    Right (tree, _) -> rawParse [tree] s

------------------------

parseTree::Grammar->String->Forest Expression
parseTree g startRule=seq2ParseTree (cleanSMap g) [Link startRule]
  where
        --cleanSMap = leftFactorSequenceMap True . fmap removeWSAndOut . fmap removeDefaultWS . sequenceMap
    --cleanSMap = fillInWSSeqs g . leftFactorSequenceMap True . fmap removeDefaultWS . sequenceMap
    cleanSMap = leftFactorSequenceMap True . fmap removeDefaultWS . sequenceMap

createEParserForClass::String->Grammar->EParser
createEParserForClass startRule g =
    expandOperators
    . fillInAttributes
    . checkForVarConsistency []
    . fillInVariableAssignments
    . fillInFutureItems
    . rawParse (parseTree g startRule)
    . (createLString $)

createParserForClass::String->Grammar->Parser
createParserForClass startRule g =
        enhancedString2String
        . (>>= eAmpEscape)
        . createEParserForClass startRule g

createEParser::Grammar->EParser
createEParser g = createEParserForClass (g^.main) g

--createParser::Grammar->Parser
--createParser g =
--    enhancedString2String
--    . (>>= eAmpEscape)
--    . createEParser g

createParser::Grammar->Parser
createParser g =
--    (format =<<)
    enhancedString2String
    . (>>= eAmpEscape)
    . expandOperators
    . fillInAttributes
    . checkForVarConsistency []
    . fillInVariableAssignments
    . cleanUpAfterError --has to happen after fillInFutureItems, as it counts tags, which might not be filled in until after that
    . fillInFutureItems
    . rawParse (parseTree g (g^.main))
    . (createLString $)




createEParserWithErrors::Grammar->String->(EString, [ParseError])
createEParserWithErrors g s = (result, getErrors result)
    where
        result = createEParser g s
        getErrors::EString->[ParseError]
        getErrors [] = []
        getErrors (Fail err:rest) = err:getErrors rest
        getErrors (_:rest) = getErrors rest

createParserWithErrors::Grammar->String->(String, [ParseError])
createParserWithErrors g s = mapFst enhancedString2String (createEParserWithErrors g s)

parseUsingSpecName::SpecName->String->IO String
parseUsingSpecName theSpecName input = do
    grammar<-loadGrammarAndSimplifyForParse theSpecName
    return $ createParser grammar input


---------

data Options = Options { specName::Maybe String, inputFileName::String }
deflt::Options
deflt = Options { specName = Nothing, inputFileName="-" }

parseMain::[String]->IO ()
parseMain args = do
    let options = $(arg2Opts ''Options ["inputFileName"]) args deflt

    let theSpecName =
          case msum [specName options, getFileExtension $ inputFileName options] of
            Just x -> x
            _ -> error "You need to supply 'specName' when the inputFileName doesn't have an extension"

    input <-
      case inputFileName options of
        "-" -> getContents
        theFileName -> hGetContents =<< openFile theFileName ReadMode

    putStrLn =<< parseUsingSpecName theSpecName input

getFileExtension::FilePath->Maybe String
getFileExtension x =
  case takeExtension x of
    ('.':ext) -> Just ext
    _ -> Nothing



