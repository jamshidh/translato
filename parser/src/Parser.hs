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
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
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

type EParser = TL.Text->EString
type Parser = TL.Text->String

--err::LString->String->Forest EChar
--err s msg =
--    [Node { rootLabel=Fail $ Error [singleCharacterRangeAt s] msg, subForest=[]}]

expectErr::LString->String->ParseError
expectErr s expectation = ExpectationError [singleCharacterRangeAt s] [expectation] s




-------------------------------

addName::TL.Text->Sequence->Sequence
addName _ [] = []
addName theName (TextMatch text _:rest) = TextMatch text (Just theName):rest
addName theName (Character charset _:rest) = Character charset (Just theName):rest
addName theName (c:rest) = c:addName theName rest

addReparserIfNecessary::Maybe TL.Text->Sequence->Sequence
addReparserIfNecessary Nothing sq = sq
addReparserIfNecessary (Just reparseName) sq = [Out [ReparseStart reparseName]] ++ sq ++ [Out [ReparseEnd]]

seq2ParseTree::SequenceMap->Sequence->Forest Expression
seq2ParseTree sMap (Link reparser theName:rest) =
    case M.lookup theName sMap of
        Nothing -> error ("The grammar links to a non-existant rule named '" ++ TL.unpack theName ++ "'")
        Just sq -> seq2ParseTree sMap (addName theName (addReparserIfNecessary reparser sq ++ rest))
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

parseTree::Grammar->TL.Text->Forest Expression
parseTree g startRule=seq2ParseTree (cleanSMap g) [Link Nothing startRule, EOF]
  where
        --cleanSMap = leftFactorSequenceMap True . fmap removeWSAndOut . fmap removeDefaultWS . sequenceMap
    --cleanSMap = fillInWSSeqs g . leftFactorSequenceMap True . fmap removeDefaultWS . sequenceMap
    cleanSMap = leftFactorSequenceMap True . fmap removeDefaultWS . sequenceMap

createEParserForClass::TL.Text->Grammar->EParser
createEParserForClass startRule g = 
    expandOperators
    . reparseIfNeeded g
    . fillInAttributes
    . checkForVarConsistency []
    . fillInVariableAssignments
    . cleanUpAfterError --has to happen after fillInFutureItems, as it counts tags, which might not be filled in until after that
    . fillInFutureItems
    . rawParse (parseTree g startRule)
    . (createLString $)

createParserForClass::TL.Text->Grammar->Parser
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
    . reparseIfNeeded g
    . expandOperators
    . fillInAttributes
    . checkForVarConsistency []
    . fillInVariableAssignments
    . cleanUpAfterError --has to happen after fillInFutureItems, as it counts tags, which might not be filled in until after that
    . fillInFutureItems
    . rawParse (parseTree g (g^.main))
    . (createLString $)




createEParserWithErrors::Grammar->TL.Text->(EString, [ParseError])
createEParserWithErrors g s = (result, getErrors result)
    where
        result = createEParser g s
        getErrors::EString->[ParseError]
        getErrors [] = []
        getErrors (Fail err:rest) = err:getErrors rest
        getErrors (_:rest) = getErrors rest

createParserWithErrors::Grammar->TL.Text->(String, [ParseError])
createParserWithErrors g s = mapFst enhancedString2String (createEParserWithErrors g s)

parseUsingSpecName::SpecName->TL.Text->IO String
parseUsingSpecName theSpecName input = do
    grammar<-loadGrammarAndSimplifyForParse theSpecName
    return $ createParser grammar input


-----------------------

reparseIfNeeded::Grammar->EString->EString
reparseIfNeeded _ [] = []
reparseIfNeeded g (ReparseStart reparseName:FilledInEStart _ _:rest) = 
  doReparse theString ++ reparseIfNeeded g rest'
  where 
    (theString, rest') = splitReparseString reparseName rest
    doReparse = createEParserForClass reparseName g
reparseIfNeeded _ (ReparseStart reparseName:e:rest) = error ("reparseIfNeeded: Shouldn't be here: ReparseStart followed by " ++ show e)
reparseIfNeeded _ (ReparseEnd:rest) = error "reparseIfNeeded: Shouldn't be here"
reparseIfNeeded g (c:rest) = c:reparseIfNeeded g rest

splitReparseString::TL.Text->EString->(TL.Text, EString)
splitReparseString reparseName (EEnd _:ReparseEnd:rest) = ("", rest)
splitReparseString reparseName (Ch c:rest) = (TL.cons c out, rest')
  where (out, rest') = splitReparseString reparseName rest



















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
        "-" -> TL.getContents
        theFileName -> TL.hGetContents =<< openFile theFileName ReadMode

    putStrLn =<< parseUsingSpecName theSpecName input

getFileExtension::FilePath->Maybe String
getFileExtension x =
  case takeExtension x of
    ('.':ext) -> Just ext
    _ -> Nothing



