{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
    createParserForClass,
    createParser
) where

import Prelude hiding (lookup)
import Data.Char hiding (Space)
import Data.Functor
import Data.Graph.Inductive.Query.Monad
import Data.Maybe
--import Data.Text as DT hiding (map, length, foldl, foldl1, empty, head, tail, filter, intercalate, isPrefixOf, take, drop, find)
import Data.Tree
import Text.XML
import Data.List as L hiding (union, lookup, insert)
import Data.Map hiding (map, foldl, filter)

import Atom as A
import CharSet
import Context
import EnhancedString as E
import EStringTools
import ETreeTools
import Grammar as G hiding (WhiteSpace)
import GrammarTools
import LeftFactoring
import Lookahead
import LString (LString, line, col, string, createLString)
import qualified LString as LS
import OperatorNames
import VarAssignment
import XPath

--import Debug.Trace
import JDebug

type Attribute = (String, String)

type EParser = LString->EString
type Parser = String->String

err::LString->String->Forest EChar
err s message = [Node { rootLabel=Error message s, subForest=[]}]

expectErr::LString->String->Forest EChar
expectErr s expectation = [Node { rootLabel=ExpectationError [expectation] s, subForest=[]}]

-------------------------------

{--removeBind::Sequence->Sequence
removeBind (Bind:rest)=removeBind rest
removeBind (x:rest) = x:removeBind rest
removeBind [] = []--}

rawParse::[Exp]->Maybe LString->Forest EChar
{--rawParse cx (EOF:rest) s | string s == [] = [Node {rootLabel=Ch '\n', subForest=rawParse cx rest s}]
rawParse cx (EOF:rest) s = expectErr s "EOF"
--rawParse cx seq s | string s == [] =  [err s ("File ends too soon, expecting " ++ show seq)]--}
--rawParse [] s | (not . LS.null) s = (expectErr s "EOF")
rawParse [] s = [Node { rootLabel=(E.Ch '\n'), subForest=[] }]

rawParse [Node{rootLabel=Out ec, subForest=rest}] s =
    [Node {rootLabel=ec, subForest=rawParse rest s}]

rawParse _ Nothing = error "No more input"

--rawParse [Node{rootLabel=A.Ch charset, subForest=rest}] s | LS.null s = expectErr s (formatAsRegex charset)
rawParse [Node{rootLabel=A.Ch charset, subForest=rest}] (Just s) | listToMaybe (LS.string s) `isIn` charset =
    rawParse rest (LS.tailOrMaybe s)

rawParse [Node{rootLabel=A.Ch charset, subForest=rest}] (Just s) = expectErr s (formatAsRegex charset)


--rawParse [Node{rootLabel=x}] s = error ("Missing case in rawParse: " ++ show x)

rawParse items (Just s) = rawParse [chooseOne items s] (Just s)



------------------------

eParser2Parser::EParser->Parser
eParser2Parser p = enhancedString2String . p . createLString
--eParser2Parser p = show . p . createLString

isntError::EChar->Bool
isntError (Error _ _) = False
isntError (ExpectationError _ _) = False
isntError x = True

isBound::Tree EChar->Bool
isBound (Node {rootLabel=Bound})=True
isBound (Node {rootLabel=Sync _})=False
isBound (Node {subForest=[tree]})=isBound tree
isBound (Node {subForest=[]})=False
isBound tree@(Node {subForest=_})= False
--    if safe then False
--        else error ("split before bound ambiguity resolved: " ++ drawTree (fmap show tree))

isError::Bool->Tree EChar->Bool
isError safe (Node {rootLabel=(Error _ _)})=True
isError safe (Node {rootLabel=(ExpectationError _ _)})=True
isError safe (Node {rootLabel=Sync _})=False
isError safe (Node {subForest=[tree]})=isError safe tree
isError safe (Node {subForest=[]})=False
isError safe tree@(Node {subForest=_})=
    if safe then False
        else error ("split before ambiguity resolved: " ++ cleanDraw tree)

hasSync::Tree EChar->Bool
hasSync (Node {rootLabel=Sync _})=True
hasSync (Node {subForest=[next]})=hasSync next
hasSync (Node {subForest=[]})=False

getError::Tree EChar->EChar
getError (Node {rootLabel=err@(Error _ _)})=err
getError (Node {rootLabel=err@(ExpectationError _ _)})=err
getError (Node {rootLabel=Sync _})=error "There should have been an error"
getError (Node {subForest=[tree]})=getError tree

movePastNextSync::(EString, Tree EChar)->(EString, Tree EChar)
movePastNextSync (output, node@Node {rootLabel=Sync c, subForest=[next]}) = (output ++ [Sync c], next)
movePastNextSync (output, Node {rootLabel=rootLabel, subForest=[next]}) = movePastNextSync (output ++ [rootLabel], next)
movePastNextSync (output, Node {rootLabel=rootLabel, subForest=[]}) =
    (output ++ [rootLabel], Node {rootLabel=Sync '!', subForest=[]})
movePastNextSync (output, node@Node {rootLabel=rootLabel, subForest=subForest}) =
    error ("ambiguity within ambiguity:\n\n" ++ cleanDraw node)

removeImmediateBound::Tree EChar->Forest EChar
removeImmediateBound Node{rootLabel=Bound,subForest=subForest} = subForest
removeImmediateBound node@Node{subForest=[item]} = [node {subForest=removeImmediateBound item}]
removeImmediateBound node = [node]


lookaheadChoice::Forest EChar->Forest EChar
lookaheadChoice items =
    case filter isBound items of
        [tree]->removeImmediateBound tree
        [] -> case filter (not . (isError True)) items of
            [] -> [Node {rootLabel=concatErrors (map getError items), subForest=[]}]
            [item] -> removeImmediateBound item
            items -> items
        _ -> error ("There are two bounds appearing in the tree at the same time:\n  ----"
            ++ intercalate "\n  ----" (map show items) ++ drawForest (map (fmap show) items))

simplifyUsingLookahead::Tree EChar->Tree EChar
simplifyUsingLookahead (node@Node {subForest=[oneNode]}) = node {subForest=[simplifyUsingLookahead oneNode]}
simplifyUsingLookahead (node@Node {subForest=[]}) = node
simplifyUsingLookahead (Node {rootLabel=rootLabel, subForest=nextNodes}) =
    Node {rootLabel=rootLabel, subForest=lookaheadChoice (simplifyUsingLookahead <$> nextNodes)}


partial2CorrectPath::[(EString, Tree EChar)]->EString
partial2CorrectPath [(es, tree)] = rootLabel tree:correctPath (subForest tree)
partial2CorrectPath [] = []
partial2CorrectPath items = --jtrace ("\n\ndog: " ++ drawForest (map (fmap show) (map snd items))) $
    case filter (isBound.snd) items of
        [(output, tree)]->output ++ partial2CorrectPath [(e "", tree)]
        [] -> case filter (not.(isError False).snd) items of
            [] -> [concatErrors (map (getError.snd) items)]
            [(output, tree)] -> output ++ partial2CorrectPath [(e "", tree)]
            nonErrorItems -> if (hasSync (snd $ head nonErrorItems)) then partial2CorrectPath (map movePastNextSync nonErrorItems)
                    else error "ambiguous parse"
        _ -> error ("There are two bounds appearing in the tree at the same time:\n  ----"
            ++ cleanDrawForest (snd <$> items))

correctPath::Forest EChar->EString
correctPath forest = partial2CorrectPath (map (\x -> (e "", x)) forest)

removeDoubleSyncs::Tree EChar->Tree EChar
removeDoubleSyncs (Node {rootLabel=rootLabel, subForest=[next@(Node {rootLabel=Sync _})]}) = removeDoubleSyncs next
removeDoubleSyncs (Node {rootLabel=rootLabel, subForest=next}) =
    Node {rootLabel=rootLabel, subForest=map removeDoubleSyncs next}

createParserForClass::String->Grammar->Parser
createParserForClass startRule g s =
    --jtrace "\nResulting Forest:"
--    jtrace (drawForest (map (fmap show) (map simplifyUsingLookahead forest))) $
    --jtrace (cleanDrawForest forest) $
    --jtrace "\nIn between\n" $
    --jtrace (cleanDrawForest (assignVariables forest)) $
--    jtrace (cleanDrawForest (simplifyUsingLookahead <$> forest)) $
--    jtrace (cleanDrawForest (simplifyUsingLookahead <$> (assignVariables forest))) $
    --jtrace "End Resulting Forest\n"
        enhancedString2String (correctPath (simplifyUsingLookahead <$> (assignVariables forest)))
            where forest=rawParse (name2Exp g startRule) (Just (createLString s))

createParser::Grammar->Parser
createParser g = createParserForClass (main g) g
