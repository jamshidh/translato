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
--import Data.Text as DT hiding (map, length, foldl, foldl1, empty, head, tail, filter, intercalate, isPrefixOf, take, drop, find)
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

expectErr::LString->String->Forest EChar
expectErr s expectation = [Node { rootLabel=ExpectationError [expectation] s, subForest=[]}]

-------------------------------

haveSameStart::(Eq a, Show a)=>[[a]]->Bool
haveSameStart [] = False
haveSameStart [[]] = False
haveSameStart sequences = (length $ nub (map head sequences)) == 1

getSingleCondition::[Condition]->Condition
getSingleCondition cns = case nub cns of
    [cn] -> cn
    _ -> error "Elements with the same name must have the same condition"

{--continue::Context->EString->Sequence->LString->Forest EChar
--continue cx item (Bind:rest) s = continue cx (item ++ [Bound]) rest s
--continue cx item (JustOutput value:rest) s = continue cx (item ++ value) rest s
--continue cx item (WhiteSpace _:JustOutput value:rest) s = continue cx (item ++ value) (WhiteSpace " ":rest) s
--continue cx item (WhiteSpace _:Bind:rest) s = continue cx (item ++ [Bound]) (WhiteSpace " ":rest) s
--continue cx item (JustOutput value:Bind:rest) s = continue cx (item ++ [Bound]) (JustOutput value:rest) s
--continue cx (c:rest) seq s | string s == [] = [Node {rootLabel=c, subForest=continue cx rest seq s}]
--continue cx [] seq s | string s == [] = [Node {rootLabel=Sync, subForest=rawParse cx seq (LS.tail s)}]

continue cx (c:rest) seq s = --jtrace ("Seq=" ++ show seq) $
    [Node {rootLabel=c, subForest=continue cx rest seq s}]
continue cx [] seq s = --jtrace ("Seq2=" ++ show seq) $
    [Node {rootLabel=Sync (LS.head s), subForest=rawParse cx seq (LS.tail s)}]--}

forestConcat::Forest a->Forest a->Forest a
forestConcat [] b = b
forestConcat items b = (\item -> item{subForest=forestConcat (subForest item) b}) <$> items

(+++)::Forest a->Forest a->Forest a
(+++) = forestConcat

seq2ParseTree::SequenceMap->Sequence->Forest Expression
seq2ParseTree sMap (Link name:rest) =
    case lookup name sMap of
        Nothing -> error ("The grammar links to a non-existant rule named '" ++ name ++ "'")
        Just seq ->
            forestConcat
                            (seq2ParseTree sMap seq)
                            (seq2ParseTree sMap rest)
{--seq2ParseTree sMap (List count seq@[Character charset]:rest) =
    seq2ParseTree sMap (List count seq:rest)--}

seq2ParseTree sMap (List 0 seq:rest) =
    seq2ParseTree sMap [Or [seq ++ [List 0 seq] ++ rest, rest]]
seq2ParseTree sMap (List count seq:rest) =
    seq2ParseTree sMap (seq ++ [List (count -1) seq] ++ rest)
seq2ParseTree sMap (Or seqs:rest) =
--    forestConcat
        (((++ rest) <$> seqs) >>= seq2ParseTree sMap)
--        (seq2ParseTree sMap rest)
seq2ParseTree sMap (e:rest) = [Node{rootLabel=e, subForest=seq2ParseTree sMap rest}]
seq2ParseTree sMap [] = []



rawParse::Forest Expression->LString->Forest EChar
rawParse [Node{rootLabel=EOF, subForest=rest}] s | string s == [] = [Node {rootLabel=Ch '\n', subForest=rawParse rest s}]
rawParse [Node{rootLabel=EOF, subForest=rest}] s = expectErr s "EOF"
--rawParse cx seq s | string s == [] =  [err s ("File ends too soon, expecting " ++ show seq)]--}
rawParse [] s = [Node { rootLabel=(Ch '\n'), subForest=[] }]

rawParse [Node{rootLabel=TextMatch matchString, subForest=rest}] s | LS.isPrefixOf matchString s =
        rawParse rest (LS.drop (length matchString) s)
rawParse [Node{rootLabel=TextMatch matchString, subForest=_}] s = expectErr s matchString

rawParse [Node{rootLabel=AStart name, subForest=rest}] s =
    [Node {rootLabel=VStart ("@" ++ name) s, subForest=rawParse rest s}]
rawParse [Node{rootLabel=AEnd, subForest=rest}] s =
    [Node {rootLabel=E.VEnd, subForest=rawParse rest s}]

rawParse [Node{rootLabel=TabStart _, subForest=rest}] s = rawParse rest s
rawParse [Node{rootLabel=TabEnd, subForest=rest}] s = rawParse rest s

{--rawParse cx (Bind:rest) s = --rawParse cx (JustOutput [Bound]:rest) s
    [Node { rootLabel=Bound, subForest=rawParse cx rest s}]--}

rawParse [Node{rootLabel=G.EStart tagName attributes, subForest=rest}] s =
    [Node { rootLabel=E.EStart tagName attributes, subForest=rawParse rest s}]

rawParse [Node{rootLabel=G.EEnd tagName, subForest=rest}] s =
    [Node { rootLabel=E.EEnd tagName, subForest=rawParse rest s}]

{--rawParse cx (List 0 [Character charset]:rest) s | LS.null s =
    rawParse cx rest s
rawParse cx (List 0 [Character charset]:rest) s | LS.head s `isIn` charset =
    Node{rootLabel=Ch (LS.head s), subForest=(List 0 [Character charset]:rest) s
rawParse cx (List 0 [Character charset]:rest) s =
    rawParse cx rest s--}


rawParse [Node{rootLabel=G.InfixTag priority name, subForest=rest}] s = --rawParse cx [Or seqsWithOutputs] s
--    where seqsWithOutputs =
--            map (\(priority, name, symbol) -> symbol ++ [JustOutput [InfixOpSymbol priority name]] ++ rest) symbols
    [Node { rootLabel=E.InfixTag priority name, subForest=rawParse rest s}]


rawParse [Node{rootLabel=WhiteSpace _, subForest=rest}] s | LS.null s = rawParse rest s
--rawParse cx seq@(WhiteSpace _:rest) s | isSpace (LS.head s) = continue cx [] seq s
rawParse [Node{rootLabel=WhiteSpace _, subForest=rest}] s | isSpace (LS.head s) = rawParse rest (LS.tail s)
rawParse [Node{rootLabel=WhiteSpace _, subForest=rest}] s = rawParse rest s

rawParse [Node{rootLabel=Character charset, subForest=rest}] s | LS.null s =
    expectErr s (formatCharSet charset)
rawParse [Node{rootLabel=Character charset, subForest=rest}] s | LS.head s `isIn` charset =
    [Node{rootLabel=Ch (LS.head s), subForest=rawParse rest (LS.tail s)}]
rawParse [Node{rootLabel=Character charset, subForest=rest}] s =
    expectErr s (formatCharSet charset)

{--rawParse Ident:rest) s | LS.null s = expectErr s "Ident"
rawParse (Ident:rest) s | isAlpha (LS.head s) =
    rawParse (List 1 [Character (CharSet False [WordChar])]:rest) s
rawParse (Ident:rest) s = expectErr s "Ident"--}

{--rawParse cx (JustOutput (EEnd:remainingChars):rest) s =
    [Node { rootLabel=EEnd, subForest=rawParse (popCondition cx) (JustOutput remainingChars:rest) s}]--}

{--rawParse cx (JustOutput (AEnd:remainingChars):rest) s =
    case eval (head $ attributes newCx) (head $ conditions cx) of
        Just CnFalse -> err s "Condition failed"
        _ -> [Node { rootLabel=AEnd, subForest=rawParse newCx (JustOutput remainingChars:rest) s}]
    where newCx = moveCurrentAttributeToAttributes cx--}

{--rawParse cx (JustOutput (char:remainingChars):rest) s =
    [Node { rootLabel=char, subForest=rawParse cx (JustOutput remainingChars:rest) s}]
rawParse cx (JustOutput []:rest) s = rawParse cx rest s--}

rawParse [x] _ = error ("Missing case in rawParse: " ++ safeDrawTree (fmap show x))

rawParse items s = rawParse [chooseOne items s] s

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
--        else error ("split before bound ambiguity resolved: " ++ safeDrawTree (fmap show tree))

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
            ++ safeDrawForest (map (fmap show) items))

simplifyUsingLookahead::Tree EChar->Tree EChar
simplifyUsingLookahead (node@Node {subForest=[oneNode]}) = node {subForest=[simplifyUsingLookahead oneNode]}
simplifyUsingLookahead (node@Node {subForest=[]}) = node
simplifyUsingLookahead (Node {rootLabel=rootLabel, subForest=nextNodes}) =
    Node {rootLabel=rootLabel, subForest=lookaheadChoice (simplifyUsingLookahead <$> nextNodes)}


partial2CorrectPath::[(EString, Tree EChar)]->EString
partial2CorrectPath [(es, tree)] = rootLabel tree:correctPath (subForest tree)
partial2CorrectPath [] = []
partial2CorrectPath items = --jtrace ("\n\ndog: " ++ safeDrawForest (map (fmap show) (map snd items))) $
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

parseTree::Grammar->String->Forest Expression
parseTree g startRule=seq2ParseTree (sequenceMap g) [Link startRule]

createParserForClass::String->Grammar->Parser
createParserForClass startRule g s =
--    jtrace "\nResulting Forest:"
--    jtrace (safeDrawForest (map (fmap show) (map simplifyUsingLookahead forest))) $
--    jtrace (cleanDrawForest forest) $
--    jtrace "\nIn between\n" $
--    jtrace (cleanDrawForest (assignVariables forest)) $
--    jtrace (cleanDrawForest (simplifyUsingLookahead <$> forest)) $
--    jtrace (cleanDrawForest (simplifyUsingLookahead <$> (assignVariables forest))) $
--    jtrace "End Resulting Forest\n"
        enhancedString2String (correctPath (map (simplifyUsingLookahead) (assignVariables forest)))
            where
                forest=rawParse (parseTree g startRule) (createLString s)

createParser::Grammar->Parser
createParser g = createParserForClass (main g) g
