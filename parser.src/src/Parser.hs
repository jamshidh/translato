{-# LANGUAGE CPP, TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser (
    createParserForClass,
    createParser
) where

import Prelude hiding (lookup)
import Data.Char hiding (Space)
import Data.Graph.Inductive.Query.Monad
import Data.Maybe
--import Data.Text as DT hiding (map, length, foldl, foldl1, empty, head, tail, filter, intercalate, isPrefixOf, take, drop, find)
import Data.Tree
import Text.XML
import Data.List as L hiding (union, lookup, insert)
import Data.Map hiding (map, foldl, filter)

import CharSet
import EnhancedString
import Grammar
import GrammarTools
import LString (LString, line, col, string, createLString)
import qualified LString as LS
import OperatorNames
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

removeBind::Sequence->Sequence
removeBind (Bind:rest)=removeBind rest
removeBind (x:rest) = x:removeBind rest
removeBind [] = []

haveSameStart::(Eq a, Show a)=>[[a]]->Bool
haveSameStart [] = False
haveSameStart [[]] = False
haveSameStart sequences = (length $ nub (map head sequences)) == 1

pushCondition::Context->Condition->Context
pushCondition cx cn =
    cx {
        conditions=cn:(conditions cx),
        attributes=empty:(attributes cx)
    }

popCondition::Context->Context
popCondition cx =
    cx {
        conditions=tail (conditions cx),
        attributes=tail (attributes cx)
    }

addCharToCurrentAttribute::Context->EChar->Context
addCharToCurrentAttribute cx c = case currentAttribute cx of
    Nothing -> cx
    Just (name, partialString) -> case c of
            Ch c2 -> cx { currentAttribute=Just (name, partialString ++ [c2]) }
            _ -> error "Attributes must only parse strings"

beginCurrentAttribute::Context->String->Context
beginCurrentAttribute cx name = case currentAttribute cx of
    Just _ -> error "beginCurrentAttribute called twice without moving attribute"
    Nothing -> cx { currentAttribute=Just (name, []) }

moveCurrentAttributeToAttributes::Context->Context
moveCurrentAttributeToAttributes cx = case currentAttribute cx of
    Nothing -> error "Can't call moveCurrentAttributeToAttributes unless currentAttribute is non-null"
    Just (name, value) -> cx {
                            currentAttribute=Nothing,
                            attributes=insert name value (head atts):tail atts
                            }
                        where atts = attributes cx

getSingleCondition::[Condition]->Condition
getSingleCondition cns = case nub cns of
    [cn] -> cn
    _ -> error "Elements with the same name must have the same condition"


continue::Context->EString->Sequence->LString->Forest EChar
--continue cx item (Bind:rest) s = continue cx (item ++ [Bound]) rest s
--continue cx item (JustOutput value:rest) s = continue cx (item ++ value) rest s
--continue cx item (WhiteSpace _:JustOutput value:rest) s = continue cx (item ++ value) (WhiteSpace " ":rest) s
--continue cx item (WhiteSpace _:Bind:rest) s = continue cx (item ++ [Bound]) (WhiteSpace " ":rest) s
--continue cx item (JustOutput value:Bind:rest) s = continue cx (item ++ [Bound]) (JustOutput value:rest) s
--continue cx (c:rest) seq s | string s == [] = [Node {rootLabel=c, subForest=continue cx rest seq s}]
--continue cx [] seq s | string s == [] = [Node {rootLabel=Sync, subForest=rawParse cx seq (LS.tail s)}]

continue cx (c:rest) seq s = [Node {rootLabel=c, subForest=
    continue (addCharToCurrentAttribute cx c) rest seq s}]
continue cx [] seq s = [Node {rootLabel=Sync, subForest=rawParse cx seq (LS.tail s)}]


rawParse::Context->Sequence->LString->Forest EChar
rawParse cx (EOF:rest) s | string s == [] = [Node {rootLabel=Ch '\n', subForest=rawParse cx rest s}]
rawParse cx (EOF:rest) s = expectErr s "EOF"
--rawParse cx seq s | string s == [] =  [err s ("File ends too soon, expecting " ++ show seq)]--}
rawParse cx [] s = [Node { rootLabel=(Ch '\n'), subForest=[] }]

rawParse cx (TextMatch [c]:rest) s | LS.isPrefixOf [c] s = continue cx [] rest s
rawParse cx (TextMatch matchString:rest) s | LS.isPrefixOf matchString s =
        continue cx [] (TextMatch (tail matchString):rest) s
rawParse cx (TextMatch matchString:rest) s = expectErr s matchString

rawParse cx (Attribute name seq:rest) s =
    rawParse nextCx ([JustOutput [AStart name]] ++ seq ++ [JustOutput [AEnd]] ++ rest) s
        where nextCx = beginCurrentAttribute cx name

rawParse cx (Tab _ e:rest) s = rawParse cx (e ++ rest) s

--rawParse cx (Or sequences:rest) s | haveSameStart sequences =
--    rawParse cx ([head $ head $ sequences] ++ [Or (map tail sequences)] ++ rest) s
rawParse cx (Or sequences:rest) s =
    concat  (map (\seq -> rawParse cx (seq ++ rest) s) sequences)

rawParse cx (Bind:rest) s = rawParse cx (JustOutput [Bound]:rest) s

rawParse cx (SepBy count seq@[Character charset]:rest) s = rawParse cx (List count seq:rest) s

rawParse cx (SepBy 0 seq:rest) s =
    rawParse cx [Or [seq ++ [Bind] ++ [List 0 (separator ++ seq)] ++ cleanedRest, cleanedRest]] s
    where separator = (seq2Separator cx) seq; cleanedRest = removeBind rest
rawParse cx (SepBy count seq:rest) s =
    rawParse cx (seq ++ [List (count -1) (separator++seq)] ++ rest) s
    where separator = (seq2Separator cx) seq

rawParse cx (List 0 [Character charset]:rest) s | LS.null s =
    rawParse cx rest s
rawParse cx (List 0 [Character charset]:rest) s | LS.head s `isIn` charset =
    continue cx [Ch (LS.head s)] (List 0 [Character charset]:rest) s
rawParse cx (List 0 [Character charset]:rest) s =
    rawParse cx rest s

rawParse cx (List 0 seq:rest) s = rawParse cx [Or [seq ++ [Bind] ++ [List 0 seq] ++ cleanedRest, cleanedRest]] s
    where cleanedRest = removeBind rest
rawParse cx (List min seq:rest) s = rawParse cx (seq ++ [List (min-1) seq] ++ rest) s

rawParse cx (Link name:rest) s =
    case lookup name (allSubstitutionsWithName cx) of
            Just [(condition, sequence)] -> rawParse (pushCondition cx condition) (sequence ++ rest) s
            Just cnAndSeqs ->
            {--case nub (map fst cnAndSeqs) of
                [cn] ->
                    rawParse
                        (pushCondition cx cn)
                        (leftFactor (Or (map snd cnAndSeqs):rest))
                        s
                _ -> concat  (map (\seq -> rawParse cx (seq ++ rest) s) sequences)--}

                concat $ map (\(cn, seq) -> rawParse (pushCondition cx cn) seq s)
                    (map (mapSnd (\seqs -> leftFactor (Or seqs:rest))) (toList cnSeqMap))
                            where cnSeqMap = --jtrace ("qqqq" ++ intercalate "\n  " (map show cnAndSeqs)) $
                                    fromListWith (++) (map (\(a, b) -> (a, [b])) cnAndSeqs)


            Nothing -> error ("The grammar links to a non-existant rule named '" ++ name ++ "'")

rawParse cx (WhiteSpace _:rest) s | LS.null s = rawParse cx rest s
--rawParse cx seq@(WhiteSpace _:rest) s | isSpace (LS.head s) = continue cx [] seq s
rawParse cx seq@(WhiteSpace _:rest) s | isSpace (LS.head s) = rawParse cx seq (LS.tail s)
rawParse cx (WhiteSpace _:rest) s = rawParse cx rest s

rawParse cx (Character charset:rest) s | LS.null s = expectErr s (show charset)
rawParse cx (Character charset:rest) s | LS.head s `isIn` charset = continue cx [Ch (LS.head s)] rest s
rawParse cx (Character charset:rest) s = expectErr s (show charset)

rawParse cx (Ident:rest) s | LS.null s = expectErr s "Ident"
rawParse cx (Ident:rest) s | isAlpha (LS.head s) =
    rawParse cx (List 1 [Character (CharSet False [WordChar])]:rest) s
rawParse cx (Ident:rest) s = expectErr s "Ident"

rawParse cx (JustOutput (EEnd:remainingChars):rest) s =
    [Node { rootLabel=EEnd, subForest=rawParse (popCondition cx) (JustOutput remainingChars:rest) s}]

rawParse cx (JustOutput (AEnd:remainingChars):rest) s =
    case eval (head $ attributes newCx) (head $ conditions cx) of
        Just CnFalse -> err s "Condition failed"
        _ -> [Node { rootLabel=AEnd, subForest=rawParse newCx (JustOutput remainingChars:rest) s}]
    where newCx = moveCurrentAttributeToAttributes cx

rawParse cx (JustOutput (char:remainingChars):rest) s = [Node { rootLabel=char, subForest=rawParse cx (JustOutput remainingChars:rest) s}]
rawParse cx (JustOutput []:rest) s = rawParse cx rest s

rawParse cx (x:_) s = error ("Missing case in rawParse: " ++ show x)

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
isBound (Node {rootLabel=Sync})=False
isBound (Node {subForest=[tree]})=isBound tree
isBound (Node {subForest=[]})=False
isBound tree@(Node {subForest=_})= False
--    if safe then False
--        else error ("split before bound ambiguity resolved: " ++ drawTree (fmap show tree))

isError::Bool->Tree EChar->Bool
isError safe (Node {rootLabel=(Error _ _)})=True
isError safe (Node {rootLabel=(ExpectationError _ _)})=True
isError safe (Node {rootLabel=Sync})=False
isError safe (Node {subForest=[tree]})=isError safe tree
isError safe (Node {subForest=[]})=False
isError safe tree@(Node {subForest=_})=
    if safe then False
        else error ("split before ambiguity resolved: " ++ drawTree (fmap show tree))

hasSync::Tree EChar->Bool
hasSync (Node {rootLabel=Sync})=True
hasSync (Node {subForest=[next]})=hasSync next
hasSync (Node {subForest=[]})=False

getError::Tree EChar->EChar
getError (Node {rootLabel=err@(Error _ _)})=err
getError (Node {rootLabel=err@(ExpectationError _ _)})=err
getError (Node {rootLabel=Sync})=error "There should have been an error"
getError (Node {subForest=[tree]})=getError tree

movePastNextSync::(EString, Tree EChar)->(EString, Tree EChar)
movePastNextSync (output, node@Node {rootLabel=Sync, subForest=[next]}) = (output ++ [Sync], next)
movePastNextSync (output, Node {rootLabel=rootLabel, subForest=[next]}) = movePastNextSync (output ++ [rootLabel], next)
movePastNextSync (output, Node {rootLabel=rootLabel, subForest=[]}) =
    (output ++ [rootLabel], Node {rootLabel=Sync, subForest=[]})
movePastNextSync (output, Node {rootLabel=rootLabel, subForest=subForest}) = error ("ambiguity within ambiguity: " ++ drawForest (map (fmap show) subForest))

lookaheadChoice::Forest EChar->Forest EChar
lookaheadChoice items =
    case filter fst (map (\x -> (isBound x, x)) items) of
        [(_, tree)]->[tree]
        [] -> case filter fst (map (\x -> (not (isError True x), x)) items) of
            [] -> [Node {rootLabel=concatErrors (map getError items), subForest=[]}]
            items -> map snd items
        _ -> error ("There are two bounds appearing in the tree at the same time:\n  ----"
            ++ intercalate "\n  ----" (map show items) ++ drawForest (map (fmap show) items))

simplifyUsingLookahead::Tree EChar->Tree EChar
simplifyUsingLookahead (Node {rootLabel=rootLabel, subForest=[oneNode]}) =
    Node {rootLabel=rootLabel, subForest=[simplifyUsingLookahead oneNode]}
simplifyUsingLookahead (Node {rootLabel=rootLabel, subForest=[]}) =
    Node {rootLabel=rootLabel, subForest=[]}
simplifyUsingLookahead (Node {rootLabel=rootLabel, subForest=nextNodes}) =
    Node {rootLabel=rootLabel, subForest=lookaheadChoice (map simplifyUsingLookahead nextNodes)}


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
            ++ intercalate "\n  ----" (map show items) ++ drawForest (map (fmap show) (map snd items)))

correctPath::Forest EChar->EString
correctPath forest = partial2CorrectPath (map (\x -> (e "", x)) forest)

removeDoubleSyncs::Tree EChar->Tree EChar
removeDoubleSyncs (Node {rootLabel=rootLabel, subForest=[next@(Node {rootLabel=Sync})]}) = removeDoubleSyncs next
removeDoubleSyncs (Node {rootLabel=rootLabel, subForest=next}) =
    Node {rootLabel=rootLabel, subForest=map removeDoubleSyncs next}


createParserForClass::String->Context->Parser
createParserForClass startRule cx s = --jtrace (drawForest (map (fmap show) (map simplifyUsingLookahead forest))) $
        enhancedString2String (correctPath (map (simplifyUsingLookahead) forest))
            where forest=rawParse cx [Link startRule] (createLString s)

createParser::Context->Parser
createParser cx = createParserForClass (main (grammar cx)) cx
