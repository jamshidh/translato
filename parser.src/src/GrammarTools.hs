-----------------------------------------------------------------------------
--
-- Module      :  GrammarTools
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

module GrammarTools (
    --fullySimplifyGrammar,
    --simplify,
    --rewriteLeftRecursionInGrammar,
    --expandOperators,
    rule2Seq,
    addEOFToGrammar,
    stripWhitespaceFromGrammar,
    orIfy,
    loadGrammar,
    fixG
) where

import Data.Char
import Data.Functor
import Data.Graph.Inductive.Query.Monad
import Data.List
import Data.Map as M hiding (filter, map, foldl, null)
import Data.Maybe
import GHC.Exts

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec as P hiding (try)


--import Debug.Trace

import EnhancedString
import Grammar
import GrammarParser
import OperatorNames

import JDebug

stripWhitespaceFromGrammar::Grammar->Grammar
stripWhitespaceFromGrammar g = g
    {
        classes =
            (\c ->
                if (className c == main g)
                    then c
                    else stripWhitespaceFromClass c)
                            <$> (classes g)

    }

stripWhitespaceFromClass::Class->Class
stripWhitespaceFromClass c = c { rules = stripRule <$> rules c }

stripRule rule = rule{rawSequence=strip (rawSequence rule)}

strip::Sequence->Sequence
strip ((WhiteSpace defaultText):rest) = removeLastWhitespace rest
strip x = removeLastWhitespace x

removeLastWhitespace::Sequence->Sequence
removeLastWhitespace (e:[WhiteSpace defaultText]) = [e]
removeLastWhitespace (e:rest) = e:(removeLastWhitespace rest)
removeLastWhitespace [] = []

---------------------

rule2Seq::Rule->Sequence
rule2Seq rule = []



rawSeqMap f rule@Rule{rawSequence=seq} = rule{rawSequence=f seq}

rewriteLeftRecursionInGrammar::Grammar->Grammar
rewriteLeftRecursionInGrammar g =
    g {
        classes = fmap (rewriteLeftRecursionInClass g) (classes g)
    }

rewriteLeftRecursionInClass::Grammar->Class->Class
rewriteLeftRecursionInClass g cl =
    cl {
        rules = filter (not . (isLRecursive (className cl))) (rules cl),
        suffixSeqs = (recursiveRule2SuffixSeq <$> (filter (isLRecursive (className cl)) (rules cl)))
                            ++ (operator2SuffixSeq g cl <$> operators cl)
    }

operator2SuffixSeq::Grammar->Class->Sequence->Sequence
operator2SuffixSeq g cl seq = seq++[Out [InfixTag 0 (opSeq2Name seq)], Or (replicate 1 <$> Link <$> nonRecursiveRuleNames cl)]
    where nonRecursiveRuleNames cl = (name <$> filter (not . isLRecursive (className cl)) (rules cl))
                                        ++ (className <$> parents g cl)
recursiveRule2SuffixSeq::Rule->Sequence
recursiveRule2SuffixSeq rule = (Out [InfixTag 0 (name rule)]:) $ tail $ rawSequence rule

isLRecursive::String->Rule->Bool
isLRecursive className Rule{name=ruleName, rawSequence=Link linkName:rest}
    | (linkName == ruleName) || (linkName == className)
    = True
isLRecursive _ _ = False

{--removeLeftRecursion::(RuleName, Expression)->[(RuleName, Expression)]
removeLeftRecursion (name, e) = case result of
        Nothing -> [(name, e)]
        Just [nonRecursivePart, ruleAfter] ->
            [
                (name, Sequence [nonRecursivePart, Link ("#" ++ name)]),
                ("#" ++ name, Or [Blank, Sequence [ruleAfter, Link ("#" ++ name)]])
            ]
        where result = match (Or [Variable, Sequence [Link name, Variable]]) e--}



{--or::Expression->Expression->Expression
or (Or list1) (Or list2) = Or (list1 ++ list2)
or (Or list1) e = Or (e:list1)
or e (Or list1) = Or (e:list1)
or e1 e2 = Or [e1, e2]--}

{--addOrIfMultiple::[Expression]->Expression
addOrIfMultiple [] = Blank
addOrIfMultiple [e] = e
addOrIfMultiple list = Or list--}

{--match::Sequence->Sequence->Maybe [Expression]
match Variable e = Just [e]
match (Or [p1, p2]) (Or [e1, e2]) = case (m11, m22) of
    (Just m1, Just m2) -> Just (m1 ++ m2)
    (x, y) -> case (m12, m21) of
        (Just m1, Just m2) -> Just (m1 ++ m2)
        (x, y) -> Nothing
    where m11 = match p1 e1; m22 = match p2 e2; m21 = match p2 e1; m12 = match p1 e2
match (p:rest1) (e:rest2) = case result of
    Just m -> case result2 of
        Just m2 -> Just (m ++ m2)
        Nothing ->  Nothing
    Nothing -> Nothing
    where result = match p e; result2 = match rest1 rest2
match (Sequence (_:_)) (Sequence []) = Nothing
match (Sequence []) (Sequence (_:_)) = Nothing
match (Sequence []) (Sequence []) = Just []
match (Link name1) (Link name2) | name1 == name2 = Just []
match (Link _) _ = Nothing
match (TextMatch name1) (TextMatch name2) | name1 == name2 = Just []
match (TextMatch _) _ = Just []
match (InfixElement name1) (InfixElement name2) | name1 == name2 = Just []
match (InfixElement _) _ = Just []
match (Or list1) (Or list2) = error ("huh, Or " ++ show list1 ++ ", Or " ++ show list2)
match (Or _) _ = Nothing
match (Sequence _) _ = Nothing--}

--match pattern e = Nothing

---------------------

{--symbol2Sequence::OperatorSymbol->Sequence
symbol2Sequence symbol = symbol2ExpressionList symbol

symbol2ExpressionList::OperatorSymbol->[Expression]
symbol2ExpressionList [] = []
symbol2ExpressionList s | isSpace $ head s =
    let (spaces, rest) = span isSpace s in (WhiteSpace spaces):symbol2ExpressionList rest
symbol2ExpressionList s =
    let (spaces, rest) = span (not . isSpace) s in (TextMatch spaces):symbol2ExpressionList rest--}

---------------------
addEOFToGrammar::Grammar->Grammar
addEOFToGrammar g = g {
        classes =
            (\c -> if (className c == main g) then addEOFToClass c else c) <$>
                (classes g)
    }

addEOFToClass::Class->Class
addEOFToClass c = c { rules=(\rule->rule{rawSequence=rawSequence rule ++ [EOF]}) <$> (rules c)}

orIfy::[Sequence]->Sequence
orIfy [seq] = seq
orIfy [] = []
orIfy seqs = [Or (seqs >>= removeOr)]
    where
        removeOr (Or seqs:rest) = (++ rest) <$> seqs
        removeOr seq = [seq]

-----------------------

loadGrammar::String->IO Grammar
loadGrammar filename =
    do
        specHandle<-openFile filename ReadMode
        grammarFile<-TL.hGetContents specHandle
        case P.parse parseGrammar "grammar" (TL.unpack grammarFile) of
            Left err -> error ("Error parsing grammar: " ++ show err)
            Right grammar -> return grammar

fixG = rewriteLeftRecursionInGrammar . addEOFToGrammar . stripWhitespaceFromGrammar

--        task opts ((rewriteLeftRecursionInGrammar . addEOFToGrammar . stripWhitespaceFromGrammar) grammar)











{--fullySimplifyGrammar::Grammar->Grammar
fullySimplifyGrammar g = fst $ fromJust $ find (\(g1, g2) -> g1 == g2) (zip simplifiedProgression (tail simplifiedProgression))
    where
        simplifiedProgression = (stripWhitespaceFromGrammar g):(map simplifyGrammar  simplifiedProgression)

simplifyGrammar::Grammar->Grammar
simplifyGrammar g = g
    {
        classes = map (\(name, e) -> (name, simplify e)) (classes g)
    }

simplify::Sequence->Sequence
simplify [] = []
simplify (x:rest) = simplifyExpression x ++ simplify rest

--bindWhitespaces::[Expression]->[Expression]
--bindWhitespaces (WhiteSpace _:next:rest) = (IgnorableWhiteSpacePrefix next):bindWhitespaces rest
--bindWhitespaces (x:rest) = x:(bindWhitespaces rest)
--bindWhitespaces [] = []

simplifyExpression::Expression->Sequence
--simplifyExpression (Or ([Or x]:y)) = [Or (x++y)]
--simplifyExpression (Or x) = leftFactor (Or (map simplify x))
simplifyExpression (Attribute s e) = [Attribute s (simplify e)]
simplifyExpression (Reparse e1 e2) = [Reparse (simplify e1) (simplify e2)]
simplifyExpression (Tab s e) = [Tab s (simplify e)]
simplifyExpression (List min e) = [List min (simplify e)]
simplifyExpression e@(WhiteSpace _) = [e]
simplifyExpression e@(Link _) = [e]
simplifyExpression e@(TextMatch _) = [e]
simplifyExpression Ident = [Ident]
simplifyExpression Number = [Number]
simplifyExpression EOF = [EOF]
simplifyExpression e = error ("Missing case in simplifyExpression: " ++ show e)--}

