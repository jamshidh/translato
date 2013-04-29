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
    fullySimplifyGrammar,
    simplify,
    --removeLeftRecursionFromGrammar,
    --expandOperators,
    addEOFToGrammar,
    stripWhitespaceFromGrammar
) where

import Data.Char
import Data.List
import Data.Map as M hiding (filter, map, foldl)
import Data.Maybe

import Debug.Trace

import GrammarParser
import OperatorNames

stripWhitespaceFromGrammar::Grammar->Grammar
stripWhitespaceFromGrammar g = Grammar
    {
        startSymbol = startSymbol g,
        elementRules = map (\(name, e) -> if (name == startSymbol g) then (name, e) else (name, strip e)) (elementRules g),
        assignments = map (\(name, e) -> if (name == startSymbol g) then (name, e) else (name, strip e)) (assignments g),
        operatorDefinitions = operatorDefinitions g

    }

strip::Sequence->Sequence
strip ((WhiteSpace defaultText):rest) = removeLastWhitespace rest
strip ((Or x):rest) = (Or (map strip x)):rest
strip x = x

removeLastWhitespace::Sequence->Sequence
removeLastWhitespace (e:[WhiteSpace defaultText]) = [e]
removeLastWhitespace (e:rest) = e:(removeLastWhitespace rest)
removeLastWhitespace [] = []

---------------------

{--removeLeftRecursionFromGrammar::Grammar->Grammar
removeLeftRecursionFromGrammar g =
    g {
        elementRules = concat $ (map removeLeftRecursion (elementRules g)),
        assignments = fromList $ concat $ (map removeLeftRecursion (toList $ assignments g))
    }

removeLeftRecursion::(RuleName, Expression)->[(RuleName, Expression)]
removeLeftRecursion (name, e) = case result of
        Nothing -> [(name, e)]
        Just [nonRecursivePart, ruleAfter] ->
            [
                (name, Sequence [nonRecursivePart, Link ("#" ++ name)]),
                ("#" ++ name, Or [Blank, Sequence [ruleAfter, Link ("#" ++ name)]])
            ]
        where result = match (Or [Variable, Sequence [Link name, Variable]]) e --}

instance Ord Expression where
    a <= b = show a <= show b

debugHead::Show a=>[a]->a
debugHead x= (trace $ show x) $ head x

leftFactor::Expression->Sequence
leftFactor (Or []) = []
leftFactor (Or [x]) = x
leftFactor (Or sequences) | all ((head $ head sequences) ==) (map head sequences) =
    (head $ head sequences):(leftFactor (Or (map tail sequences)))
leftFactor x = [x]

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

expandOperators2::Grammar->Grammar
expandOperators2 g = g {
        elementRules = map addOperators (elementRules g),
        assignments = map addOperators (assignments g)
        }
        where addOperators (name, e) =
                if (member name (operatorDefinitions g))
                    then (name, buildExpressionParser ((operatorDefinitions g) ! name) e)
                    else (name, e)

buildExpressionParser::[OperatorSymbol]->Sequence->Sequence
buildExpressionParser [] e = e
buildExpressionParser (symbol:rest) e =
    [MultiElementWrapper (op2Name symbol)
        [SepBy (buildExpressionParser rest e) (symbol2Sequence symbol)]]

{--table2Expression::[OperatorSymbol]->Expression->Expression
table2Expression [] terminal = terminal
table2Expression (symbol:rest) terminal = [NestedElement (op2Name symbol) [SepBy (table2Expression rest terminal) (TextMatch symbol)]]--}

symbol2Sequence::OperatorSymbol->Sequence
symbol2Sequence symbol = symbol2ExpressionList symbol

symbol2ExpressionList::OperatorSymbol->[Expression]
symbol2ExpressionList [] = []
symbol2ExpressionList s | isSpace $ debugHead s =
    let (spaces, rest) = span isSpace s in (WhiteSpace spaces):symbol2ExpressionList rest
symbol2ExpressionList s =
    let (spaces, rest) = span (not . isSpace) s in (TextMatch spaces):symbol2ExpressionList rest

---------------------
addEOFToGrammar::Grammar->Grammar
addEOFToGrammar g = g {
        elementRules = map (\(name, e) -> if (name == startSymbol g) then (name, e ++ [EOF]) else (name, strip e)) (elementRules g),
        assignments = map (\(name, e) -> if (name == startSymbol g) then (name, e ++ [EOF]) else (name, strip e)) (assignments g)
    }

fullySimplifyGrammar::Grammar->Grammar
fullySimplifyGrammar g = fst $ fromJust $ find (\(g1, g2) -> g1 == g2) (zip simplifiedProgression (tail simplifiedProgression))
    where
        simplifiedProgression = (stripWhitespaceFromGrammar g):(map simplifyGrammar  simplifiedProgression)

simplifyGrammar::Grammar->Grammar
simplifyGrammar g = g
    {
        elementRules = map (\(name, e) -> (name, simplify e)) (elementRules g),
        assignments = map (\(name, e) -> (name, simplify e)) (assignments g)
    }

simplify::Sequence->Sequence
simplify [] = []
simplify (x:rest) = simplifyExpression x ++ simplify rest

--bindWhitespaces::[Expression]->[Expression]
--bindWhitespaces (WhiteSpace _:next:rest) = (IgnorableWhiteSpacePrefix next):bindWhitespaces rest
--bindWhitespaces (x:rest) = x:(bindWhitespaces rest)
--bindWhitespaces [] = []

simplifyExpression::Expression->Sequence
simplifyExpression (Or ([Or x]:y)) = [Or (x++y)]
simplifyExpression (Or x) = leftFactor (Or (map simplify x))
simplifyExpression (Attribute s e) = [Attribute s (simplify e)]
simplifyExpression (MultiElementWrapper name e) = [MultiElementWrapper name (simplify e)]
simplifyExpression (Reparse e1 e2) = [Reparse (simplify e1) (simplify e2)]
simplifyExpression (SepBy e separator) = [Or [[SepBy1 e separator], [Blank]]]
simplifyExpression (SepBy1 e separator) = simpE ++ [List (simpSep ++ simpE)]
    where simpE = simplify e; simpSep = simplify separator
simplifyExpression (SepBy2 e separator) = simpE ++ simpSep ++ simpE ++ [List (simpSep ++ simpE)]
    where simpE = simplify e; simpSep = simplify separator
simplifyExpression (Tab s e) = [Tab s (simplify e)]
simplifyExpression (List e) = [List (simplify e)]
simplifyExpression e@(WhiteSpace _) = [e]
simplifyExpression e@(Link _) = [e]
simplifyExpression e@(TextMatch _) = [e]
simplifyExpression e@(StringOf _) = [e]
simplifyExpression Ident = [Ident]
simplifyExpression EIdent = [EIdent]
simplifyExpression Number = [Number]
simplifyExpression Blank = [Blank]
simplifyExpression EOF = [EOF]
simplifyExpression e = error ("Missing case in simplifyExpression: " ++ show e)


