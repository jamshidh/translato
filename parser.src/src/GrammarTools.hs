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
    expandOperators,
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

strip::Expression->Expression
strip (Sequence ((WhiteSpace defaultText):rest)) = Sequence (removeLastWhitespace rest)
strip (Or x) = Or (map strip x)
strip x = x

removeLastWhitespace::[Expression]->[Expression]
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

leftFactor::Expression->Expression
leftFactor (Or e) = addOrIfMultiple
    (map
        (\(first, second) -> eSequence first second)
        (M.toList (M.fromListWith GrammarTools.or (map takeFirstExpression e))))

or::Expression->Expression->Expression
or (Or list1) (Or list2) = Or (list1 ++ list2)
or (Or list1) e = Or (e:list1)
or e (Or list1) = Or (e:list1)
or e1 e2 = Or [e1, e2]

eSequence::Expression->Expression->Expression
eSequence Blank e = e
eSequence e Blank = e
eSequence (Sequence list1) (Sequence list2) = Sequence (list1 ++ list2)
eSequence (Sequence list1) e = Sequence (list1 ++ [e])
eSequence e (Sequence list1) = Sequence (e:list1)
eSequence e1 e2 = Sequence [e1, e2]

addSequenceIfMultiple::[Expression]->Expression
--addSequenceIfMultiple [] = Blank
addSequenceIfMultiple [e] = e
addSequenceIfMultiple list = Sequence list

addOrIfMultiple::[Expression]->Expression
addOrIfMultiple [] = Blank
addOrIfMultiple [e] = e
addOrIfMultiple list = Or list

takeFirstExpression::Expression->(Expression, Expression)
takeFirstExpression (Sequence (first:rest)) = (first, addSequenceIfMultiple rest)
takeFirstExpression x = (x, Blank)

match::Expression->Expression->Maybe [Expression]
match Variable e = Just [e]
match (Or [p1, p2]) (Or [e1, e2]) = case (m11, m22) of
    (Just m1, Just m2) -> Just (m1 ++ m2)
    (x, y) -> case (m12, m21) of
        (Just m1, Just m2) -> Just (m1 ++ m2)
        (x, y) -> Nothing
    where m11 = match p1 e1; m22 = match p2 e2; m21 = match p2 e1; m12 = match p1 e2
match (Sequence (p:rest1)) (Sequence (e:rest2)) = case result of
    Just m -> case result2 of
        Just m2 -> Just (m ++ m2)
        Nothing ->  Nothing
    Nothing -> Nothing
    where result = match p e; result2 = match (addSequenceIfMultiple rest1) (addSequenceIfMultiple rest2)
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
match (Sequence _) _ = Nothing

--match pattern e = Nothing

---------------------

expandOperators::Grammar->Grammar
expandOperators g = g {
        elementRules = map addOperators (elementRules g),
        assignments = map addOperators (assignments g)
        }
        where addOperators (name, e) =
                if (member name (operatorDefinitions g))
                    then (name, buildExpressionParser ((operatorDefinitions g) ! name) e)
                    else (name, e)

buildExpressionParser::[OperatorSymbol]->Expression->Expression
buildExpressionParser [] e = e
buildExpressionParser (symbol:rest) e =
    Sequence [ MultiElementWrapper (op2Name symbol)
        (SepBy (buildExpressionParser rest e) (symbol2Expression symbol))]

table2Expression::[OperatorSymbol]->Expression->Expression
table2Expression [] terminal = terminal
table2Expression (symbol:rest) terminal = Sequence [ NestedElement (op2Name symbol) (SepBy (table2Expression rest terminal) (TextMatch symbol))]

symbol2Expression::OperatorSymbol->Expression
symbol2Expression symbol = addSequenceIfMultiple (symbol2ExpressionList symbol)

symbol2ExpressionList::OperatorSymbol->[Expression]
symbol2ExpressionList [] = []
symbol2ExpressionList s | isSpace $ head s =
    let (spaces, rest) = span isSpace s in (WhiteSpace spaces):symbol2ExpressionList rest
symbol2ExpressionList s =
    let (spaces, rest) = span (not . isSpace) s in (TextMatch spaces):symbol2ExpressionList rest

---------------------
addEOFToGrammar::Grammar->Grammar
addEOFToGrammar g = g {
        elementRules = map (\(name, e) -> if (name == startSymbol g) then (name, addEOF e) else (name, strip e)) (elementRules g),
        assignments = map (\(name, e) -> if (name == startSymbol g) then (name, addEOF e) else (name, strip e)) (assignments g)
    }

addEOF::Expression->Expression
addEOF x = Sequence [x, EOF]

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

simplify::Expression->Expression
----
simplify (Or ((Or x):y)) = Or (x++y)
simplify (Or x) = leftFactor (Or (map simplify x))
---- The remainder are a recursive identity
simplify (Attribute s e) = Attribute s (simplify e)
simplify (SepBy e separator) = SepBy (simplify e) (simplify separator) --(Or [Blank, Sequence [simplify e, List $ simplify exp]])
    --where exp = Sequence [(ReturnBlank separator), e]
simplify (List e) = List (simplify e)
simplify (ReturnBlank (WhiteSpace defaultText)) = WhiteSpace defaultText
simplify (MultiElementWrapper name e) = MultiElementWrapper name (simplify e)



simplify (Sequence x) = addSequenceIfMultiple (removeSequence $ map simplify x)
simplify x = x

--bindWhitespaces::[Expression]->[Expression]
--bindWhitespaces (WhiteSpace _:next:rest) = (IgnorableWhiteSpacePrefix next):bindWhitespaces rest
--bindWhitespaces (x:rest) = x:(bindWhitespaces rest)
--bindWhitespaces [] = []

removeSequence::[Expression]->[Expression]
removeSequence (Sequence list:rest) = list ++ removeSequence rest
removeSequence (x:rest) = x:removeSequence rest
removeSequence [] = []


