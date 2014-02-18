-----------------------------------------------------------------------------
--
-- Module      :  OperatorNames
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

module OperatorNames (
    --OperatorSymbol,
    --expandOperators,
    symbol2Name,
    op2Name
) where

import Data.List
import Data.Map hiding (map, foldl, filter)
import Data.Text hiding (concat, map, filter, length)

import Grammar

import Debug.Trace

--import Grammar

{--
operatorList2Expressions::(RuleName, [OperatorSymbol])->[(RuleName, Sequence)]
operatorList2Expressions (name, []) = []
operatorList2Expressions (name, (o1:rest)) =
    (op2Name o1, [SepBy2 [Or ((map (\x -> [Link (op2Name x)]) rest) ++ [[Link ("#" ++ name ++ "_terminals")]])]
            [TextMatch o1]])
        :operatorList2Expressions (name, rest)

operatorFreeRules::[(RuleName, Sequence)]->Map RuleName [OperatorSymbol]->[(RuleName, Sequence)]
operatorFreeRules rules opDefinitions = filter (\(ruleName, _) -> notMember ruleName opDefinitions) rules

addTerminals::[(RuleName, Sequence)]->Map RuleName [OperatorSymbol]->[(RuleName, Sequence)]
addTerminals rules opDefinitions =
    (map (\(n, e) -> ("#" ++ n ++ "_terminals", e)) operatorRules)
        ++ (map (\(n, e) -> (n, [Link ("#" ++ n ++ "_terminals")])) operatorRules)
    where operatorRules = filter (\(ruleName, _) -> member ruleName opDefinitions) rules

operatorExpressions::Grammar->[(RuleName, Sequence)]
operatorExpressions g = concat (map operatorList2Expressions (toList $ operatorDefinitions g))

expandOperators::Grammar->Grammar
expandOperators g = g {
        elementRules = operatorFreeRules (elementRules g) (operatorDefinitions g)
            ++ addTerminals (elementRules g) (operatorDefinitions g)
            ++ operatorExpressions g,
        operatorDefinitions = fromList []
        }--}

{--op2Name::OperatorSymbol->String
op2Name (TextMatch s:rest) = rawOp2Name s ++ (op2Name rest)
op2Name (x:rest) = op2Name rest--}

symbol2Name::Sequence->String
symbol2Name (TextMatch text _:rest) = op2Name text ++ symbol2Name rest
symbol2Name (WhiteSpace _:rest) = symbol2Name rest
symbol2Name [] = []


op2Name::String->String
op2Name "+" = "plus"
op2Name "-" = "minus"
op2Name "*" = "times"
op2Name "/" = "divide"
op2Name "^" = "power"
op2Name "." = "dot"
op2Name "==" = "equals"
op2Name "!=" = "doesNotEqual"
op2Name "<" = "lessThan"
op2Name ">" = "greaterThan"
op2Name "<=" = "lessThanOrEquals"
op2Name ">=" = "greaterThanOrEquals"
op2Name x = error ("Unknown operator in op2Name: \'" ++ x ++ "'")
