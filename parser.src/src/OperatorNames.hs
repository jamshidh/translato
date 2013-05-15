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
    OperatorSymbol,
    --expandOperators,
    op2Name
) where

import Data.List
import Data.Map hiding (map, foldl, filter)
import Data.Text hiding (concat, map, filter, length)

import Debug.Trace

import Grammar

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

op2Name::OperatorSymbol->String
op2Name (TextMatch s:rest) = rawOp2Name s ++ (op2Name rest)
op2Name (x:rest) = op2Name rest

rawOp2Name::String->String
rawOp2Name "+" = "plus"
rawOp2Name "-" = "minus"
rawOp2Name "*" = "times"
rawOp2Name "/" = "divide"
rawOp2Name "." = "dot"
rawOp2Name "==" = "equals"
rawOp2Name "<" = "lessThan"
rawOp2Name ">" = "greaterThan"
rawOp2Name "<=" = "lessThanOrEquals"
rawOp2Name ">=" = "greaterThanOrEquals"
rawOp2Name x = error ("Unknown operator in op2Name: \'" ++ x ++ "'")
