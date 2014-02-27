{-# OPTIONS_GHC -Wall #-}

module OperatorNames (
    symbol2Name,
    op2Name
) where

import Data.Char
import Data.List

import Grammar

--import JTrace

symbol2Name::Sequence->String
symbol2Name (TextMatch text _:rest) = op2Name text ++ symbol2Name rest
symbol2Name (WhiteSpace _ _:rest) = symbol2Name rest
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
op2Name x | all isAlpha x = x
op2Name x = error ("Unknown operator in op2Name: \'" ++ x ++ "'")
