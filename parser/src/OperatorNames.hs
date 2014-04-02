{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module OperatorNames (
    symbol2Name,
    op2Name
) where

import Data.Char
import Data.List
import qualified Data.Text.Lazy as TL

import Grammar

--import JDebug

symbol2Name::Sequence->TL.Text
symbol2Name [WhiteSpace _ _] = "space"
symbol2Name x = symbol2Name' x

symbol2Name'::Sequence->TL.Text
symbol2Name' (TextMatch text _:rest) = op2Name text `TL.append` symbol2Name rest
symbol2Name' (WhiteSpace _ _:rest) = symbol2Name rest
symbol2Name' [] = TL.empty


op2Name::TL.Text->TL.Text
op2Name "+" = "plus"
op2Name "-" = "minus"
op2Name "*" = "times"
op2Name "/" = "divide"
op2Name "%" = "mod"
op2Name "^" = "power"
op2Name "." = "dot"
op2Name "=" = "assign"
op2Name "+=" = "addAssign"
op2Name "-=" = "subAssign"
op2Name "==" = "equals"
op2Name "===" = "tripleEquals"
op2Name "||" = "or"
op2Name "&&" = "and"
op2Name "|" = "singleOr"
op2Name "&" = "singleAnd"
op2Name "!=" = "doesNotEqual"
op2Name "!==" = "tripleDoesNotEqual"
op2Name "<" = "lessThan"
op2Name ">" = "greaterThan"
op2Name "<=" = "lessThanOrEquals"
op2Name ">=" = "greaterThanOrEquals"
op2Name "<<" = "shiftLeft"
op2Name ">>" = "shiftRight"
op2Name "," = "comma"
op2Name x | all isAlpha (TL.unpack x) = x
op2Name x = error ("Unknown operator in op2Name: \'" ++ TL.unpack x ++ "'")
