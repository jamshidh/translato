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
    op2Name
) where

import Data.Text

type OperatorSymbol = String

op2Name::OperatorSymbol->String
op2Name symbol = rawOp2Name (unpack $ strip $ pack symbol)

rawOp2Name::OperatorSymbol->String
rawOp2Name "+" = "plus"
rawOp2Name "-" = "minus"
rawOp2Name "*" = "times"
rawOp2Name "/" = "divide"
rawOp2Name "." = "dot"
rawOp2Name "==" = "equals"
rawOp2Name x = error ("Unknown operator in op2Name: \'" ++ x ++ "'")
