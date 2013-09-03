-----------------------------------------------------------------------------
--
-- Module      :  ParseError
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

module ParseError (
    ParseError(..),
    Position(..),
    Range,
    format,
    formatRange,
    singleCharacterRangeAt,
    message
) where

import Data.Functor
import Data.List

import qualified LString as LS

format::ParseError->String
format (Error ranges description) = "[" ++ intercalate ", " (formatRange <$> ranges) ++ "]\n    --"
    ++ description


-- Position is a location in the input....

data Position = Position { line::Int, column::Int, filename::String }

formatPosition::Position->String
formatPosition p = "(Line: " ++ show (line p) ++ ", Col: " ++ show (column p) ++ ")"

instance Eq Position where
    p1 == p2 = (line p1 == line p2) && (column p1 == column p2)

instance Ord Position where
    p1 <= p2 | line p1 < line p2 = True
    p1 <= p2 | line p1 > line p2 = False
    p1 <= p2 | line p1 == line p2 = column p1 <= column p2

instance Show Position where
    show p = filename p ++ ": line " ++ show (line p) ++ ", column " ++ show (column p)

positionAt::LS.LString->Position
positionAt s = Position{line=LS.line s,column=LS.col s,filename="qq"}

type Range = (Position, Position)

formatRange::Range->String
formatRange (start, end) = formatPosition start ++ "-" ++ formatPosition end

---------------------

singleCharacterRangeAt::LS.LString->Range
singleCharacterRangeAt s = (positionAt s, positionAt (LS.tail s))

data ParseError =
    Error { ranges::[Range], description::String } |
    ExpectationError { ranges::[Range], expected::[String] } |
    MatchError { ranges::[Range], first::String, second::String }
    deriving (Eq, Ord, Show)

message::ParseError->String
message Error{description=description} = description
message ExpectationError{expected=expected} = "Expected " ++ intercalate ", or " (show <$> expected)
