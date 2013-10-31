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
    rangeAt,
    singleCharacterRangeAt,
    message
) where

import Data.Functor
import Data.List

import Format
import qualified LString as LS

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

rangeAt::LS.LString->Int->Range
rangeAt s length = (positionAt s, positionAt (LS.drop length s))

singleCharacterRangeAt::LS.LString->Range
singleCharacterRangeAt s = rangeAt s 1

data ParseError =
    Error { ranges::[Range], description::String } |
    ExpectationError { ranges::[Range], expected::[String] } |
    MatchError { name::String, ranges::[Range], first::String, second::String } |
    AmbiguityError { ranges::[Range] }
    deriving (Eq, Ord, Show)

message::ParseError->String
message Error{description=description} = description
message ExpectationError{expected=expected} =
    "Expected "
    ++ if length expected == 0
        then "[Empty list]"
        else intercalate ", or " (show <$> expected)
message MatchError{name=name, first=first, second=second} =
    show name ++ "s don't match: " ++ show first ++ " != " ++ show second
message AmbiguityError{} = "Ambiguity Error"

instance Format ParseError where
    format (Error ranges description) = "[" ++ intercalate ", " (formatRange <$> ranges) ++ "]\n    --"
        ++ description
    format (ExpectationError ranges expected) = "[" ++ intercalate ", " (formatRange <$> ranges) ++ "]\n    --Expected: "
        ++ if length expected == 0
            then "[Empty list]"
            else intercalate ", " expected
    format (MatchError name ranges first second) = "[" ++ intercalate ", " (formatRange <$> ranges) ++ "]\n    --"
        ++ name ++ " didn't match: first=" ++ first ++ ", second=" ++ second
    format (AmbiguityError ranges) = "[" ++ intercalate ", " (formatRange <$> ranges) ++ "]\n    --"
        ++ "AmbiguityError"
