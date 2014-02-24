
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
import Data.Monoid

import Format
import qualified LString as LS

-- Position is a location in the input....

data Position = Position { line::Int, column::Int, filename::String }

formatPosition::Position->String
formatPosition p = "(Line: " ++ show (line p + 1) ++ ", Col: " ++ show (column p + 1) ++ ")"

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
    NullError | --Only put in to make this a monoid....  This should never appear outside of this file
    Error { ranges::[Range], description::String } |
    ExpectationError { ranges::[Range], expected::[String], actual::LS.LString } |
    MatchError { name::String, ranges::[Range], firstVal::String, secondVal::String } |
    AmbiguityError { ranges::[Range] }
    deriving (Eq, Ord, Show)

--TODO Figure out how to merge cases where actual1 /= actual2
instance Monoid ParseError where
  mempty = NullError
  mappend (ExpectationError r1 expected1 actual1) (ExpectationError r2 expected2 actual2) | r1 == r2 && actual1 == actual2 = ExpectationError r1 (expected1++expected2) actual1
  mappend err1@(ExpectationError _ _ actual1) err2@(ExpectationError _ _ actual2) | actual1 /= actual2 = 
                          error ("error in ParseError mappend: trying to merge two expectation errors with different actuals:\n----" ++ format err1 ++ "\n----" ++ format err2)
  mappend (ExpectationError r1 expected1 actual1) (ExpectationError r2 expected2 actual2) = ExpectationError (r1++r2) (expected1++expected2) actual1
  mappend NullError e = e
  mappend e NullError = e
  
message::ParseError->String
message Error{description=description} = description
message ExpectationError{expected=expected} =
    "Expected "
    ++ if length expected == 0
        then "[Empty list]"
        else intercalate ", or " (show <$> expected)
message MatchError{name=name, firstVal=firstVal, secondVal=secondVal} =
    show name ++ "s don't match: " ++ show firstVal ++ " != " ++ show secondVal
message AmbiguityError{} = "Ambiguity Error"

instance Format ParseError where
    format (Error ranges description) = "[" ++ intercalate ", " (formatRange <$> ranges) ++ "]\n    --"
        ++ description
    format (ExpectationError ranges expected actual) = "\n    --location: " ++ intercalate ", " (formatRange <$> ranges) ++ "\n    --Expected: "
        ++ if length expected == 0
            then "[Empty list]"
            else intercalate ", " expected
        ++ "\n    --input = " ++ shortShowString (LS.string actual)
    format (MatchError name ranges firstVal secondVal) = "[" ++ intercalate ", " (formatRange <$> ranges) ++ "]\n    --"
        ++ name ++ " didn't match: first=" ++ firstVal ++ ", second=" ++ secondVal
    format (AmbiguityError ranges) = "[" ++ intercalate ", " (formatRange <$> ranges) ++ "]\n    --"
        ++ "AmbiguityError"

shortShowString::String->String
shortShowString s | length s <= 20 = show s
shortShowString s = show $ take 20 s ++ "...."

