
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
import Data.Int
import Data.List
import Data.Monoid
import qualified Data.Text.Lazy as TL

import Format
import qualified LString as LS

-- Position is a location in the input....

data Position = Position { line::Int64, column::Int64, filename::String }

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

rangeAt::LS.LString->Int64->Range
rangeAt s length = (positionAt s, positionAt (LS.drop length s))

singleCharacterRangeAt::LS.LString->Range
singleCharacterRangeAt s = rangeAt s 1

data ParseError =
    NullError { ranges::[Range] } | --Only put in to make this a monoid....  This should never appear outside of this file.
    --Although this should never appear in the wild, I was running into a case where it did, and the program was crashing when it tried to use ranges.  I've added it here now to fix the problem, although I really should just figure out what caused that to happen.  Yeah, I know that is lame, but I have too much other stuff to do.
    Error { ranges::[Range], description::String } |
    ExpectationError { ranges::[Range], expected::[String], actual::LS.LString } |
    MatchError { name::String, ranges::[Range], firstVal::String, secondVal::String } |
    AmbiguityError { ranges::[Range], expected::[String] }
    deriving (Eq, Ord, Show)

--TODO Figure out how to merge cases where actual1 /= actual2
instance Monoid ParseError where
  mempty = NullError []
  mappend (ExpectationError r1 expected1 actual1) (ExpectationError r2 expected2 actual2) | r1 == r2 && actual1 == actual2 = ExpectationError r1 (expected1++expected2) actual1
  mappend err1@(ExpectationError _ _ actual1) err2@(ExpectationError _ _ actual2) | actual1 /= actual2 = 
                          error ("error in ParseError mappend: trying to merge two expectation errors with different actuals:\n----" ++ format err1 ++ "\n----" ++ format err2)
  mappend (ExpectationError r1 expected1 actual1) (ExpectationError r2 expected2 actual2) = ExpectationError (r1++r2) (expected1++expected2) actual1
  mappend (NullError []) e = e
  mappend e (NullError []) = e
  mappend x y = error ("Internal Error: unknown case in ParseError mappend\n--------\n" ++ format x ++ "\n--------\n" ++ format y)
  
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
        ++ "\n    --input = " ++ shortShowText (LS.string actual)
    format (MatchError name ranges firstVal secondVal) = "[" ++ intercalate ", " (formatRange <$> ranges) ++ "]\n    --"
        ++ name ++ " didn't match: first=" ++ firstVal ++ ", second=" ++ secondVal
    format (AmbiguityError ranges expected) = "[" ++ intercalate ", " (formatRange <$> ranges) ++ "]\n"
        ++ "    --AmbiguityError\n"
        ++ "    --expected: " ++ intercalate ", " expected ++ "\n"

shortShowText::TL.Text->String
shortShowText s | TL.length s <= 20 = show s
shortShowText s = (show $ TL.take 20 s) ++ "...."

