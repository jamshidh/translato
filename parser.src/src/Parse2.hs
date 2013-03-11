{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
--
-- Module      :  Parse2
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

module Parse2 (
    mainLike
) where

import Data.List
import Data.Maybe
import Text.XML
import Data.Either

import Debug.Trace

type Attribute = (String, String)

data ParseError = ParseError { errorPosition::Position, description::String } deriving (Show)

type CharParser a = Position->String->[State a]

data State a = Error Position String |
    Pending (CharParser a) [a] | Done [a]


type Parser a = (String->Either [ParseError] [a])

(|||)::CharParser a->CharParser a->CharParser a
(a ||| b) position s = a position s ++ b position s

string::String->CharParser Char
string "" _ s = error "String to match in 'string' is empty"
string textToMatch position "" = [Error position "Early EOF in 'string'"]
string textToMatch position s =
    if (head s == c) then (if (next == "") then [Done [c]] else [Pending (string next) [c]])
        else [Error position ("Error, expecting \"" ++ textToMatch ++ "\", but got \"" ++ take (length textToMatch) s ++ "\"")]
            where next = tail textToMatch; c = head textToMatch

evolve::TimeSlice a->TimeSlice a
evolve (TimeSlice { count = oldCount, position = oldPosition, input = oldInput, states = oldStates }) =
    TimeSlice {
        count = oldCount + 1,
        position = nextPosition (head oldInput) oldPosition,
        input = tail oldInput,
        states = concat (map (evolveState oldPosition oldInput) oldStates) }

nextPosition::Char->Position->Position
nextPosition c (p@Position { line=oldLine, column=oldColumn }) =
    p {
        line = oldLine + if (c == '\n') then 1 else 0,
        column = if (c == '\n') then 1 else oldColumn + 1
        }

evolveState::Position->String->State a->[State a]
evolveState position s (Pending charParser value) = map (prependValue value) (charParser position s)
evolveState _ s x = [x]

prependValue::[a]->State a->State a
prependValue value x = case x of
            Done value2 -> Done (value ++ value2)
            Pending charParser value2 ->  Pending charParser (value ++ value2)
            Error p x -> Error p x

data Position = Position { line::Integer, column::Integer, filename::String }

instance Show Position where
    show p = filename p ++ ": line " ++ show (line p) ++ ", column " ++ show (column p)

data TimeSlice a = TimeSlice { count::Integer, position::Position, input::String, states::[State a] }

type Universe a = [TimeSlice a]

showState::(Show a)=>State a->String
showState (Error position e) = "Error[" ++ show position ++ ": " ++ e ++ "]"
showState (Pending charParser value) = "Pending[" ++ show value ++ "]"
showState (Done value) = "Done[" ++ show value ++ "]"

showTimeSlice::(Show a)=>TimeSlice a->String
showTimeSlice t = "Slice " ++ show (count t) ++ " (" ++ show (position t) ++ ")\n"
    ++ "Input = \"" ++ input t ++ "\"\nStates = [" ++ intercalate ", " (map showState (states t)) ++ "]"

showUniverse::(Show a)=>Universe a->String
showUniverse (slice:rest) |  notPending (states slice) = showTimeSlice slice ++ "\n-----------\n"
showUniverse (slice:rest) = showTimeSlice slice ++ "\n-----------\n" ++ showUniverse rest

createUniverse::String->CharParser a->String->Universe a
createUniverse name charParser s = iterate evolve (TimeSlice { count=1, position = Position { line=1, column=1, filename=name }, input=s, states=[Pending charParser []] })

getResult::Universe a->Either [ParseError] [[a]]
getResult = summarizeResult . states . finalTimeSlice

parse::String->CharParser Char->Parser String
parse name charParser s = getResult (createUniverse name charParser s)

finalTimeSlice::Universe a->TimeSlice a
finalTimeSlice u = fromJust $ find timeSliceIsNotPending u

summarizeResult::[State a]->Either [ParseError] [[a]]
summarizeResult x | hasAtLeastOneDone x = Right $ dones x
summarizeResult x = Left $ errors x

timeSliceIsNotPending::TimeSlice a->Bool
timeSliceIsNotPending t = notPending $ states t

notPending::[State a]->Bool
notPending (Pending _ _:rest) = False
notPending (first:rest) = notPending rest
notPending [] = True

hasAtLeastOneDone::[State a]->Bool
hasAtLeastOneDone (Done _ :rest) = True
hasAtLeastOneDone (first:rest) = hasAtLeastOneDone rest
hasAtLeatOneDone [] = False

dones::[State a]->[[a]]
dones ((Done value):rest) = value:(dones rest)
dones (x:rest) = dones rest
dones [] = []

errors::[State a]->[ParseError]
errors ((Error position value):rest) = ParseError position value:(errors rest)
errors (x:rest) = errors rest
errors [] = []


showErrors::[ParseError]->String
showErrors list = intercalate ", or " (map show list)



mainLike::IO ()
mainLike =
    do
        let universe = createUniverse "qqqq" ((string "abcd\nq") ||| (string "aq")) "abcd\nqqqq\n"
        putStrLn (showUniverse universe)
        case getResult universe of
            Right val -> putStrLn (show val)
            Left e -> putStrLn $ showErrors e


