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

module ManyWorldsParser (
    State(Error, Pending, Done),
    ParseError,
    err,
    errorPosition,
    description,
    parse,
    many,
    sepBy,
    sMap,
    sMap2,
    (|||),
    blank,
    eof,
    string,
    char,
    noneOf,
    ident,
    number,
    eIdent,
    ignorableWhitespacePrefix,
    combine,
    (<->>),
    string1Of
) where

import Data.List
import Data.Maybe
import Text.XML
import Data.Either
import Data.Char

--import Debug.Trace

type Attribute = (String, String)

-- Position is a location in the input....

data Position = Position { line::Integer, column::Integer, filename::String }

instance Eq Position where
    p1 == p2 = (line p1 == line p2) && (column p1 == column p2)

instance Ord Position where
    p1 <= p2 | line p1 < line p2 = True
    p1 <= p2 | line p1 > line p2 = False
    p1 <= p2 | line p1 == line p2 = column p1 <= column p2

instance Show Position where
    show p = filename p ++ ": line " ++ show (line p) ++ ", column " ++ show (column p)

---------------------

data ParseError = ParseError { errorPosition::Maybe Position, description::String }

-- Error comparison is based on location in file....  Not severity
-- e1 == e2 if they are at the same place, etc.
-- (in "many worlds", this often corresponds to severity....  An earlier error can often be
-- erased by taking a different world path.  If an error occurs and no path is available, it is the
-- last one dealt with before failing, and probably the one you want to see).


instance Eq ParseError where
    e1 == e2 = errorPosition e1 == errorPosition e2

instance Ord ParseError where
    e1 <= e2 = errorPosition e1 <= errorPosition e2

------------------------

data State a = Error ParseError |
    Pending (String->[State a]) | Done a

err::String->State a
err description = Error (ParseError { errorPosition = Nothing, description = description } )

type Parser a = (String->Either [ParseError] [a])

sMap::(a->b)->[State a]->[State b]
sMap f = map (singleSMap f)

singleSMap::(a->b)->State a->State b
singleSMap f (Pending f2) = Pending (\s -> (map (singleSMap f) (f2 s)))
singleSMap f (Done a) = Done (f a)
singleSMap f (Error e) = Error e

sMap2::(String->[State ([Element], [Attribute])])->[State String]->[State ([Element], [Attribute])]
sMap2 f x = concat $ map (singleSMap2 f) x

singleSMap2::(String->[State ([Element], [Attribute])])->State String->[State ([Element], [Attribute])]
singleSMap2 f (Error e) = [Error e]
singleSMap2 f (Pending f2) = [Pending (\s -> (sMap2 f) (f2 s))]
singleSMap2 f (Done v) = f v

evolve::TimeSlice a->TimeSlice a
evolve (TimeSlice { count = oldCount, position = oldPosition, input = oldInput, states = oldStates }) =
    TimeSlice {
        count = oldCount + 1,
        position = nextPosition (head oldInput) oldPosition,
        input = tail oldInput,
        states = map (fillInPosition oldPosition) (concat (map (evolveState oldInput) oldStates)) }

fillInPosition::Position->State a->State a
fillInPosition position (Error e) | errorPosition e == Nothing = Error e { errorPosition = Just position }
fillInPosition position x = x

nextPosition::Char->Position->Position
nextPosition c (p@Position { line=oldLine, column=oldColumn }) =
    p {
        line = oldLine + if (c == '\n') then 1 else 0,
        column = if (c == '\n') then 1 else oldColumn + 1
        }

evolveState::String->State a->[State a]
evolveState s (Pending charParser) = (charParser s)
evolveState s x = [x]

data TimeSlice a = TimeSlice { count::Integer, position::Position, input::String, states::[State a] }

type Universe a = [TimeSlice a]

showState::(Show a)=>State a->String
showState (Error e) = "Error[" ++ show e ++ "]"
showState (Pending charParser) = "Pending"
showState (Done value) = "Done[" ++ show value ++ "]"

showTimeSlice::(Show a)=>TimeSlice a->String
showTimeSlice t = "Slice " ++ show (count t) ++ " (" ++ show (position t) ++ ")\n"
    ++ "Input = \"" ++ input t ++ "\"\nStates = [" ++ intercalate ", " (map showState (states t)) ++ "]"

showUniverse::(Show a)=>Universe a->String
showUniverse (slice:rest) |  notPending (states slice) = showTimeSlice slice ++ "\n-----------\n"
showUniverse (slice:rest) = showTimeSlice slice ++ "\n-----------\n" ++ showUniverse rest

createUniverse::String->[State a]->String->Universe a
createUniverse name states s = iterate evolve
    (TimeSlice
        {
            count=1,
            position = Position { line=1, column=1, filename=name },
            input=s,
            states=states
        }
    )

getResult::Universe a->Either [ParseError] [a]
getResult = summarizeResult . states . finalTimeSlice

parse::String->[State a]->Parser a
parse name charParser string = getResult (createUniverse name charParser string)

finalTimeSlice::Universe a->TimeSlice a
finalTimeSlice u = fromJust $ find timeSliceIsNotPending u

summarizeResult::[State a]->Either [ParseError] [a]
summarizeResult x | hasAtLeastOneDone x = Right $ dones x
summarizeResult x = Left $ errors x

timeSliceIsNotPending::TimeSlice a->Bool
timeSliceIsNotPending t = notPending $ states t

notPending::[State a]->Bool
notPending (Pending _:rest) = False
notPending (first:rest) = notPending rest
notPending [] = True

hasAtLeastOneDone::[State a]->Bool
hasAtLeastOneDone (Done _ :rest) = True
hasAtLeastOneDone (first:rest) = hasAtLeastOneDone rest
hasAtLeastOneDone [] = False

dones::[State a]->[a]
dones ((Done value):rest) = value:(dones rest)
dones (x:rest) = dones rest
dones [] = []

errors::[State a]->[ParseError]
errors ((Error e):rest) = e:(errors rest)
errors (x:rest) = errors rest
errors [] = []

instance Show ParseError where
    show e = show (errorPosition e) ++ ": " ++ description e


showErrors::[ParseError]->String
showErrors list = intercalate ", or " (map show list)

---------------

blank::a->[State a]
blank x = [Done x]

combine::(a->b->c)->[State a]->[State b]->[State c]
combine f [] y = []
combine f (state:(rest1)) states = combineSingle f state states ++ combine f rest1 states
    where combineSingle f (Done v) states = sMap (f v) states
            ; combineSingle f (Error e) s2 = [Error e]
            ; combineSingle f (Pending f1) s2 = [Pending (\s -> combine f (f1 s) s2)]

(|||)::[State a]->[State a]->[State a]
(|||) s1 s2 = s1 ++ s2

-------------------------




many::[State a]->[State [a]]
many rule = [Done []] ++ rule <:> many rule

many1::[State a]->[State [a]]
many1 rule = rule <:> many rule

eof::a->[State a]
eof x = [Pending (\s -> if (s == []) then [Done x]
    else [err "File EOF expected"])]


(<:>)::[State a]->[State [a]]->[State [a]]
(<:>) x y = combine (:) x y


(<->>)::[State a]->[State b]->[State b]
x <->> y = combine (\a b->b) x y

sepBy1::[State a]->[State b]->[State [a]]
sepBy1 rule separator = rule <:> many (separator <->> rule)

sepBy::[State a]->[State b]->[State [a]]
sepBy rule separator = blank [] ||| sepBy1 rule separator
--whitespace::[State String]
--whitespace = stringOf space

ignorableWhitespacePrefix::[State a]->[State a]
ignorableWhitespacePrefix states = stringPrefixOf space states

conditionalChar::String->(Char->Bool)->[State Char]
conditionalChar humanReadableName test = [Pending (\s ->
    if (s == []) then [err ("Early EOF in 'string', expecting " ++ humanReadableName)]
        else (
            if (test (head s)) then [Done (head s)]
                else [err ("Error, expecting "
                    ++ humanReadableName ++ ", but got " ++ show  [head s])]))]

char c = conditionalChar ("'" ++ [c] ++ "'") (c ==)

anyChar::[State Char]
anyChar = [Pending (\s ->
    if (s == []) then [err ("Early EOF in 'string'")]
                    else [Done (head s)])]

--Greedy string with 1 char lookahead.  String must have at least one character.
string1Of::[State Char]->[State String]
string1Of charStates = case charStates of
    [Pending f] -> [Pending (\string@(first:rest) -> case (f string, f rest) of
        ([Done v1], [Done v2]) -> sMap (first:) (string1Of charStates)
        ([Done v1], [Error e2]) -> [Done [first]]
        ([Error e], _) -> [Error e])]

stringPrefixOf::[State Char]->[State a]->[State a]
--stringOf charStates = blank "" ||| string1Of charStates
stringPrefixOf charStates next = case charStates of
    [Pending f] -> [Pending (\string@(first:rest) -> case (f string, f rest) of
        ([Done v1], [Done v2]) -> stringPrefixOf charStates next
        ([Done v1], [Error e2]) -> next
        ([Error e], _) -> concat $ map (\f -> f string) (map getFunction next)
            )]

getFunction::State a->(String->[State a])
getFunction (Pending f) = f
getFunction (Error e) = error "unexpected error"
getFunction (Done value) = error "unexpected done"


space = conditionalChar "space" isSpace
letter = conditionalChar "letter" isLetter
digit = conditionalChar "letter" isDigit
noneOf forbiddenChars = conditionalChar ("one of '" ++ forbiddenChars ++ "'") (\c -> notElem c forbiddenChars)

string "" = [Done ""]
string (c:rest) = char c <:> string rest

eIdent = letter <:> many (letter ||| digit ||| char '.' ||| char '_' )
--ident = letter &&& many (letter ||| digit ||| char '_')
ident = letter <:> many (letter ||| digit ||| char '_')

number = digit <:> many digit

{--mainLike::IO ()
mainLike =
    do
        let universe = createUniverse "qqqq" ((string "abcd\nq") ||| (string "aq")) "abcd\nqqqq\n"
        putStrLn (showUniverse universe)
        case getResult universe of
            Right val -> putStrLn (show val)
            Left e -> putStrLn $ showErrors e --}


