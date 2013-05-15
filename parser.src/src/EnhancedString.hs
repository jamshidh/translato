-----------------------------------------------------------------------------
--
-- Module      :  EnhancedString
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

module EnhancedString (
    EChar (Ch, EStart, EEnd, AStart, AEnd, TabLeft, TabRight, Bound, Sync, ExpectationError, Error),
    EString,
    concatErrors,
    e,
    chs2String,
    enhancedString2String,
    truncateString
) where

import Data.Char
import Data.List

import Colors
import LString hiding (head, tail, take)

--import Debug.Trace
import JDebug

data EChar = Ch Char
    | EStart String [String]
    | EEnd
    | AStart String
    | AEnd
    | TabRight String
    | TabLeft
    | Bound
    | Sync
    | ExpectationError [String] LString
    | Error String LString deriving (Eq)

instance Show EChar where
    show (Ch '\n') = "\\n"
    show (Ch c) = [c]
    show (EStart name attributeNames) = "<" ++ name
        ++ (if (length attributeNames > 0) then " " ++ intercalate " " attributeNames else "")
        ++ ">"
    show (EEnd) = "</>"
    show (AStart name) = "{@" ++ name ++ "="
    show (AEnd) = "}"
    show (TabLeft) = "<=="
    show (TabRight tabString) = "==>(" ++ tabString ++ ")"
    show Bound = "<Bound>"
    show Sync = "<Sync>"
    show (ExpectationError expected s) = "{ExpectationError: " ++ show expected ++ "}"
    show (Error message s) = "{Error: " ++ message ++ "}"

type EString = [EChar]

e::String->EString
e (x:rest) = Ch x:(e rest)
e [] = []

chs2String::[String]->EString->String
chs2String tl (Ch x:rest) = x:chs2String tl rest
chs2String tl (Error err s:rest) =
    red ("\nError(line:" ++ show (line s) ++ ",col:" ++ show (col s) ++ "): " ++ err ++ "\n") ++ chs2String tl rest
chs2String tl (ExpectationError err s:rest) = red ("\nError(line:" ++ show (line s) ++ ",col:" ++ show (col s) ++ "): "
    ++ "Expecting " ++ intercalate " or " (map show err) ++ ", but got " ++ show (truncateString 10 (string s)) ++ "\n") ++ chs2String tl rest
chs2String tl (eChar@(EStart name attributeNames):rest) = show eChar ++ chs2String (name:tl) rest
chs2String (firstTag:remainingTags) (EEnd:rest) = "</" ++ firstTag ++ ">" ++ chs2String remainingTags rest
chs2String [] (EEnd:rest) = red "</[empty]>" ++ chs2String [] rest --error "End tag appears, but tagList is empty"
chs2String tl (AStart name:rest) = "{@" ++ name ++ "=" ++ chs2String tl rest
chs2String tl (AEnd:rest) = "}" ++ chs2String tl rest
chs2String tl (TabRight _:_) = error "There shouldn't be a tabright in chs2String"
chs2String tl (TabLeft:_) = error "There shouldn't be a tableft in chs2String"
chs2String tl (Bound:rest) = chs2String tl rest
chs2String tl (Sync:rest) = chs2String tl rest
chs2String tl [] = []

concatErrors::[EChar]->EChar
concatErrors [] = error "Can't call concatErrors with empty list"
concatErrors [err] = err
concatErrors (ExpectationError expectation s:rest) =
    ExpectationError (expectation ++ expectationRest) s
        where expectationRest = getExpectation (concatErrors rest);

getExpectation::EChar->[String]
getExpectation (ExpectationError expectation _) = expectation

theString::EChar->LString
theString (Error _ s) = s
theString (ExpectationError _ s) = s

enhancedString2String::EString->String
enhancedString2String es =
    ((chs2String []) . expandWhitespace . (expandTabs [])) es
--enhancedString2String = debugOutput

expandTabs::[String]->EString->EString
expandTabs tab ((TabRight tabString):rest) = e tabString ++ expandTabs (tabString:tab) rest
expandTabs tab (TabLeft:rest) = expandTabs (tail tab) rest
expandTabs tab (Ch '\n':rest) = Ch '\n':(e (concat tab) ++ expandTabs tab rest)
expandTabs tab (x:rest) = x:(expandTabs tab rest)
expandTabs tab [] = []

expandWhitespace::EString->EString
expandWhitespace (Ch x:Ch '_':Ch y:rest) | isAlphaNum x && isAlphaNum y = Ch x:Ch ' ':Ch y:(expandWhitespace rest)
expandWhitespace (Ch '_':rest) = expandWhitespace rest
expandWhitespace (c:rest) = c:(expandWhitespace rest)
expandWhitespace [] = []

truncateString::Int->String->String
truncateString newLength s | length s <= newLength = s
truncateString newLength s = take newLength s ++ "...."

debugOutput::EString->String
debugOutput (TabLeft:rest) = "<==" ++ debugOutput rest
debugOutput (TabRight tabString:rest) = "==>(" ++ tabString ++ ")" ++ debugOutput (tail rest)
debugOutput (Ch c:rest) = c:debugOutput rest
debugOutput [] = []
