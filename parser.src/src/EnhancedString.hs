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
    EChar (..),
    EString,
    concatErrors,
    e,
    chs2String,
    enhancedString2String,
    truncateString
) where

import Data.Char
import Data.Functor
import Data.List

import Colors
import LString hiding (head, tail, take)
import XPath

import JDebug

data EChar = Ch Char
    | EStart String [String] Condition
    | EEnd String
    | VPush
    | VPop
    | VOut String
    | VStart String LString
    | VEnd
    | VAssign String String
    | TabRight String
    | TabLeft
    | InfixTag Int String
    | Bound
    | Sync Char
    | ExpectationError [String] LString
    | Error String LString deriving (Eq, Ord)

instance Show EChar where
    show (Ch '\n') = "\\n"
    show (Ch c) = [c]
    show (EStart name attributes _) = cyan ("<" ++ name ++ concat (map (" " ++) attributes) ++ ">")
        --(if condition /= CnTrue then "(" ++ show condition ++ ")" ++ ">>"
    show (EEnd name) = cyan ("</" ++ name ++ ">")
    show VPush = magenta ">>VPush>>"
    show VPop = magenta "<<VPop<<"
    show (VOut name) = green ("[" ++ name ++ "]")
    show (VStart name _) = green ("{" ++ name ++ "=")
    show (VEnd) = green "}"
    show (VAssign name val) = green ("assign{" ++ name ++ "=" ++ val ++ "}")
    show (TabLeft) = magenta "<=="
    show (TabRight tabString) = magenta ("==>(" ++ tabString ++ ")")
    show Bound = magenta "<Bound>"
    show (Sync c) = blue (underline ([c]))
    show (InfixTag priority name) = "InfixOpSymbol(" ++ show priority ++ "," ++ name ++ ")"
    show (ExpectationError expected s) = red ("{ExpectationError: " ++ show expected ++ "}")
    show (Error message s) = red ("{Error: " ++ message ++ "}")

type EString = [EChar]

e::String->EString
e (x:rest) = Ch x:(e rest)
e [] = []

chs2String::EString->String
chs2String (Ch x:rest) = x:chs2String rest
chs2String (Error err s:rest) =
    red ("\nError(line:" ++ show (line s) ++ ",col:" ++ show (col s) ++ "): " ++ err ++ "\n") ++ chs2String rest
chs2String (ExpectationError err s:rest) = red ("\nError(line:" ++ show (line s) ++ ",col:" ++ show (col s) ++ "): "
    ++ "Expecting " ++ intercalate " or " (map show err) ++ ", but got " ++ show (truncateString 10 (string s)) ++ "\n") ++ chs2String rest
chs2String (VPush:rest) = magenta ">>VPush" ++ chs2String rest
chs2String (VPop:rest) = magenta "<<VPop<<" ++ chs2String rest
chs2String (VOut name:rest) = green ("[" ++ name ++ "]") ++ chs2String rest
chs2String (VStart name _:rest) = "{" ++ name ++ "=" ++ chs2String rest
chs2String (VEnd:rest) = "}" ++ chs2String rest
chs2String (VAssign name val:rest) = green ("assign{" ++ name ++ "=" ++ val ++ "}") ++ chs2String rest
chs2String (TabRight _:_) = error "There shouldn't be a tabright in chs2String"
chs2String (TabLeft:_) = error "There shouldn't be a tableft in chs2String"
chs2String (InfixTag priority name:rest) = "Op(" ++ show priority ++ "," ++ name ++ ")" ++ chs2String rest
chs2String (Bound:rest) = chs2String rest
chs2String (Sync _:rest) = chs2String rest
chs2String [] = []
chs2String x = error ("missing case in chs2String: " ++ show x)

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
    (chs2String . expandWhitespace . expandTabs [] . expandElements) es
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


expandElements::EString->EString
expandElements (EStart name attributes condition:rest) =
    ([VPush] ++ e ("<" ++ name)
        ++ concat ((\name -> e(" " ++ name ++ "='") ++ [VOut ("@" ++ name)] ++ e "'") <$> attributes)
        ++ e (">")
        ++ [Ch '\n', TabRight "  "])
    ++ expandElements rest
expandElements (EEnd name:rest) =
    ([TabLeft, Ch '\n'] ++ e("</" ++ name ++ ">") ++ [VPop]) ++ expandElements rest
expandElements (c:rest) = c:expandElements rest
expandElements [] = []

