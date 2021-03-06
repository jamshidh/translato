{-# OPTIONS_GHC -Wall #-}

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
    DefaultWS(..),
    EChar (..),
    EString,
    Associativity(..),
    InfixOp(..),
    e,
    chs2String,
    enhancedString2String,
    eAmpEscape,
    formatMaybe,
    truncateString
) where

import Control.Monad.State
import Data.Char
import Data.Functor
import Data.Map as M
import qualified Data.Text.Lazy as TL

import Colors
import Format
import qualified LString as LS
import ParseError

--import JDebug

data Associativity = LeftAssoc | RightAssoc | UseEndCap deriving (Eq, Ord, Show)

data InfixOp = InfixOp{opName::TL.Text, opPriority::Int, opAssociativity::Associativity} deriving (Eq, Ord, Show)

data DefaultWS = WSString String | FutureWS | EmptyWS | WSWithoutDefault deriving (Eq, Ord, Show)

data EChar =
    Ch Char
    | EStart TL.Text (M.Map TL.Text Bool) --The Bool signifies that the attribute is optional (ie- it may not show up in the pattern).
    | FilledInEStart TL.Text [(TL.Text, Maybe TL.Text)]
        --The Maybe is for error reporting, value should be a string unless something has gone wrong
    | EEnd TL.Text
    | NestedItem EString
    | FutureItem (Maybe LS.LString)
    | ItemInfo EString
    | WSItem DefaultWS
    | DelayedWS DefaultWS
    | VOut String
    | VStart TL.Text (Maybe LS.LString) --The LString is only added for error reporting, to know the location of the string
    | VEnd
    | VAssign TL.Text (Maybe TL.Text) LS.LString
        --The LString is only added for error reporting, to know the location of the string
        --The Maybe is also for error reporting, value should be a string unless something has gone wrong
    | ReparseStart TL.Text
    | ReparseEnd
    | TabRight String
    | TabLeft
    | Unknown --used when error occurs before ItemInfo is given
    | StartBlock
    | EndBlock
    | InfixTag InfixOp
    | EndCap TL.Text
    | Fail ParseError
    deriving (Eq, Ord, Show)

instance Format EChar where
    format (Ch '\n') = "\\n"
    format (Ch c) = [c]
    format (EStart tagName attributes) =
        cyan ("<" ++ TL.unpack tagName ++ (toList attributes >>= formatAttName) ++ ">")
        where
            formatAttName (attName, optional) = " " ++ TL.unpack attName ++ if optional then "?" else ""
    format (FilledInEStart tagName atts) = cyan ("<" ++ TL.unpack tagName ++ concat ((" " ++) <$> (\(tagName', val) -> TL.unpack tagName' ++ "='" ++ formatMaybe val ++ "'") <$> atts) ++ ">")
    format (FutureItem _) = cyan ("<??>")
    format (ItemInfo eString) = cyan ("{itemInfo=" ++ (format =<< eString) ++ "}")
    format (DelayedWS defltWS) = cyan ("{delayedWS=" ++ show defltWS ++ "}")
    format (WSItem defltWS) = blue ("{wsItem=" ++ show defltWS ++ "}")
    format (EEnd tagName) = cyan ("</" ++ TL.unpack tagName ++ ">")
    format (ReparseStart reparseName) = yellow ("[[[[" ++ TL.unpack reparseName ++ ":")
    format ReparseEnd = yellow "]]]]"
    format (NestedItem s) = yellow "<<<" ++ (format =<< s) ++ yellow ">>>"
    format (VOut attrName) = green ("[" ++ attrName ++ "]")
    format (VStart attrName _) = green ("{" ++ TL.unpack attrName ++ "=")
    format (VEnd) = green "}"
    format (VAssign attrName maybeVal _) = green ("assign{" ++ TL.unpack attrName ++ "=" ++ formatMaybe maybeVal ++ "}")
    format (TabLeft) = magenta "<=="
    format (TabRight tabString) = magenta ("==>(" ++ tabString ++ ")")
    format Unknown = red "Unknown"
    format StartBlock = red "["
    format EndBlock = red "]"
    format (InfixTag InfixOp{opPriority=p, opName=opName'}) = cyan ("<-" ++ TL.unpack opName' ++ ":" ++ show p ++ "->")
    format (EndCap endCapName) = yellow ("EndCap(" ++ TL.unpack endCapName ++ ")")
    format (Fail err) = red ("Fail: " ++ message err)

type EString = [EChar]

e::String->EString
e (x:rest) = Ch x:(e rest)
e [] = []

formatMaybe::Maybe TL.Text->String
formatMaybe (Just x) = TL.unpack x
formatMaybe Nothing = "[Unknown]"

chs2String::EString->String
chs2String (Ch x:rest) = x:chs2String rest
chs2String (Fail err:rest) = "<error>" ++ (format err >>= ampEscape) ++ "</error>" ++ chs2String rest
chs2String (VOut attrName:rest) = green ("[" ++ attrName ++ "]") ++ chs2String rest
chs2String (VStart attrName _:rest) = "{" ++ TL.unpack attrName ++ "=" ++ chs2String rest
chs2String (VEnd:rest) = "}" ++ chs2String rest
chs2String (EStart tagName atts:rest) = "<" ++ TL.unpack tagName ++ (toList atts >>= formatAttName) ++ ">" ++ chs2String rest
        where
            formatAttName (attName, optional) = " " ++ TL.unpack attName ++ if optional then "?" else ""
chs2String (FilledInEStart tagName atts:rest) = cyan ("<" ++ TL.unpack tagName ++ ((" " ++) =<< expandAttsWithVal <$> atts) ++ ">") ++ chs2String rest
chs2String (EEnd tagName:rest) = "</" ++ TL.unpack tagName ++ ">" ++ chs2String rest
chs2String (NestedItem s:rest) = yellow "<<<" ++ show s ++ yellow ">>>" ++ chs2String rest
chs2String (VAssign attrName maybeVal _:rest) = green ("assign{" ++ TL.unpack attrName ++ "=" ++ formatMaybe maybeVal ++ "}") ++ chs2String rest
chs2String (ReparseStart reparseName:rest) = yellow "[[[[" ++ TL.unpack reparseName ++ ":" ++ chs2String rest
chs2String (ReparseEnd:rest) = yellow "]]]]" ++ chs2String rest
chs2String (FutureItem _:rest) = blue "<??>" ++ chs2String rest
chs2String (ItemInfo eString:rest) = blue ("{itemInfo=" ++ show eString ++ "}") ++ chs2String rest
chs2String (WSItem defltWS:rest) = blue ("{wsItem=" ++ show defltWS ++ "}") ++ chs2String rest
chs2String (DelayedWS defltWS:rest) = blue ("{delayedWS=" ++ show defltWS ++ "}") ++ chs2String rest
chs2String (TabRight _:_) = error "There shouldn't be a tabright in chs2String"
chs2String (TabLeft:_) = error "There shouldn't be a tableft in chs2String"
chs2String (Unknown:rest) = red "Unknown" ++ chs2String rest
chs2String (StartBlock:rest) = red "[" ++ chs2String rest
chs2String (EndBlock:rest) = red "]" ++ chs2String rest
chs2String (InfixTag (InfixOp{opPriority=p, opName=opName'}):rest) = "Op(" ++ show p ++ "," ++ TL.unpack opName' ++ ")" ++ chs2String rest
chs2String (EndCap endCapName:rest) = yellow ("EndCap(" ++ TL.unpack endCapName ++ ")") ++ chs2String rest
chs2String [] = []
--chs2String x = error ("missing case in chs2String: " ++ show x)

ampEscape::Char->String
ampEscape '"' = "&quot;"
ampEscape '\'' = "&apos;"
ampEscape '<' = "&lt;"
ampEscape '>' = "&gt;"
ampEscape '&' = "&amp;"
ampEscape c = [c]

eAmpEscape::EChar->EString
eAmpEscape (Ch c) = Ch <$> ampEscape c
eAmpEscape x = [x]

enhancedString2String::EString->String
enhancedString2String =
    chs2String
        . (`evalState` [])
        .  expandTabs
        . expandWhitespace
        . expandElements
        . addLineBreaks [False]

--enhancedString2String = debugOutput

expandTabs::EString->State [String] EString
expandTabs (TabRight tabString:rest) = do
    modify (tabString:)
    expandTabs rest
expandTabs (TabLeft:rest) = do
    modify tail --error ("TabLeft missed in expandTabs: " ++ show tabs)
    expandTabs rest
expandTabs (Ch '\n':rest) = do
    tabs <- get
    remainingOutput <- expandTabs rest
    return (Ch '\n':(e (concat tabs) ++ remainingOutput))
expandTabs (x:rest) = do
    remainingOutput <- expandTabs rest
    return (x:remainingOutput)
expandTabs [] = return []

expandWhitespace::EString->EString
expandWhitespace (WSItem (WSString defltWS):rest) = e defltWS ++ expandWhitespace rest
expandWhitespace (WSItem FutureWS:rest) = expandWhitespace (WSItem futureDefltWS:rest2)
    where
        (futureDefltWS, rest2) = getFutureDefltWS rest
        getFutureDefltWS::EString->(DefaultWS, EString)
        getFutureDefltWS [] = error "End of string reached, before DelayedWS was found."
        getFutureDefltWS (DelayedWS defltWS:rest') = (defltWS, rest')
        getFutureDefltWS (c:rest') = (c:) <$> getFutureDefltWS rest'
expandWhitespace (Ch x:WSItem EmptyWS:Ch y:rest) | isAlphaNum x && isAlphaNum y = Ch x:Ch ' ':Ch y:(expandWhitespace rest)
expandWhitespace (WSItem EmptyWS:rest) = expandWhitespace rest
expandWhitespace (c:rest) = c:(expandWhitespace rest)
expandWhitespace [] = []

truncateString::Int->String->String
truncateString newLength s | length s <= newLength = s
truncateString newLength s = take newLength s ++ "...."

{-debugOutput::EString->String
debugOutput (TabLeft:rest) = "<==" ++ debugOutput rest
debugOutput (TabRight tabString:rest) = "==>(" ++ tabString ++ ")" ++ debugOutput rest
debugOutput (Ch c:rest) = c:debugOutput rest
debugOutput (c:rest) = show c ++ debugOutput rest
debugOutput [] = []-}

addLineBreaks::[Bool]->EString->EString
addLineBreaks (needsBreak:breakRest) (e1@(FilledInEStart _ _):e2@(FilledInEStart _ _):rest) = --jtrace ("1" ++ show (needsBreak:breakRest)) $
    (if needsBreak then [Ch '\n'] else [])
        ++ e1:TabRight " ":addLineBreaks (True:needsBreak:breakRest) (e2:rest)
addLineBreaks (needsBreak:breakRest) (expr@(FilledInEStart _ _):rest) =
    (if needsBreak then [Ch '\n'] else [])
        ++ expr:addLineBreaks (False:needsBreak:breakRest) rest
addLineBreaks (needsBreak:breakRest) (expr@(EEnd _):rest) = --jtrace "EEnd" $
    (if needsBreak then [TabLeft, Ch '\n'] else [])
        ++ expr:addLineBreaks breakRest rest
--TODO Figure out what should happen here
addLineBreaks [] (EEnd _:_) = e $ red "Error" -- error "shouldn't call addLIneBreaks for EEnd with empty breakStack"
addLineBreaks breakStack (c:rest) =
    c:addLineBreaks breakStack rest
addLineBreaks _ [] = [] -- You can't prettify the output by adding whitespace here!  If you do, it will appear in embedded reparsings (ie- A '\n' would be inserted in the embedded 'onclick=' javascript code).



expandElements::EString->EString
expandElements (EStart tagName atts:rest) = e ("<" ++ TL.unpack tagName) ++ (expandAtts atts) ++ e(">") ++ expandElements rest
expandElements (FilledInEStart name1 atts:EEnd name2:rest) | name1 == name2 =
        e ("<" ++ TL.unpack name1 ++ ((" "++) =<< expandAttsWithVal <$> atts) ++ "/>") ++ expandElements rest
expandElements (FilledInEStart tagName atts:rest) = e ("<" ++ TL.unpack tagName ++ ((" "++) =<< expandAttsWithVal <$> atts) ++ ">") ++ expandElements rest
expandElements (FutureItem _:rest) = e ("<??>") ++ expandElements rest
expandElements (ItemInfo eString:rest) = e "{itemInfo=" ++ eString ++ e ("}") ++ expandElements rest
expandElements (EEnd tagName:rest) = e("</" ++ TL.unpack tagName ++ ">") ++ expandElements rest
expandElements (c:rest) = c:expandElements rest
expandElements [] = []

expandAtts::Map TL.Text Bool->EString
expandAtts atts = toList atts >>= (\(attrName, _) -> e (" " ++ TL.unpack attrName ++ "='") ++ [VOut ("@" ++ TL.unpack attrName)] ++ e "'")

expandAttsWithVal::(TL.Text, Maybe TL.Text)->String
expandAttsWithVal (attrName, val) = TL.unpack attrName ++ "='" ++ (ampEscape =<< formatMaybe val) ++ "'"

