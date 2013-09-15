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
    formatMaybe,
    truncateString
) where

import Data.Char
import Data.Functor

import Colors
import Format
import qualified LString as LS
import ParseError

--import JDebug

data Associativity = LeftAssoc | RightAssoc | UseEndCap deriving (Eq, Ord, Show)

data InfixOp = InfixOp{opName::String, opPriority::Int, opAssociativity::Associativity} deriving (Eq, Ord)

data DefaultWS = WSString String | FutureWS | EmptyWS | NoDefaultWS deriving (Eq, Ord, Show)

data EChar =
    Ch Char
    | EStart String [String]
    | FilledInEStart String [(String, Maybe String)]
        --The Maybe is for error reporting, value should be a string unless something has gone wrong
    | EEnd String
    | NestedItem EString
    | FutureItem (Maybe LS.LString)
    | ItemInfo EString
    | WSItem DefaultWS
    | DelayedWS DefaultWS
    | VOut String
    | VStart String (Maybe LS.LString) --The LString is only added for error reporting, to know the location of the string
    | VEnd
    | VAssign String (Maybe String) LS.LString
        --The LString is only added for error reporting, to know the location of the string
        --The Maybe is also for error reporting, value should be a string unless something has gone wrong
    | TabRight String
    | TabLeft
    | Unknown --used when error occurs before ItemInfo is given
    | StartBlock
    | EndBlock
    | InfixTag InfixOp
    | EndCap String
    | Fail ParseError
    deriving (Eq, Ord)

instance Show EChar where
    show (Ch '\n') = "\\n"
    show (Ch c) = [c]
    show (EStart tagName attributes) = cyan ("<" ++ tagName ++ concat (map (" " ++) attributes) ++ ">")
    show (FilledInEStart tagName atts) = cyan ("<" ++ tagName ++ concat ((" " ++) <$> (\(tagName', val) -> tagName' ++ "='" ++ formatMaybe val ++ "'") <$> atts) ++ ">")
    show (FutureItem _) = cyan ("<??>")
    show (ItemInfo eString) = cyan ("{itemInfo=" ++ show eString ++ "}")
    show (DelayedWS defltWS) = cyan ("{delayedWS=" ++ show defltWS ++ "}")
    show (WSItem defltWS) = blue ("{wsItem=" ++ show defltWS ++ "}")
    show (EEnd tagName) = cyan ("</" ++ tagName ++ ">")
    show (NestedItem s) = yellow "<<<" ++ show s ++ yellow ">>>"
    show (VOut attrName) = green ("[" ++ attrName ++ "]")
    show (VStart attrName _) = green ("{" ++ attrName ++ "=")
    show (VEnd) = green "}"
    show (VAssign attrName maybeVal _) = green ("assign{" ++ attrName ++ "=" ++ formatMaybe maybeVal ++ "}")
    show (TabLeft) = magenta "<=="
    show (TabRight tabString) = magenta ("==>(" ++ tabString ++ ")")
    show Unknown = red "Unknown"
    show StartBlock = red "["
    show EndBlock = red "]"
    show (InfixTag InfixOp{opPriority=p, opName=opName'}) = cyan ("<-" ++ opName' ++ ":" ++ show p ++ "->")
    show (EndCap endCapName) = yellow ("EndCap(" ++ endCapName ++ ")")
    show (Fail err) = red ("Fail: " ++ message err)

type EString = [EChar]

e::String->EString
e (x:rest) = Ch x:(e rest)
e [] = []

formatMaybe::Maybe String->String
formatMaybe (Just x) = x
formatMaybe Nothing = "[Unknown]"

chs2String::EString->String
chs2String (Ch x:rest) = x:chs2String rest
chs2String (Fail err:rest) = red (format err) ++ chs2String rest
chs2String (VOut attrName:rest) = green ("[" ++ attrName ++ "]") ++ chs2String rest
chs2String (VStart attrName _:rest) = "{" ++ attrName ++ "=" ++ chs2String rest
chs2String (VEnd:rest) = "}" ++ chs2String rest
chs2String (EStart tagName atts:rest) = "<" ++ tagName ++ ((" "++) =<< atts) ++ ">" ++ chs2String rest
chs2String (FilledInEStart tagName atts:rest) = cyan ("<" ++ tagName ++ ((" " ++) =<< expandAttsWithVal <$> atts) ++ ">") ++ chs2String rest
chs2String (EEnd tagName:rest) = "</" ++ tagName ++ ">" ++ chs2String rest
chs2String (NestedItem s:rest) = yellow "<<<" ++ show s ++ yellow ">>>" ++ chs2String rest
chs2String (VAssign attrName maybeVal _:rest) = green ("assign{" ++ attrName ++ "=" ++ formatMaybe maybeVal ++ "}") ++ chs2String rest
chs2String (FutureItem _:rest) = blue "<??>" ++ chs2String rest
chs2String (ItemInfo eString:rest) = blue ("{itemInfo=" ++ show eString ++ "}") ++ chs2String rest
chs2String (WSItem defltWS:rest) = blue ("{wsItem=" ++ show defltWS ++ "}") ++ chs2String rest
chs2String (DelayedWS defltWS:rest) = blue ("{delayedWS=" ++ show defltWS ++ "}") ++ chs2String rest
chs2String (TabRight _:_) = error "There shouldn't be a tabright in chs2String"
chs2String (TabLeft:_) = error "There shouldn't be a tableft in chs2String"
chs2String (Unknown:rest) = red "Unknown" ++ chs2String rest
chs2String (StartBlock:rest) = red "[" ++ chs2String rest
chs2String (EndBlock:rest) = red "]" ++ chs2String rest
chs2String (InfixTag (InfixOp{opPriority=p, opName=opName'}):rest) = "Op(" ++ show p ++ "," ++ opName' ++ ")" ++ chs2String rest
chs2String (EndCap endCapName:rest) = yellow ("EndCap(" ++ endCapName ++ ")") ++ chs2String rest
chs2String [] = []
--chs2String x = error ("missing case in chs2String: " ++ show x)

ampEscape::String->String
ampEscape [] = []
ampEscape ('"':rest) = "&quot;" ++ ampEscape rest
ampEscape ('\'':rest) = "&apos;" ++ ampEscape rest
ampEscape ('<':rest) = "&lt;" ++ ampEscape rest
ampEscape ('>':rest) = "&gt;" ++ ampEscape rest
ampEscape ('&':rest) = "&amp;" ++ ampEscape rest
ampEscape (c:rest) = c:ampEscape rest

enhancedString2String::EString->String
enhancedString2String es =
    chs2String
--    show
        $ expandWhitespace
        $ expandTabs []
        $ expandElements
        $ addLineBreaks [False]
            es
--enhancedString2String = debugOutput

expandTabs::[String]->EString->EString
expandTabs tab ((TabRight tabString):rest) = e tabString ++ expandTabs (tabString:tab) rest
expandTabs (_:tabs) (TabLeft:rest) = expandTabs tabs rest
expandTabs tabs (TabLeft:_) = error ("TabLeft missed in expandTabs: " ++ show tabs)
expandTabs tab (Ch '\n':rest) = Ch '\n':(e (concat tab) ++ expandTabs tab rest)
expandTabs tab (x:rest) = x:(expandTabs tab rest)
expandTabs _ [] = []

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
addLineBreaks [] (EEnd _:_) = error "shouldn't call addLIneBreaks for EEnd with empty breakStack"
addLineBreaks breakStack (c:rest) =
    c:addLineBreaks breakStack rest
addLineBreaks _ [] = [Ch '\n']



expandElements::EString->EString
expandElements (EStart tagName atts:rest) = e ("<" ++ tagName) ++ (expandAtts atts) ++ e(">") ++ expandElements rest
expandElements (FilledInEStart name1 atts:EEnd name2:rest) | name1 == name2 =
        e ("<" ++ name1 ++ ((" "++) =<< expandAttsWithVal <$> atts) ++ "/>") ++ expandElements rest
expandElements (FilledInEStart tagName atts:rest) = e ("<" ++ tagName ++ ((" "++) =<< expandAttsWithVal <$> atts) ++ ">") ++ expandElements rest
expandElements (FutureItem _:rest) = e ("<??>") ++ expandElements rest
expandElements (ItemInfo eString:rest) = e "{itemInfo=" ++ eString ++ e ("}") ++ expandElements rest
expandElements (EEnd tagName:rest) = e("</" ++ tagName ++ ">") ++ expandElements rest
expandElements (c:rest) = c:expandElements rest
expandElements [] = []

expandAtts::[String]->EString
expandAtts atts = atts >>= (\attrName -> e (" " ++ attrName ++ "='") ++ [VOut ("@" ++ attrName)] ++ e "'")

expandAttsWithVal::(String, Maybe String)->String
expandAttsWithVal (attrName, val) = attrName ++ "='" ++ ampEscape (formatMaybe val) ++ "'"

