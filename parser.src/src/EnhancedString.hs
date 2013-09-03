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
    InfixAssociativity(..),
    InfixOp(..),
    e,
    chs2String,
    enhancedString2String,
    truncateString
) where

import Data.Char
import Data.Functor
import Data.List as DL

import Colors
import qualified LString as LS
import ParseError

import JDebug

data InfixAssociativity = InfixLeftAssoc | InfixRightAssoc | InfixUseEndCap deriving (Eq, Ord, Show)

data InfixOp = InfixOp{opName::String, opPriority::Int, opAssociativity::InfixAssociativity} deriving (Eq, Ord)

data EChar = Ch Char
    | EStart String [String]
    | FilledInEStart String [(String, String)]
    | EEnd String
    | NestedItem EString
    | FutureItem
    | ItemInfo EString
    | VPush
    | VPop
    | VOut String
    | VStart String (Maybe LS.LString) --The LString is only added for error reporting, to know the location of the string
    | VEnd
    | VAssign String String LS.LString --The LString is only added for error reporting, to know the location of the string
    | TabRight String
    | TabLeft
    | Unknown --used when error occurs before ItemInfo is given
    | StartBlock
    | EndBlock
    | InfixTag InfixOp
    | EndCap String
    | Fail ParseError
--    | ExpectationError [String] LS.LString
--    | Error String LS.LString
    deriving (Eq, Ord)


instance Show EChar where
    show (Ch '\n') = "\\n"
    show (Ch c) = [c]
    show (EStart name attributes) = cyan ("<" ++ name ++ concat (map (" " ++) attributes) ++ ">")
    show (FilledInEStart name atts) = cyan ("<" ++ name ++ concat ((" " ++) <$> (\(name, val) -> name ++ "='" ++ val ++ "'") <$> atts) ++ ">")
    show (FutureItem) = cyan ("<??>")
    show (ItemInfo eString) = cyan ("{itemInfo=" ++ show eString ++ "}")
    show (EEnd name) = cyan ("</" ++ name ++ ">")
    show (NestedItem s) = yellow "<<<" ++ show s ++ yellow ">>>"
    show VPush = magenta ">>VPush>>"
    show VPop = magenta "<<VPop<<"
    show (VOut name) = green ("[" ++ name ++ "]")
    show (VStart name _) = green ("{" ++ name ++ "=")
    show (VEnd) = green "}"
    show (VAssign name val s) = green ("assign{" ++ name ++ "=" ++ LS.formatLString (LS.take (length val) s) ++ "}")
    show (TabLeft) = magenta "<=="
    show (TabRight tabString) = magenta ("==>(" ++ tabString ++ ")")
    show StartBlock = red "["
    show EndBlock = red "]"
    show (InfixTag InfixOp{opPriority=p, opName=name}) = cyan ("<-" ++ name ++ ":" ++ show p ++ "->")
    show (EndCap name) = yellow ("EndCap(" ++ name ++ ")")
--    show (ExpectationError expected s) = red ("{ExpectationError: " ++ show expected ++ "}")
--    show (Error message s) = red ("{Error: " ++ message ++ "}")
    show (Fail error) = red ("Fail: " ++ message error)

type EString = [EChar]

e::String->EString
e (x:rest) = Ch x:(e rest)
e [] = []

chs2String::EString->String
chs2String (Ch x:rest) = x:chs2String rest
chs2String (Fail err:rest) = red (format err) ++ chs2String rest
chs2String (VPush:rest) = magenta ">>VPush" ++ chs2String rest
chs2String (VPop:rest) = magenta "<<VPop<<" ++ chs2String rest
chs2String (VOut name:rest) = green ("[" ++ name ++ "]") ++ chs2String rest
chs2String (VStart name _:rest) = "{" ++ name ++ "=" ++ chs2String rest
chs2String (VEnd:rest) = "}" ++ chs2String rest
chs2String (EStart name atts:rest) = "<" ++ name ++ concat ((" "++) <$> atts) ++ ">" ++ chs2String rest
chs2String (FilledInEStart name atts:rest) = cyan ("<" ++ name ++ concat ((" " ++) <$> (\(name, val) -> name ++ "='" ++ val ++ "'") <$> atts) ++ ">") ++ chs2String rest
chs2String (EEnd name:rest) = "</" ++ name ++ ">" ++ chs2String rest
chs2String (NestedItem s:rest) = yellow "<<<" ++ show s ++ yellow ">>>" ++ chs2String rest
chs2String (VAssign name val s:rest) = green ("assign{" ++ name ++ "=" ++ LS.formatLString (LS.take (length val) s) ++ "}") ++ chs2String rest
chs2String (FutureItem:rest) = blue "<??>" ++ chs2String rest
chs2String (ItemInfo eString:rest) = blue ("{itemInfo=" ++ show eString ++ "}") ++ chs2String rest
chs2String (TabRight _:_) = error "There shouldn't be a tabright in chs2String"
chs2String (TabLeft:_) = error "There shouldn't be a tableft in chs2String"
chs2String (StartBlock:rest) = red "[" ++ chs2String rest
chs2String (EndBlock:rest) = red "]" ++ chs2String rest
chs2String (InfixTag (InfixOp{opPriority=p, opName=name}):rest) = "Op(" ++ show p ++ "," ++ name ++ ")" ++ chs2String rest
chs2String (EndCap name:rest) = yellow ("EndCap(" ++ name ++ ")") ++ chs2String rest
chs2String [] = []
--chs2String x = error ("missing case in chs2String: " ++ show x)

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
debugOutput (TabRight tabString:rest) = "==>(" ++ tabString ++ ")" ++ debugOutput rest
debugOutput (Ch c:rest) = c:debugOutput rest
debugOutput (c:rest) = show c ++ debugOutput rest
debugOutput [] = []

addLineBreaks::[Bool]->EString->EString
addLineBreaks (needsBreak:breakRest) (e1@(FilledInEStart _ _):e2@(FilledInEStart _ _):rest) = --jtrace ("1" ++ show (needsBreak:breakRest)) $
    (if needsBreak then [Ch '\n'] else [])
        ++ e1:TabRight " ":addLineBreaks (True:needsBreak:breakRest) (e2:rest)
addLineBreaks (needsBreak:breakRest) (e@(FilledInEStart _ _):rest) =  --jtrace ("2" ++ show (needsBreak:breakRest)) $
    (if needsBreak then [Ch '\n'] else [])
        ++ e:addLineBreaks (False:needsBreak:breakRest) rest
addLineBreaks (needsBreak:breakRest) (e@(EEnd _):rest) = --jtrace "EEnd" $
    (if needsBreak then [TabLeft, Ch '\n'] else [])
        ++ e:addLineBreaks breakRest rest
addLineBreaks [] (e@(EEnd _):rest) = error "shouldn't call addLIneBreaks for EEnd with empty breakStack"
addLineBreaks breakStack (c:rest) = --jtrace ("Other: " ++ show c) $
    c:addLineBreaks breakStack rest
addLineBreaks _ [] = [Ch '\n']



expandElements::EString->EString
expandElements (EStart name atts:rest) = e ("<" ++ name) ++ (expandAtts atts) ++ e(">") ++ expandElements rest
expandElements (FilledInEStart name1 atts:EEnd name2:rest) | name1 == name2 =
        e ("<" ++ name1 ++ expandAttsWithVals atts ++ "/>") ++ expandElements rest
expandElements (FilledInEStart name atts:rest) = e ("<" ++ name ++ expandAttsWithVals atts ++ ">") ++ expandElements rest
expandElements (FutureItem:rest) = e ("<??>") ++ expandElements rest
expandElements (ItemInfo eString:rest) = e "{itemInfo=" ++ eString ++ e ("}") ++ expandElements rest
expandElements (EEnd name:rest) = e("</" ++ name ++ ">") ++ expandElements rest
expandElements (c:rest) = c:expandElements rest
expandElements [] = []

expandAtts::[String]->EString
expandAtts atts = atts >>= (\name -> e (" " ++ name ++ "='") ++ [VOut ("@" ++ name)] ++ e "'")

expandAttsWithVals::[(String, String)]->String
expandAttsWithVals atts = concat ((\(name, value) -> " " ++ name ++ "='" ++ value ++ "'") <$> atts)

