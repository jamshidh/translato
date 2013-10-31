-----------------------------------------------------------------------------
--
-- Module      :  LString
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

module LString (
    LString (..),
    formatLString,
    createLString,
    empty,
    isPrefixOf,
    LString.null,
    LString.drop,
    LString.take,
    LString.head,
    LString.tail
) where

import qualified Data.List as DL

import JDebug

data LString = LString { string::String, line::Int, col::Int } deriving (Eq, Ord, Show)

createLString::String->LString
createLString s = LString { string=s, line=0, col=0 }

empty::LString
empty = LString { string="", line=0, col=0 }

isPrefixOf::String->LString->Bool
isPrefixOf string1 (LString {string=string2}) = DL.isPrefixOf string1 string2

drop::Int->LString->LString
drop i s =
    s {
        string=DL.drop i (string s),
        line=line s+lineDelta,
        col= if (lineDelta > 0) then length $ last $ lines takeRes
            else col s + length takeRes
        }
    where (takeRes, dropRes) = DL.splitAt i (string s); lineDelta = length (filter ('\n'==) takeRes)

null::LString->Bool
null s = DL.null (string s)

take::Int->LString->LString
take i s = s{string=DL.take i (string s)}

head::LString->Char
head s = DL.head (string s)

tail::LString->LString
tail LString{string=(first:rest), line=line, col=col} =
    LString {
        string=rest,
        line=line+lineDelta,
        col=if (lineDelta == 0) then col+1 else 0
        }
    where lineDelta = if (first == '\n') then 1 else 0

formatLString s =
    "[(" ++ show (col s) ++ "," ++ show (line s) ++ ")" ++ show (string s) ++ "]"

{--instance Show LText where
    show (LText { text=text, start=start, finish=finish }) = show (T.take (finish-start) (T.drop start text))

initialize::String->LText
initialize s = LText { text=T.pack s, start=0, finish=Prelude.length s }

text2LText::Text->LText
text2LText text = LText { text=text, start=0, finish=T.length text }

lText2Text::LText->T.Text
lText2Text LText { text=text, start=start, finish=finish } = T.take (finish-start) (T.drop start text)

lText2String::LText->String
lText2String = T.unpack . lText2Text

takeWhile::(Char->Bool)->LText->LText
takeWhile f s@(LText { text=text, start=start, finish=finish }) = s { finish=end }
    where end = case T.findIndex (not . f) (T.drop start text) of
            Just i -> start + i
            Nothing -> finish

head::LText->Char
head s@(LText { text=text, start=start, finish=finish }) = T.index text start

length::LText->Int
length (LText { text=text, start=start, finish=finish }) = finish - start

instance Eq LText where
    x == y = start x == start y

instance Ord LText where
    x <= y = start x <= start y
--}


