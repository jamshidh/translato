-----------------------------------------------------------------------------
--
-- Module      :  LText
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

module LText (
    LText (LText),
    finish,
    initialize,
    text2LText,
    lText2String,
    LText.drop,
    LText.head,
    LText.length,
    LText.takeWhile
) where

import Data.Text as T

data LText = LText { text::T.Text, start::Int, finish::Int }

instance Show LText where
    show (LText { text=text, start=start, finish=finish }) = show (T.take (finish-start) (T.drop start text))

initialize::String->LText
initialize s = LText { text=T.pack s, start=0, finish=Prelude.length s }

text2LText::Text->LText
text2LText text = LText { text=text, start=0, finish=T.length text }

lText2Text::LText->T.Text
lText2Text LText { text=text, start=start, finish=finish } = T.take (finish-start) (T.drop start text)

lText2String::LText->String
lText2String = T.unpack . lText2Text

drop::Int->LText->LText
drop i s@(LText { text=text, start=start, finish=finish }) = s { start=start+i }

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

