{-# OPTIONS_GHC -Wall #-}

module LString (
    LString (..),
    formatLString,
    createLString,
    empty,
    isPrefixOf,
    LString.null,
    LString.drop,
    LString.dropWhile,
    LString.take,
    LString.head,
    LString.tail
) where

import Data.Int
import qualified Data.List as DL
import qualified Data.Text.Lazy as TL

--import JDebug

data LString = LString { 
  previousChar::Maybe Char, 
  string::TL.Text, 
  line::Int64, 
  col::Int64 
  } deriving (Eq, Ord, Show)

createLString::TL.Text->LString
createLString s = LString { 
  previousChar=Nothing, 
  string=s, 
  line=0, 
  col=0 
  }

empty::LString
empty = LString { 
  previousChar=Nothing, 
  string=TL.empty, 
  line=0, 
  col=0 
  }

isPrefixOf::TL.Text->LString->Bool
isPrefixOf string1 (LString {string=string2}) = string1 `TL.isPrefixOf` string2

drop::Int64->LString->LString
drop i s =
    s {
        previousChar=if TL.null takeRes 
                     then previousChar s
                     else Just $ TL.last takeRes,
        string=TL.drop i (string s),
        line=line s+lineDelta,
        col= if (lineDelta > 0) then TL.length $ DL.last $ TL.lines takeRes
            else col s + TL.length takeRes
        }
    where 
      (takeRes, dropRes) = TL.splitAt i (string s)
      lineDelta = TL.length (TL.filter ('\n'==) takeRes)

dropWhile::(Char->Bool)->LString->LString
dropWhile f s =
    s {
        previousChar=if TL.null takeRes 
                     then previousChar s
                     else Just $ TL.last takeRes,
        string=TL.dropWhile f (string s),
        line=line s+lineDelta,
        col= if (lineDelta > 0) 
               then TL.length $ DL.last $ TL.lines takeRes
               else col s + TL.length takeRes
        }
    where 
      (takeRes, dropRes) = TL.span f (string s)
      lineDelta = TL.length (TL.filter ('\n'==) takeRes)

null::LString->Bool
null s = TL.null (string s)

take::Int64->LString->LString
take i s = s{string=TL.take i (string s)}

head::LString->Char
head s = TL.head (string s)

tail::LString->LString
tail LString{string=t, line=line, col=col} =
  case TL.uncons t of
    Just (first, rest) ->
      LString {
           previousChar=Just first,
           string=rest,
           line=line+lineDelta,
           col=if (lineDelta == 0) then col+1 else 0
           }
      where
              lineDelta = if (first == '\n') then 1 else 0
    Nothing -> error "LString.tail called on empty list"

formatLString s =
    "[(" ++ show (col s) ++ "," ++ show (line s) ++ ")" ++ show (string s) ++ "]"

