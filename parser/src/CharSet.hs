-----------------------------------------------------------------------------
--
-- Module      :  CharSet
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

module CharSet (
    CharSet (CharSet),
    formatCharSet,
    CharType (..),
    isIn
) where

import Data.Char hiding (Space)

import JDebug

data CharType =
    Alpha
    | Alphanum
    | CharRange Char Char
    | Digit
    | LowercaseChar
    | SingleChar Char
    | Space
    | UppercaseChar
    | WordChar deriving (Eq, Ord, Show)

data CharSet = CharSet Bool [CharType] deriving (Eq, Ord, Show)


formatCharSet (CharSet isNot sets) = "[" ++ (if isNot then "^" else "") ++ formatSets sets ++ "]"

formatSets::[CharType]->String
formatSets (Alpha:rest) = "[a-zA-Z]" ++ formatSets rest
formatSets (Alphanum:rest) = "[a-zA-Z0-9]" ++ formatSets rest
formatSets (CharRange char1 char2:rest) = char1:'-':char2:formatSets rest
formatSets (Digit:rest) = "\\d" ++ formatSets rest
formatSets (LowercaseChar:rest) = "[a-z]" ++ formatSets rest
formatSets (SingleChar '\n':rest) = "\\n" ++ formatSets rest
formatSets (SingleChar '\r':rest) = "\\r" ++ formatSets rest
formatSets (SingleChar '\t':rest) = "\\t" ++ formatSets rest
formatSets (SingleChar c:rest) = c:formatSets rest
formatSets (Space:rest) = "\\s" ++ formatSets rest
formatSets (UppercaseChar:rest) = "[A-Z]" ++ formatSets rest
formatSets (WordChar:rest) = "\\w" ++ formatSets rest
formatSets [] = []

isIn::Char->CharSet->Bool
isIn c (CharSet False (chartype:_)) | isInCharType c chartype = True
isIn c (CharSet False (_:rest)) = isIn c (CharSet False rest)
isIn c (CharSet False []) = False
isIn c (CharSet True charTypes) = not $ isIn c (CharSet False charTypes)

isInCharType::Char->CharType->Bool
isInCharType c Alpha | isAlpha c = True
isInCharType c Alphanum | isAlphaNum c = True
isInCharType c (CharRange char1 char2) | (c >= char1) && (c <= char2) = True
isInCharType c Digit | isDigit c = True
isInCharType c LowercaseChar | isLower c = True
isInCharType c Space | isSpace c = True
isInCharType c UppercaseChar | isUpper c = True
isInCharType c WordChar | isAlphaNum c || c == '_' = True
isInCharType c1 (SingleChar c2) | c1 == c2 = True
--isInCharType c1 (SingleChar c2) | jtrace ("c1 = " ++ show c1 ++ " c2 = " ++ show c2) $ c1 == c2 = True
isInCharType c _ = False


