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
    CharType (
        Alpha,
        Alphanum,
        Digit,
        LowercaseChar,
        SingleChar,
        Space,
        UppercaseChar,
        WordChar
    ),
    isIn
) where

import Data.Char hiding (Space)

data CharType =
    Alpha
    | Alphanum
    | Digit
    | LowercaseChar
    | SingleChar Char
    | Space
    | UppercaseChar
    | WordChar deriving (Eq, Show)

data CharSet = CharSet Bool [CharType] deriving (Eq)

instance Show CharSet where
    show (CharSet isNot sets) = "[" ++ (if isNot then "^" else "") ++ showSets sets ++ "]"

showSets::[CharType]->String
showSets (SingleChar c:rest) = c:showSets rest
showSets (Alpha:rest) = "[a-zA-Z]" ++ showSets rest
showSets (Alphanum:rest) = "[a-zA-Z0-9]" ++ showSets rest
showSets (Digit:rest) = "\\d" ++ showSets rest
showSets (LowercaseChar:rest) = "[a-z]" ++ showSets rest
showSets (Space:rest) = "\\s" ++ showSets rest
showSets (UppercaseChar:rest) = "[A-Z]" ++ showSets rest
showSets (WordChar:rest) = "\\w" ++ showSets rest
showSets [] = []

isIn::Char->CharSet->Bool
isIn c (CharSet False (chartype:_)) | isInCharType c chartype = True
isIn c (CharSet False (_:rest)) = isIn c (CharSet False rest)
isIn c (CharSet False []) = False
isIn c (CharSet True charTypes) = not $ isIn c (CharSet False charTypes)

isInCharType::Char->CharType->Bool
isInCharType c Alpha | isAlpha c = True
isInCharType c Alphanum | isAlphaNum c = True
isInCharType c Digit | isDigit c = True
isInCharType c LowercaseChar | isLower c = True
isInCharType c Space | isSpace c = True
isInCharType c UppercaseChar | isUpper c = True
isInCharType c WordChar | isAlphaNum c || c == '_' = True
isInCharType c1 (SingleChar c2) | c1 == c2 = True
isInCharType c _ = False


