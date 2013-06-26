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
    CharType (..),
    formatAsRegex,
    isIn
) where

import Data.Char hiding (Space)

data CharType =
    Alpha
    | Alphanum
    | Digit
    | LowercaseChar
    | NoChar --Same as EOF, but 'EOF' is used elsewhere, so it is easier to use a different name here
    | SingleChar Char
    | Space
    | UppercaseChar
    | WordChar deriving (Eq, Ord)

data CharSet = CharSet Bool [CharType] deriving (Eq, Ord)

instance Show CharSet where
    show (CharSet isNot sets) = "[" ++ (if isNot then "^" else "") ++ showSets sets ++ "]"

formatAsRegex::CharSet->String
formatAsRegex (CharSet invert [SingleChar c]) = [c]
formatAsRegex (CharSet invert [WordChar]) = "\\w"
formatAsRegex (CharSet invert [NoChar]) = "<EOF>"
formatAsRegex (CharSet invert types) =
    "["
    ++ (if invert then "^" else "")
    ++ (types >>= formatCharTypeAsRegex)
    ++ "]"

formatCharTypeAsRegex::CharType->String
formatCharTypeAsRegex (SingleChar '\n') = "\\n"
formatCharTypeAsRegex (SingleChar '\r') = "\\r"
formatCharTypeAsRegex (SingleChar '\t') = "\\t"
formatCharTypeAsRegex (SingleChar c) = [c]
formatCharTypeAsRegex Alpha = "[a-zA-Z]"
formatCharTypeAsRegex Alphanum = "[a-zA-Z0-9]"
formatCharTypeAsRegex Digit = "\\d"
formatCharTypeAsRegex LowercaseChar = "[a-z]"
formatCharTypeAsRegex NoChar = error "NoChar should be taken care of in formatAsRegex"
formatCharTypeAsRegex Space = "\\s"
formatCharTypeAsRegex UppercaseChar = "[A-Z]"
formatCharTypeAsRegex WordChar = "\\w"


showSets::[CharType]->String
showSets (SingleChar '\n':rest) = "\\n" ++ showSets rest
showSets (SingleChar '\r':rest) = "\\r" ++ showSets rest
showSets (SingleChar '\t':rest) = "\\t" ++ showSets rest
showSets (SingleChar c:rest) = c:showSets rest
showSets (Alpha:rest) = "[a-zA-Z]" ++ showSets rest
showSets (Alphanum:rest) = "[a-zA-Z0-9]" ++ showSets rest
showSets (Digit:rest) = "\\d" ++ showSets rest
showSets (LowercaseChar:rest) = "[a-z]" ++ showSets rest
showSets (Space:rest) = "\\s" ++ showSets rest
showSets (UppercaseChar:rest) = "[A-Z]" ++ showSets rest
showSets (WordChar:rest) = "\\w" ++ showSets rest
showSets [] = []

isIn::Maybe Char->CharSet->Bool
isIn c (CharSet False (chartype:_)) | isInCharType c chartype = True
isIn c (CharSet False (_:rest)) = isIn c (CharSet False rest)
isIn c (CharSet False []) = False
isIn c (CharSet True charTypes) = not $ isIn c (CharSet False charTypes)

isInCharType::Maybe Char->CharType->Bool
isInCharType Nothing NoChar = True --The EOF case
isInCharType (Just c) Alpha | isAlpha c = True
isInCharType (Just c) Alphanum | isAlphaNum c = True
isInCharType (Just c) Digit | isDigit c = True
isInCharType (Just c) LowercaseChar | isLower c = True
isInCharType (Just c) Space | isSpace c = True
isInCharType (Just c) UppercaseChar | isUpper c = True
isInCharType (Just c) WordChar | isAlphaNum c || c == '_' = True
isInCharType (Just c1) (SingleChar c2) | c1 == c2 = True
isInCharType _ _ = False


