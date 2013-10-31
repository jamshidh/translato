-----------------------------------------------------------------------------
--
-- Module      :  StupidSetGet
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- | This is a hack to create a setter/getter in a record field.
-- Note- This module is horrible, and the only reason I use it is because it is the
-- only way that Haskell allow this without an extension like template Haskell (which
-- I would rather avoid)
--
-----------------------------------------------------------------------------


module StupidSetGet (
    setFromString,
    getFields
) where

import Data.Functor
--import Text.Regex

import JDebug

type FieldName = String

getFields::Show a=>a->[FieldName]
getFields x = (\(name, val) -> name ++ " " ++ val)
    <$> fmap show <$> fmap valueString2Type <$> entry2FieldValue <$> (splitRegex (mkRegex "\\s*,\\s*")
    (subRegex (mkRegex "}\\s*$") stringWithStrippedHeader ""))
    where
        stringWithStrippedHeader = (subRegex (mkRegex "^\\w+\\s*\\{") (show x) "")

entry2FieldValue::String->(FieldName, String)
entry2FieldValue fieldValueString = case splitRegex (mkRegex "=") fieldValueString of
    [field, value] -> (field, value)

data ParamType = ParamString | ParamInt | ParamBool deriving (Show)

valueString2Type::String->ParamType
valueString2Type (' ':rest) = valueString2Type rest
valueString2Type ('"':_) = ParamString
valueString2Type "True" = ParamBool
valueString2Type "False" = ParamBool
valueString2Type value | (mkRegex "^[[:digit:]]+\\s*$") `matchRegex` value /= Nothing = ParamInt
--valueString2Type (c:_) = error ("Unknown case in valueString2Type: " ++ show c)
valueString2Type x = error ("Unknown case in valueString2Type: " ++ show x)

--TODO make this work in cases where strings contain "field=...."
setFromString::Show a=>Read a=>FieldName->String->a->a
setFromString field value input =
    case matchRegex (mkRegex (field ++ "\\s*=")) inputString of
        Nothing -> error ("Field '" ++ field ++ "' does not exist in the records.")
        _ -> read theString
    where
        theString = (subRegex (mkRegex (field ++ "\\s*=[^,}]+")) (show input) (field ++ "=" ++ valueString))
        (Just [inputVal]) = matchRegex (mkRegex (field ++ "\\s*=([^,}]+)")) inputString
        inputString = show input
        valType = valueString2Type inputVal
        valueString = case valType of
            ParamString -> "\"" ++ value ++ "\""
            ParamInt -> case matchRegex (mkRegex "^[[:digit:]]+$") value of
                            Nothing -> error ("parameter '" ++ field ++ "' must be an int")
                            _-> value
            ParamBool -> case matchRegex (mkRegex "^(True|False)$") value of
                            Nothing -> error ("parameter '" ++ field ++ "' must be a bool")
                            _-> value



