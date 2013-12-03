-----------------------------------------------------------------------------
--
-- Module      :  RecordTypes
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

module RecordTypes (
    recordTypes
) where

import Data.Functor
import Language.Haskell.TH

recordTypes :: Name -> Q Exp
recordTypes recordType = do
    rfs <- getRecordFields <$> reify recordType
    return (ListE (nameAndType <$> rfs))
        where
            nameAndType (name, theType) = TupE [LitE (stringL name), LitE (stringL $ show theType)]

getRecordFields :: Info -> [(String, Type)]
getRecordFields (TyConI (DataD _ _ _ [con] _)) = getRF' con
getRecordFields (TyConI (DataD _ _ _ _ _)) = error "recordTypes only supports data types with a single constructor"
getRecordFields _ = error "recordTypes only supports record data types"

getRF' :: Con -> [(String, Type)]
getRF' (RecC name fields) = getFieldInfo <$> fields

getFieldInfo :: (Name, Strict, Type) -> (String, Type)
getFieldInfo (name, _, theType) = (nameBase name, theType)
