{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
--
-- Module      :  HasBlankSlate
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

module HasBlankSlate (
    HasBlankSlate(..),
    deriveHasBlankSlate
) where

import Data.Functor
import Language.Haskell.TH

class HasBlankSlate a where
    blankSlate::a

deriveHasBlankSlate::Name->DecsQ
deriveHasBlankSlate recordTypeName = do
    fields <- getFields =<< reify recordTypeName
    emptyRecordClauses <- makeEmptyRecordClause recordTypeName
    return (instanceDecl recordTypeName [emptyRecordClauses])

instanceDecl::Name->[Clause]->[Dec]
instanceDecl recordName emptyRecordClauses=
    [
        InstanceD
            []
            (AppT (ConT ''HasBlankSlate) (ConT recordName))
            [FunD (mkName "blankSlate") emptyRecordClauses]
    ]

makeEmptyRecordClause::Name->Q Clause
makeEmptyRecordClause recordTypeName = do
    fields <- getFields =<< reify recordTypeName
    constructorName <- getConstructorName =<< reify recordTypeName
    return
        (
            Clause
                []
                (
                    NormalB
                        (
                            RecConE
                                constructorName
                                ((\field -> (fst field,VarE $ mkName "undefined")) <$> fields)
                        )
                )
                []
        )


getFields::Info->Q [(Name, Type)]
getFields (TyConI (DataD _ _ _ [RecC _ items] _)) = return ((\(first, _, third) -> (first, third)) <$> items)
getFields x = error (show x)

getConstructorName::Info->Q Name
getConstructorName (TyConI (DataD _ _ _ [RecC name _] _)) = return name
getConstructorName x = error (show x)
