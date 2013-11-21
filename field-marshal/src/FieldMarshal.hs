{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
--
-- Module      :  FieldMarshal
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

module FieldMarshal (
    FieldMarshal(..),
    deriveFieldMarshal,
    setFields,
    createRecord
) where

import Data.Functor
import Language.Haskell.TH

--import Debug.Trace

import HasBlankSlate

class FieldMarshal record value where
    setField::String->value->record->record

deriveFieldMarshal::Name->Name->DecsQ
deriveFieldMarshal recordTypeName marshalTypeName = do
    fields <- getTHFields =<< reify recordTypeName
    setFieldClauses <- sequence (makeSetFieldClause <$> (fst <$> fields))
    return (instanceDecl recordTypeName marshalTypeName setFieldClauses)

-- Makes instance declaration of form:
--       instance FieldMarshal <recordName> <marshalTypeName> where
--                 setField = <clause>
instanceDecl::Name->Name->[Clause]->[Dec]
instanceDecl recordName marshalTypeName setFieldClauses =
    [
        InstanceD
            []
            (
                AppT
                    (
                        AppT
                            (ConT ''FieldMarshal)
                            (ConT recordName)
                    )
                    (ConT marshalTypeName)
            )
            [FunD (mkName "setField") setFieldClauses]
    ]

-- Makes clauses of form:
--          \"fieldName" val record -> record{fieldName=convert val}
makeSetFieldClause::Name->Q Clause
makeSetFieldClause fieldName= do
    recordVariable <- newName "record"
    valVariable <- newName "val"
    let fieldNameString = nameBase fieldName
    return
        (
            Clause
                [
                    LitP (StringL fieldNameString),
                    VarP valVariable,
                    VarP recordVariable
                ]
                (
                    NormalB
                        (
                            RecUpdE
                                (VarE recordVariable)
                                [(fieldName,AppE (VarE $ mkName "convert") (VarE valVariable))]
                        )
                )
                []
        )

setFields::FieldMarshal r val=>[(String, val)]->r->r
setFields [] record = record
setFields ((name, val):rest) record = setFields rest (setField name val record)

createRecord::(HasBlankSlate r, FieldMarshal r v)=>[(String, v)]->r
createRecord fields = setFields fields blankSlate

--------------

--Template Haskell helper function

getTHFields::Info->Q [(Name, Type)]
getTHFields (TyConI (DataD _ _ _ [RecC _ items] _)) = return ((\(first, _, third) -> (first, third)) <$> items)
getTHFields x = error (show x)


