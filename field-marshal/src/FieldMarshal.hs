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
    createRecord,
    module HasBlankSlate
) where

import Data.Functor
import Language.Haskell.TH

--import Debug.Trace

import HasBlankSlate

class FieldMarshal record value where
    setField::String->value->record->record
    getField::String->record->Either String value

deriveFieldMarshal::Name->Name->DecsQ
deriveFieldMarshal recordTypeName marshalTypeName = do
    fields <- getTHFields =<< reify recordTypeName
    setFieldClauses <- sequence ((makeSetFieldClause <$> fst <$> fields) ++ [defaultErrorClause "Unknown fieldname in setField" 3])
    getFieldClauses <- sequence ((makeGetFieldClause <$> fst <$> fields) ++ [defaultErrorClause "qqqq" 2])
    return (instanceDecl recordTypeName marshalTypeName setFieldClauses getFieldClauses)

-- Makes instance declaration of form:
--       instance FieldMarshal <recordName> <marshalTypeName> where
--                 setField = <clause>
instanceDecl::Name->Name->[Clause]->[Clause]->[Dec]
instanceDecl recordName marshalTypeName setFieldClauses getFieldClauses =
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
            [
                FunD (mkName "setField") setFieldClauses,
                FunD (mkName "getField") getFieldClauses
            ]
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

createRecord::(Record r, FieldMarshal r v)=>[(String, v)]->r
createRecord fields = setFields fields blankSlate

-- Makes clauses of form:
--          \"fieldName" record -> Right $ fieldName record
makeGetFieldClause::Name->Q Clause
makeGetFieldClause fieldName= do
    recordVariable <- newName "record"
    let fieldNameString = nameBase fieldName
    return
        (
            Clause
                [
                    LitP (StringL fieldNameString),
                    VarP recordVariable
                ]
                (
                    NormalB
                        (
                            AppE
                                (ConE $ 'Right)
                                (AppE
                                    (VarE $ mkName "convert")
                                    (AppE
                                        (VarE $ mkName (nameBase fieldName))
                                        (VarE recordVariable)))
                        )
                )
                []
        )

defaultErrorClause::String->Int->Q Clause
defaultErrorClause message numVars = do
    return
        (
            Clause
                (replicate numVars WildP)
                (
                    NormalB
                        (
                            AppE
                                (VarE $ mkName "error")
                                (LitE $ stringL message)
                        )
                )
                []
        )

--------------

--Template Haskell helper function

getTHFields::Info->Q [(Name, Type)]
getTHFields (TyConI (DataD _ _ _ [RecC _ items] _)) = return ((\(first, _, third) -> (first, third)) <$> items)
getTHFields x = error (show x)


