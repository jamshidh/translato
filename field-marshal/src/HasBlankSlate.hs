{-# LANGUAGE TemplateHaskell #-}

module HasBlankSlate (
    Record(..),
    deriveRecord
) where

import Data.Functor
import Language.Haskell.TH

class Record a where
    blankSlate::a
    fieldInfo::a->[(String, String)]

deriveRecord::Name->DecsQ
deriveRecord recordTypeName = do
    fields <- getFields =<< reify recordTypeName
    emptyRecordClauses <- makeEmptyRecordClause recordTypeName
    namesAndTypes <- getRecordFields <$> reify recordTypeName
    return (instanceDecl recordTypeName [emptyRecordClauses] (fmap show <$> namesAndTypes))

instanceDecl::Name->[Clause]->[(String, String)]->[Dec]
instanceDecl recordName emptyRecordClauses nameAndTypes =
    [
        InstanceD
            []
            (AppT (ConT ''Record) (ConT recordName))
            [
                FunD (mkName "blankSlate") emptyRecordClauses,
                FunD (mkName "fieldInfo")
                    [
                        Clause
                            [WildP]
                            (
                                NormalB
                                    (
                                        ListE
                                            (
                                                (\(name, theType) -> TupE [LitE $ stringL name, LitE $ stringL theType]) <$> nameAndTypes
                                            )
                                    )
                            )
                            []

                    ]
            ]
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

--------------------------


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
