-----------------------------------------------------------------------------
--
-- Module      :  List2Record
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

module List2Record (
    list2Record,
    getByName
) where

import Data.Functor
import Language.Haskell.TH

getFields::Info->Q [(Name, Type)]
getFields (TyConI (DataD _ _ _ [RecC name items] _)) = return ((\(first, _, third) -> (first, third)) <$> items)
getFields x = error (show x)

getConstructorName (TyConI (DataD _ _ _ [RecC name _] _)) = return name


getByName name list converter =
    case lookup name list of
        Just w -> converter w
        Nothing -> error ("missing: " ++ show name)




list2Record::Name->Name->DecsQ
list2Record name converter = do
    fields <- getFields =<< reify name
    constructorName <- getConstructorName =<< reify name
    let functionName = mkName ("list2" ++ nameBase name)
    let assignments = (\x -> (x,
            AppE (AppE (AppE (VarE (mkName "getByName")) (LitE (StringL (nameBase x)))) (VarE $ mkName "list")) (VarE converter))) <$> fst <$> fields
    return [FunD functionName [Clause [VarP (mkName "list")] (NormalB (RecConE constructorName assignments)) []]]



--    [FunD qqqq_1627396311 [Clause [VarP x_1627396312] (NormalB (LitE (IntegerL 1))) []]]
