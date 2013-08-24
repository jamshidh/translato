{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
-----------------------------------------------------------------------------
--
-- Module      :  ArgOpts
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

module ArgOpts (
    arg2Opts,
    argReadOrError
) where

import Data.Functor
import Language.Haskell.TH

list::Info->Q [(Name, Type)]
list (TyConI (DataD _ _ _ [RecC _ items] _)) = return ((\(first, _, third) -> (first, third)) <$> items)
list x = error (show x)

getFields::Name->Q [(Name, Type)]
getFields optName = do
    info <- reify optName
    ret <- list info
    return ret

makeFunction::Name->Dec
makeFunction name = FunD name []

mkClause::(Name, Type)->Q Clause
mkClause (name, theType) = do
    let rest = mkName "rest"
    let orig = mkName "orig"
    let value = mkName "value"
    return
        (Clause
            [
                InfixP
                    (LitP (StringL ("--" ++ nameBase name)))
                    (mkName ":")
                    (InfixP
                        (VarP value)
                        (mkName ":")
                        (VarP rest)
                    ),
                VarP orig
            ]
            (NormalB (RecUpdE (VarE orig) [(name,
                    (AppE (AppE (VarE (mkName "argReadOrError")) (VarE value)) (LitE (StringL (nameBase name))))
                )]))
            []
        )

mkErrorClause::Q Clause
mkErrorClause = do
    nameP <- [p|('-':'-':name):_|]
    errorCode <- [|\name -> error ("Unknown argument: " ++ name)|]
    return
        (Clause
            [nameP, WildP]
            (NormalB (AppE (VarE (mkName "error")) (AppE errorCode (VarE (mkName "name")))))
            []
        )

mkEmptyClause::Q Clause
mkEmptyClause = do
    emptyP <- [p|[]|]
    orig <- newName "orig"
    return
        (Clause
            [emptyP, VarP orig]
            (NormalB (VarE orig))
            []
        )

arg2Opts::Name->ExpQ
arg2Opts name = do
    let args = mkName "args"
    let orig = mkName "orig"
    let f = mkName "f"
    fields <- getFields name
    clauses <- sequence ((mkClause <$> fields) ++ [mkErrorClause, mkEmptyClause])
    return
        (
            LamE
                [VarP args,VarP orig]
                (LetE
                    [FunD f clauses]
                    (AppE (AppE (VarE f) (VarE args)) (VarE orig))))



--------------------------

class ArgRead a where
    argReadOrError::String->String->a

instance ArgRead Int where
    argReadOrError x name =
        case reads x of
            [(i, "")] -> i
            _ -> error ("'" ++ name ++ "' must be a number")


instance ArgRead [Char] where
    argReadOrError x _ = x

instance ArgRead (Maybe String) where
    argReadOrError x name = Just (argReadOrError x name)

