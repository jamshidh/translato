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
    tagTheUntagged,
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

tagTheUntagged::[String]->[String]->[String]
tagTheUntagged args [] = args
tagTheUntagged [] (next:_) = error ("You need to supply the value of '" ++ next ++ "'")
tagTheUntagged (command@('-':'-':_):param:rest) baseList = command:param:tagTheUntagged rest baseList
tagTheUntagged [arg@('-':'-':command)] baseList = arg:tagTheUntagged [] baseList
tagTheUntagged (firstArg:argRest) (nextBase:baseRest) =
    ("--" ++ nextBase):firstArg:tagTheUntagged argRest baseRest

mkClause::(Name, Type)->Q Clause
mkClause (name, theType) = do
    let rest = mkName "rest"
    let orig = mkName "orig"
    let value = mkName "value"
    let changeValue =
            (RecUpdE
                    (VarE orig)
                    [(name,
                    (AppE (AppE (VarE (mkName "argReadOrError")) (VarE value)) (LitE (StringL (nameBase name))))
                )])

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
            (NormalB
                (AppE (AppE (VarE (mkName "f")) (VarE rest)) changeValue)
            )
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

arg2Opts::Name->[String]->ExpQ
arg2Opts name baseList = do
    let args = mkName "args"
    let orig = mkName "orig"
    let f = mkName "f"
    fields <- getFields name
    clauses <- sequence ((mkClause <$> fields) ++ [mkErrorClause, mkEmptyClause])
--    functionCall <- runQ [|f (tagTheUntagged args) orig|]
    return
        (
            LamE
                [VarP args,VarP orig]
                (LetE
                    [FunD f clauses]
                    (AppE
                        (AppE
                            (VarE f)
                            (AppE
                                (AppE
                                    (VarE (mkName "tagTheUntagged"))
                                    (VarE args))
                                (ListE (LitE . StringL <$> baseList))))
                        (VarE orig))))



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

