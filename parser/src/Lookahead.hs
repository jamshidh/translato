{-# OPTIONS_GHC -Wall #-}
-----------------------------------------------------------------------------
--
-- Module      :  Lookahead
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

module Lookahead (
    chooseOne
) where

import Prelude hiding (lookup)

import Data.Char
import Data.Functor
import Data.Tree

import CharSet
import Grammar
import qualified LString as LS
import LString (LString)
import ParseError

import JDebug

data TreeInfo = TreeInfo { tree::Tree Expression, firstMatchers::[Expression], allowsWhiteSpace::Bool, isFallBack::Bool } deriving (Eq, Show)

getTreeInfos::Tree Expression->[TreeInfo]
getTreeInfos t@
    Node{rootLabel=TextMatch text1 n,
        subForest=[Node{rootLabel=WhiteSpace _,
            subForest=[Node{rootLabel=TextMatch text2 _}]}]} =
    [TreeInfo {
        tree=t,
        firstMatchers=[TextMatch (text1++" "++text2) n],
        allowsWhiteSpace=False, isFallBack=False
    }]
getTreeInfos t@Node{rootLabel=TextMatch text n} =
    [TreeInfo {
        tree=t,
        firstMatchers=[TextMatch text n],
        allowsWhiteSpace=False, isFallBack=False
    }]
getTreeInfos t@Node{rootLabel=Character charset n} =
    [TreeInfo {
        tree=t,
        firstMatchers=[Character charset n],
        allowsWhiteSpace=False, isFallBack=False
    }]
getTreeInfos t@Node{rootLabel=EOF} =
    [TreeInfo {
        tree=t,
        firstMatchers=[EOF],
        allowsWhiteSpace=False, isFallBack=False
    }]
getTreeInfos t@Node{rootLabel=FallBack, subForest=rest} =
    (\treeInfo->treeInfo{ tree=t, isFallBack=True }) <$> (rest >>= getTreeInfos)
getTreeInfos t@Node{rootLabel=Out _, subForest=rest} =
    (\treeInfo->treeInfo{ tree=t }) <$> (rest >>= getTreeInfos)
getTreeInfos t@Node{rootLabel=WhiteSpace _, subForest=rest} =
    (\treeInfo->treeInfo{ tree=t, allowsWhiteSpace=True }) <$> (rest >>= getTreeInfos)
getTreeInfos theTree =
    error ("Missing case in getTreeInfos: " ++ safeDrawETree theTree)

isPrefixTextMatch::String->String->Bool
isPrefixTextMatch (' ':rest1) (c:rest2) | isSpace c = isPrefixTextMatch (' ':rest1) rest2
isPrefixTextMatch (' ':rest1) (c2:rest2) = isPrefixTextMatch rest1 (c2:rest2)
isPrefixTextMatch (c1:rest1) (c2:rest2) | c1 == c2 = isPrefixTextMatch rest1 rest2
isPrefixTextMatch "" "" = True
isPrefixTextMatch _ "" = False
isPrefixTextMatch (_:_) (_:_) = False
isPrefixTextMatch [] (_:_) = True


eCheck::LString->Expression->Bool
eCheck s (TextMatch text _) = text `isPrefixTextMatch` LS.string s
eCheck s (Character _ _) | LS.null s = False
eCheck s (Character charset _) = LS.head s `isIn` charset
eCheck s EOF = LS.null s
eCheck _ e =
    error ("Missing case in function 'eCheck': " ++ formatExpression e ++ ", ")

check::LString->TreeInfo->Bool
check s treeInfo@TreeInfo{allowsWhiteSpace=True} | LS.null s =
    check s treeInfo{allowsWhiteSpace=False}
check s treeInfo@TreeInfo{allowsWhiteSpace=True} | isSpace (LS.head s) =
    check (LS.tail s) treeInfo
check s treeInfo@TreeInfo{allowsWhiteSpace=True} =
    check s treeInfo{allowsWhiteSpace=False}
check s TreeInfo{firstMatchers=exps} = or (eCheck s <$> exps)
--check s (FallBack, _, _) = False

chooseOne::Forest Expression->LString->Either ParseError (Tree Expression)
chooseOne [theTree] _ = Right theTree
chooseOne trees s = --jtrace ("---------------------\nChoice: " ++ show (length trees)) $
    --jtrace ("chooseOne: " ++ safeDrawEForest trees) $
    --jtrace ("string: " ++ show s) $
    --jtrace (show $ firstMatchers <$> snd <$> addTextMatchSize s <$> treeInfos) $
    --jtrace (show $ isFallBack <$> snd <$> addTextMatchSize s <$> treeInfos) $
    --jtrace (show $ fst <$> addTextMatchSize s <$> treeInfos) $
    case (maximumsBy fst ((filter ((/= 0) . fst)) (addTextMatchSize s <$> treeInfos))) of
        [] -> case (check s) `filter` ((not . isFallBack) `filter` treeInfos) of
                [] -> case filter ((== FallBack) . rootLabel) trees of
                    [] -> Left ExpectationError{
                            expected=treeInfos >>= (\TreeInfo { allowsWhiteSpace=w, firstMatchers=exps } -> formatItem w <$> exps),
                            ranges=[singleCharacterRangeAt s]}
                            where
                                formatItem::Bool->Expression->String
                                formatItem w e = (if w then "_ " else "") ++ formatExpression e
                    [item] -> Right item
                    _ -> error "Multiple fallback cases encountered"
                [item] -> Right (tree item)
                items ->  jtrace ("multiple things matched in chooseOne:"
                                ++ (safeDrawEForest $ tree <$> items)
                                ++ "\ns = " ++ LS.string s)
                                $ Left AmbiguityError{ ranges=[singleCharacterRangeAt s] }
        [(_, item)] -> Right (tree item)
        items -> case (check s) `filter` ((not . isFallBack) `filter` (snd <$> items)) of
                    [item] -> Right (tree item)
                    _ -> jtrace ("multiple TextMatches matched in chooseOne:"
                            ++ safeDrawEForest ((tree . removeTextMatchSize) <$> items)
                            ++ "\ns = " ++ LS.string s)
                            $ Left AmbiguityError{ ranges=[singleCharacterRangeAt s] }

        where
--                theMaxTextMatchSize = trees
                treeInfos::[TreeInfo]
                treeInfos = trees >>= getTreeInfos

maximumsBy::(Eq a)=>(Ord b)=>(a->b)->[a]->[a]
maximumsBy _ [] = []
maximumsBy f list = filter ((== theMaximum) . f) list
        where theMaximum = maximum (f <$> list)

addTextMatchSize::LS.LString->TreeInfo->(Int, TreeInfo)
addTextMatchSize s treeInfo@TreeInfo{allowsWhiteSpace=True} | LS.null s =
    addTextMatchSize s treeInfo{allowsWhiteSpace=False}
addTextMatchSize s treeInfo@TreeInfo{allowsWhiteSpace=True} | isSpace (LS.head s) =
    addTextMatchSize (LS.tail s) treeInfo
addTextMatchSize s treeInfo =
    (maximum (textMatchMatchSize s <$> (firstMatchers treeInfo)), treeInfo)

removeTextMatchSize::(Int, a)->a
removeTextMatchSize = snd


textMatchMatchSize::LS.LString->Expression->Int
textMatchMatchSize s (TextMatch text _) | text `isPrefixTextMatch` LS.string s = length text
textMatchMatchSize _ _ = 0

