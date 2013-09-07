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
import Data.Function
import Data.Functor
import Data.List hiding (lookup)
import Data.Map hiding (filter, map)
import Data.Tree

import CharSet
import Context
import EnhancedString
import Grammar
import LeftFactoring
import qualified LString as LS
import LString (LString)
import ParseError
import TreeTools

import JDebug

data TreeInfo = TreeInfo { tree::Tree Expression, firstMatchers::[Expression], allowsWhiteSpace::Bool, isFallBack::Bool } deriving (Eq, Show)

getTreeInfos::Tree Expression->[TreeInfo]
getTreeInfos t@
    Node{rootLabel=TextMatch text1,
        subForest=[Node{rootLabel=WhiteSpace _,
            subForest=[Node{rootLabel=TextMatch text2}]}]} =
    [TreeInfo {
        tree=t,
        firstMatchers=[TextMatch (text1++" "++text2)],
        allowsWhiteSpace=False, isFallBack=False
    }]
getTreeInfos t@Node{rootLabel=TextMatch text} =
    [TreeInfo {
        tree=t,
        firstMatchers=[TextMatch text],
        allowsWhiteSpace=False, isFallBack=False
    }]
getTreeInfos t@Node{rootLabel=Character charset} =
    [TreeInfo {
        tree=t,
        firstMatchers=[Character charset],
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
getTreeInfos tree =
    error ("Missing case in getTreeInfos: " ++ safeDrawETree tree)

isPrefixTextMatch::String->String->Bool
isPrefixTextMatch (' ':rest1) (' ':rest2) = isPrefixTextMatch (' ':rest1) rest2
isPrefixTextMatch (' ':rest1) (c2:rest2) = isPrefixTextMatch rest1 (c2:rest2)
isPrefixTextMatch (c1:rest1) (c2:rest2) | c1 == c2 = isPrefixTextMatch rest1 rest2
isPrefixTextMatch "" "" = True
isPrefixTextMatch _ "" = False
isPrefixTextMatch (c1:rest1) (c2:rest2) = False
isPrefixTextMatch [] (c2:rest2) = True


eCheck::LString->Expression->Bool
eCheck s (TextMatch text) = text `isPrefixTextMatch` LS.string s
eCheck s (Character charset) | LS.null s = False
eCheck s (Character charset) = LS.head s `isIn` charset
eCheck s EOF = LS.null s
eCheck _ e =
    error ("Missing case in function 'eCheck': " ++ formatExpression e ++ ", ")

check::LString->TreeInfo->Bool
check s treeInfo@TreeInfo{allowsWhiteSpace=True} | LS.null s = check s treeInfo{allowsWhiteSpace=False}
check s treeInfo@TreeInfo{allowsWhiteSpace=True} | isSpace (LS.head s) = check (LS.tail s) treeInfo
check s treeInfo@TreeInfo{firstMatchers=e, allowsWhiteSpace=True} = check s treeInfo{allowsWhiteSpace=False}
check s TreeInfo{firstMatchers=exps} = or (eCheck s <$> exps)
--check s (FallBack, _, _) = False

funcAnd::(a->Bool)->(a->Bool)->a->Bool
funcAnd f1 f2 = \x -> f1 x && f2 x

chooseOne::Forest Expression->LString->Either ParseError (Tree Expression)
chooseOne [tree] s = Right tree
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
        items -> jtrace ("multiple TextMatches matched in chooseOne:"
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
addTextMatchSize s treeInfo@TreeInfo{allowsWhiteSpace=True} | LS.null s = addTextMatchSize s treeInfo{allowsWhiteSpace=False}
addTextMatchSize s treeInfo@TreeInfo{allowsWhiteSpace=True} | isSpace (LS.head s) = addTextMatchSize (LS.tail s) treeInfo
addTextMatchSize s treeInfo = (maximum (textMatchMatchSize s <$> (firstMatchers treeInfo)), treeInfo)

removeTextMatchSize::(Int, a)->a
removeTextMatchSize = snd


textMatchMatchSize::LS.LString->Expression->Int
textMatchMatchSize s (TextMatch text) | text `isPrefixTextMatch` LS.string s = length text
textMatchMatchSize _ _ = 0


maximumsUsing::Ord b=>(a->b)->[a]->[a]
maximumsUsing f list = filter (\x -> f x == max) list
    where max = maximum (f <$> list)
