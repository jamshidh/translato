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
import Data.Foldable hiding (or, concat, maximum)
import Data.Functor
import Data.List
import Data.Tree

import CharSet
import EnhancedString
import Grammar
import qualified LString as LS
import LString (LString)
import ParseError

import JDebug

data TreeInfo = 
  TreeInfo { 
    tree::Tree Expression, 
    tagList::[String],
    firstMatchers::[Expression], 
    allowsWhiteSpace::Bool, 
    branchPriority::Importance
  } deriving (Eq, Show)

getTreeInfos::Tree Expression->[TreeInfo]
getTreeInfos t@
    Node{rootLabel=TextMatch text1 n,
        subForest=[Node{rootLabel=WhiteSpace _,
            subForest=[Node{rootLabel=TextMatch text2 _}]}]} =
    [TreeInfo {
        tree=t,
        tagList=[],
        firstMatchers=[TextMatch (text1++" "++text2) n],
        allowsWhiteSpace=False, branchPriority=Medium
    }]
getTreeInfos t@Node{rootLabel=TextMatch text n} =
    [TreeInfo {
        tree=t,
        tagList=[],
        firstMatchers=[TextMatch text n],
        allowsWhiteSpace=False, branchPriority=Medium
    }]
getTreeInfos t@Node{rootLabel=Character charset n} =
    [TreeInfo {
        tree=t,
        tagList=[],
        firstMatchers=[Character charset n],
        allowsWhiteSpace=False, branchPriority=Medium
    }]
getTreeInfos t@Node{rootLabel=EOF} =
    [TreeInfo {
        tree=t,
        tagList=[],
        firstMatchers=[EOF],
        allowsWhiteSpace=False, branchPriority=Medium
    }]
getTreeInfos t@Node{rootLabel=Priority x, subForest=rest} =
    (\treeInfo->treeInfo{ tree=t, branchPriority=x }) <$> (rest >>= getTreeInfos)
getTreeInfos t@Node{rootLabel=Out value, subForest=rest} =
    (\treeInfo->treeInfo{ tree=t, tagList=[tagName|EStart tagName _<-value] ++ tagList treeInfo }) <$> (rest >>= getTreeInfos)
getTreeInfos t@Node{rootLabel=WhiteSpace _, subForest=rest} =
    (\treeInfo->treeInfo{ tree=t, allowsWhiteSpace=True }) <$> (rest >>= getTreeInfos)
getTreeInfos theTree =
    error ("Missing case in getTreeInfos: " ++ safeDrawETree theTree)



eCheck::LString->Expression->Bool
eCheck s (TextMatch text _) = text `isPrefixTextMatch` LS.string s
eCheck s (Character _ _) | LS.null s = False
eCheck s (Character charset _) = LS.head s `isIn` charset
eCheck s EOF = LS.null s
eCheck b_ expr =
    error ("Missing case in function 'eCheck': " ++ formatExpression expr ++ ", ")

check::LString->TreeInfo->Bool
check s treeInfo@TreeInfo{allowsWhiteSpace=True} | LS.null s =
    check s treeInfo{allowsWhiteSpace=False}
check s treeInfo@TreeInfo{allowsWhiteSpace=True} | isSpace (LS.head s) = check (LS.tail s) treeInfo
check s treeInfo@TreeInfo{allowsWhiteSpace=True} =
    check s treeInfo{allowsWhiteSpace=False}
check s TreeInfo{firstMatchers=exps} = or (eCheck s <$> exps)
--check s (FallBack, _, _) = False

checkTree::LString->Tree Expression->Bool
checkTree s theTree = or $ check s <$> getTreeInfos theTree

data MatchType = TextMatched Int | RegularMatch deriving (Eq, Show)

instance Ord MatchType where
  TextMatched x <= TextMatched y = x <= y
  TextMatched x <= RegularMatch = False
  RegularMatch <= TextMatched _ = True
  RegularMatch <= RegularMatch = True

-------------------------------------
  
--These next two helper function let us cover a special case....
--Two TextMatches separated by a single whitespace should be treated (as far as priority)
--as a single TextMatch.  If the input is "ab cd", it should match "ab<whitespace>cd" over "ab".

--This normalizes TextMatch text....
--WhiteSpace is turned into a space (literally).
--Of course the algorithm to match to the input must
--change also....  See the next function.
getFullTextMatch::Tree Expression->String
getFullTextMatch 
    Node{rootLabel=TextMatch text1 _,
        subForest=[Node{rootLabel=WhiteSpace _,
            subForest=[Node{rootLabel=TextMatch text2 _}]}]} =
        text1++" "++text2
getFullTextMatch     Node{rootLabel=TextMatch text1 _} = text1
getFullTextMatch _ = error "getFullTextMatch should only be called with rootLabel=TextMatch"

--This function matches input to normalized TextMatch text.
--It works almost like "isPrefixOf", except one space in the normalized TextMatch text
--can correspond to 0 or more spaces in the input.
isPrefixTextMatch::String->String->Bool
isPrefixTextMatch (' ':rest1) (c:rest2) | isSpace c = isPrefixTextMatch (' ':rest1) rest2
isPrefixTextMatch (' ':rest1) (c2:rest2) = isPrefixTextMatch rest1 (c2:rest2)
isPrefixTextMatch (c1:rest1) (c2:rest2) | c1 == c2 = isPrefixTextMatch rest1 rest2
isPrefixTextMatch "" "" = True
isPrefixTextMatch _ "" = False
isPrefixTextMatch (_:_) (_:_) = False
isPrefixTextMatch [] (_:_) = True

------------------------------------


matchOne::LString->Tree Expression->Either ParseError (MatchType, Importance)
matchOne s n@Node{rootLabel=WhiteSpace _, subForest=rest} | LS.null s = snd <$> chooseOne s rest
matchOne s n@Node{rootLabel=WhiteSpace _, subForest=rest} | isSpace $ LS.head s = matchOne (LS.tail s) n
matchOne s Node{rootLabel=WhiteSpace _, subForest=rest} = snd <$> chooseOne s rest
matchOne s node@Node{rootLabel=TextMatch text n} | getFullTextMatch node `isPrefixTextMatch` LS.string s = Right (TextMatched (length $ getFullTextMatch node), Medium)
matchOne s Node{rootLabel=TextMatch text n} = Left $ ExpectationError [singleCharacterRangeAt s] [show text] s
matchOne s Node{rootLabel=Character charset n} | LS.null s = Left $ ExpectationError [singleCharacterRangeAt s] [formatCharSet charset] s
matchOne s Node{rootLabel=Character charset n} | LS.head s `isIn` charset = Right (RegularMatch, Medium)
matchOne s Node{rootLabel=Character charset n} = Left $ ExpectationError [singleCharacterRangeAt s] [formatCharSet charset] s
matchOne s Node{rootLabel=EOF} | LS.null s = Right (RegularMatch, Medium)
matchOne s Node{rootLabel=EOF} = Left $ ExpectationError [singleCharacterRangeAt s] ["EOF"] s
matchOne s Node{rootLabel=Priority p, subForest=rest} = (const p <$>) <$> snd <$> chooseOne s rest
matchOne s Node{rootLabel=Out value, subForest=rest} = snd <$> chooseOne s rest
matchOne _ theTree =
    error ("Missing case in matchOne: " ++ safeDrawETree theTree)

chooseOne::LString->Forest Expression->Either ParseError (Tree Expression, (MatchType, Importance))
--Perhaps this should be put in as a performance boost, but it isn't *needed*.
--chooseOne s [t] = matchOne s t
chooseOne s forest = 
  case maximumsBy snd [(t, x)|(t, Right x) <- matchResults] of
    [] -> Left $ fold [err|(_, Left err) <- matchResults]
    [x] -> Right x
    _ -> Left $ AmbiguityError [singleCharacterRangeAt s]
  where
    matchResults::[(Tree Expression, Either ParseError (MatchType, Importance))]
    matchResults = (\t -> (t, matchOne s t)) <$> forest



{-


chooseOne::Forest Expression->LString->Either ParseError (Tree Expression)
chooseOne [theTree] _ = Right theTree
chooseOne trees s = --jtrace ("---------------------\nChoice: " ++ show (length trees)) $
    jtrace ("chooseOne: " ++ safeDrawEForest trees) $
    jtrace ("string: " ++ show s) $
    --jtrace (show $ firstMatchers <$> snd <$> addTextMatchSize s <$> treeInfos) $
    --jtrace (show $ isFallBack <$> snd <$> addTextMatchSize s <$> treeInfos) $
    --jtrace (show $ fst <$> addTextMatchSize s <$> treeInfos) $
    case (maximumsBy fst ((filter ((/= 0) . fst)) (addTextMatchSize s <$> treeInfos))) of
        [] -> case (check s) `filter` (((== Medium) . branchPriority) `filter` treeInfos) of
                [] -> case checkTree s `filter` (((== Priority Low) . rootLabel) `filter` trees) of
                    [] -> Left ExpectationError{
                            expected=treeInfos >>= (\TreeInfo { allowsWhiteSpace=w, firstMatchers=exps } -> formatItem w <$> exps),
                            ranges=[singleCharacterRangeAt s]}
                            where
                                formatItem::Bool->Expression->String
                                formatItem w expr = (if w then "_ " else "") ++ formatExpression expr
                    [item] -> Right item
                    _ -> error "Multiple priority = Low cases encountered"
                [item] -> Right (tree item)
                items ->  jtrace ("===================\n\nmultiple things matched in chooseOne:\n\n      "
                                  -- ++ (safeDrawEForest $ tree <$> items) ++ "\n"
                                  ++ intercalate "\n        or\n      " (treeItem2HumanReadableSummary <$> items) ++ "\n\n"
                                  ++ "-------------\n\n"
                                  ++ "input = " ++ shortShowString (LS.string s) ++ "\n\n"
                                  ++ "===================\n")
                                $ Left AmbiguityError{ ranges=[singleCharacterRangeAt s] }
        [(_, item)] -> Right (tree item)
        items -> jtrace ("Text match: " ++ show (length items)) $ 
                 case (check s) `filter` (((== Medium) . branchPriority) `filter` (snd <$> items)) of
                    [item] -> Right (tree item)
                    _ -> jtrace ("==================\n\nmultiple TextMatches matched in chooseOne:\n\n      "
                            -- ++ safeDrawEForest ((tree . removeTextMatchSize) <$> items) ++ "\n"
                            ++ intercalate "\n        or\n      " (treeItem2HumanReadableSummary <$> snd <$> items) ++ "\n\n"
                            ++ "-------------\n\n"
                            ++ "input = " ++ shortShowString (LS.string s) ++ "\n\n"
                            ++ "==================\n")
                            $ Left AmbiguityError{ ranges=[singleCharacterRangeAt s] }
        where
                treeInfos::[TreeInfo]
                treeInfos = trees >>= getTreeInfos -}

shortShowString::String->String
shortShowString s | length s <= 20 = show s
shortShowString s = "\"" ++ take 20 s ++ "....\""

treeItem2HumanReadableSummary::TreeInfo->String
treeItem2HumanReadableSummary TreeInfo{tagList=tags, firstMatchers=matchers} = 
  (concat ((++ ">") <$> ("<" ++) <$> tags)) ++ (format matchers)

maximumsBy::(Eq a, Ord b, Show b)=>(a->b)->[a]->[a]
maximumsBy _ [] = []
maximumsBy f list = --jtrace (show theMaximum) $ 
  filter ((== theMaximum) . f) list
        where theMaximum = maximum (f <$> list)

addTextMatchSize::LS.LString->TreeInfo->(Int, TreeInfo)
addTextMatchSize s treeInfo@TreeInfo{allowsWhiteSpace=True} | LS.null s =
    addTextMatchSize s treeInfo{allowsWhiteSpace=False}
addTextMatchSize s treeInfo@TreeInfo{allowsWhiteSpace=True} | isSpace (LS.head s) =
    addTextMatchSize (LS.tail s) treeInfo
addTextMatchSize s treeInfo =
    (maximum (textMatchMatchSize s <$> (firstMatchers treeInfo)), treeInfo)

textMatchMatchSize::LS.LString->Expression->Int
textMatchMatchSize s (TextMatch text _) | text `isPrefixTextMatch` LS.string s = length text
textMatchMatchSize s EOF | null $ LS.string s = 1
textMatchMatchSize _ _ = 0

