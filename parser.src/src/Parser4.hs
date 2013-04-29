-----------------------------------------------------------------------------
--
-- Module      :  Parser4
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

module Parser4 (
    Node (Parse),
    parse,
    text2Tree,
    treeShow,
    getErrors,
    showErrors
) where

import Prelude hiding (lookup)
import Data.Char
import Data.Foldable hiding (foldl, concat, find)
import Data.List hiding (lookup)
import Data.Map hiding (map, toList)
import qualified Data.Sequence as S hiding ((><))
import Data.Sequence ((><), (<|), (|>), Seq)
import qualified Data.Text as T hiding (length, map)

import Colors
import GrammarParser

import Debug.Trace

data Node =
    Error { message::String, sequence::Sequence }
    | Element { name::String, children::Tree }
    | Many [Tree]
    | Parse Tree Sequence
    | Ch Char deriving (Eq)

instance Show Node where
    show (Element { name=name}) = name
    show (Ch c) = [c]
    show (Error { message=message }) = red "<error />"
    show (Many trees) = "<many>" ++ intercalate "\n----\n" (map   show trees) ++ "</many>"
    show (Parse tree sequence) = yellow ("<parse exp='") ++ sShow sequence ++ yellow ("'>") ++ take 20 (ampEscape $ tail $ show $ treeShow tree) ++ ".... " ++ yellow "</parse>"

ampEscape::String->String
ampEscape [] = []
ampEscape ('<':rest) = "&lt;" ++ (ampEscape rest)
ampEscape ('>':rest) = "&gt;" ++ (ampEscape rest)
ampEscape (c:rest) = c:(ampEscape rest)


showError::Node->String
showError Error { message=message, Parser4.sequence=sequence } = message ++ ", expecting '" ++ sShow sequence ++ "'"

showErrors::Tree->String
showErrors tree = intercalate "\n----\n" (map showError (getErrors tree))

getErrors::Tree->[Node]
getErrors tree | S.length tree == 0 = []
getErrors tree = case S.index tree 0 of
    e@(Error _ _)->e:getErrors (S.drop 1 tree)
    _->getErrors (S.drop 1 tree)

treeShow::Tree->String
treeShow tree = concat (map show (toList tree))

type Tree = Seq Node

text2Tree::T.Text->Seq Node
text2Tree text = S.fromList (map (\c -> Ch c) (T.unpack text))

string2Tree::String->Tree
string2Tree s = text2Tree (T.pack s)

parse::Grammar->String->Tree->Tree
parse g filename tree = case substitute g "element" 1 tree of
    Just tree2 -> tree2
    Nothing -> S.singleton (Error "qqqq" [Link "element"])

substitute::Grammar->String->Int->Tree->Maybe Tree
substitute g name position tree =
    case lookup name (ruleMap g) of
        Just e -> case substituteSequence e position tree of
            Just (newTree, length) -> trace "second" $
                Just ((S.drop position tree |> Element name newTree) >< S.take (position + length) tree)
            Nothing -> Nothing
        Nothing -> error ("Rule '" ++ name ++ "' does not exist")

substituteSequence::Sequence->Int->Tree->Maybe (Tree, Int)
substituteSequence [] position tree = Just (S.empty, 0)
substituteSequence (e:rest) position tree =
    case substituteExpression e position tree of
        Just (newTree1, length1) ->
            case substituteSequence rest (position+length1) tree of
                Just (newTree2, length2) -> Just (newTree1 >< newTree2, length1 + length2)
                Nothing -> Nothing
        Nothing -> Nothing

isJust::Maybe a->Bool
isJust (Just _) = True
isJust _ = False

substituteExpression::Expression->Int->Tree->Maybe (Tree, Int)
substituteExpression (WhiteSpace _) position tree =
        Just (S.empty, S.length $ S.takeWhileL (\x -> case x of Ch c->isSpace c; _->False) (S.drop position tree))
substituteExpression (TextMatch text) position tree =
    if (string2Tree text) == (S.take (length text) (S.drop position tree))
        then Just (S.empty, length text)
        else Nothing
substituteExpression (Link name) position tree = case S.index tree 1 of
    Element { name=eName } | name==eName -> Just (S.take 1 tree, 1)
    _ -> Nothing
substituteExpression (Or options) position tree =
    case find isJust (map (\option -> substituteSequence option position tree) options) of
        Just x -> x
        Nothing -> Nothing
substituteExpression e position tree = Nothing
--    Just (S.singleton Error { message="Unknown Case", Parser4.sequence=[e] }, 0)

