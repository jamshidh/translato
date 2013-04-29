-----------------------------------------------------------------------------
--
-- Module      :  Parser3
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

module Parser3 (
    Node (Parse, Ch),
    getErrors,
    showErrors,
    treeShow,
    parse,
    text2Tree
) where

import Prelude hiding (lookup)
import Data.Char
import Data.Foldable hiding (foldl, concat)
import Data.List hiding (lookup)
import Data.Map hiding (map, toList)
import qualified Data.Sequence as S hiding ((><))
import Data.Sequence ((><), Seq)
import qualified Data.Text as T hiding (length, map)

import Colors
import GrammarParser

import Debug.Trace

data Node =
    Error { message::String, sequence::Sequence }
    | Element { name::String, children::[Node], orig::String }
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
showError Error { message=message, Parser3.sequence=sequence } = message ++ ", expecting '" ++ sShow sequence ++ "'"

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
parse g filename x =
    S.foldlWithIndex (\x i y -> x >< y) S.empty (S.mapWithIndex (\i y -> parseNode g filename y) x)

parseNode::Grammar->String->Node->Tree
parseNode g filename (Parse tree sequence) = substitute g sequence tree
parseNode g filename (Many trees) = S.singleton (Many (map (parse g filename) trees))



substitute::Grammar->Sequence->Tree->Tree
substitute g ((Or sequences):rest) tree = S.singleton (Many (map (S.singleton $ Parse tree) sequences))
substitute g (first:rest) tree = modified >< S.singleton (Parse unmodified rest)
    where (modified, unmodified) = substituteExp first tree

substituteExp::Expression->Tree->(Tree, Tree)
substituteExp e@(WhiteSpace _) tree =
        (S.empty, S.dropWhileL (\x -> case x of Ch c->isSpace c; _->False) tree)
substituteExp e@(TextMatch text) tree =
    if (string2Tree text) == (S.take (length text) tree)
        then (S.empty, S.drop (length text) tree)
        else (S.singleton Error { message="Unmatched Text", Parser3.sequence=[e] }, tree)
substituteExp e@(Link name) tree = case S.index tree 1 of
    Element { name=eName } | name==eName -> (S.take 1 tree, S.drop 1 tree)
    _ -> (singleton (Error { message="Unmatched element", Parser3.sequence=[e] }), S.empty)
substituteExp e tree = (S.singleton Error { message="Unknown Case", Parser3.sequence=[e] }, S.empty)

