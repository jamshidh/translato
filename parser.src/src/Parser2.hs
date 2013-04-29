-----------------------------------------------------------------------------
--
-- Module      :  Parser2
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

module Parser2 (
    parse,
    Node (Text, Parse),
    getErrors,
    showError
) where

import Prelude hiding (lookup)
import Data.Char
import Data.List hiding (lookup)
import Data.Map hiding (map)
import qualified Data.Text as T

import Colors
import GrammarParser
import qualified LText as L

data Node = Element { name::String, attributes::Map String String, children::[Node], orig::L.LText }
    | Text L.LText
    | Parse Sequence [Node]
    | ParsedAttribute L.LText [Node]
    | Error { message::String, expression::Expression, nodes::[Node] }

instance Eq Node where
    n1 == n2 = getOrigFromNode n1 == getOrigFromNode n2

instance Ord Node where
    n1 <= n2 = getOrigFromNode n1 <= getOrigFromNode n2

err m e n = Error { message=m, expression=e, nodes=n }

instance Show Node where
    show (Text text) = show text
    show (Element {name=name, attributes=attributes, children=children}) = green ("<" ++ name ++ ">")
        ++ concat (map show children) ++ green ("</" ++ name ++ ">")
    show (Error message e nodes) = red "<error />"
    show (Parse e nodes) = "<parse exp='" ++ show e ++ "'>" ++ show nodes ++ "</parse>"
    show (ParsedAttribute name nodes) = "<attribute name='" ++ show name ++ "'>" ++ show nodes ++ "</attribute>"

showError::Node->String
showError (Error message e nodes) =
    "Error: " ++ message ++ "\n  expression=[" ++ show e ++ "]\n  text=" ++ take 30 (concat (map show nodes)) ++ "...."

parse::Grammar->String->Node->[Node]
parse g filename (Parse e nodes) = if (rest == []) then first
    else first --[err "Extra stuff" e nodes]
    where (first, rest) = parseFirst g e nodes


parseFirst::Grammar->Sequence->[Node]->([Node], [Node])
parseFirst g [] nodes = ([], nodes)
parseFirst g (e:eRest) nodes = let (first2, nRest2) = parseFirst g eRest nRest in
        (first ++ first2, nRest)
    where (first, nRest) = parseExpressionFirst g e nodes




parseExpressionFirst::Grammar->Expression->[Node]->([Node], [Node])
parseExpressionFirst g Blank nodes = ([], nodes)
parseExpressionFirst g (TextMatch s1) (Text s2:nRest) | isPrefixOf s1 (L.lText2String s2) =
    ([], (Text (L.drop (length s1) s2)):nRest)
parseExpressionFirst g e@(TextMatch s1) nodes@(Text s2:nRest) = ([err "Text mismatch" e nodes], [])
parseExpressionFirst g (Or [e]) nodes = parseFirst g e nodes
parseExpressionFirst g (Or (first:rest)) nodes = parseFirst g first nodes `chooseWinner` parseExpressionFirst g (Or rest) nodes
parseExpressionFirst g (Tab _ seq) nodes = parseFirst g ((WhiteSpace ""):seq) nodes
parseExpressionFirst g Ident (Text s:nRest) | isAlphaNum (L.head s) =
    ([Text ident], (Text (L.drop (L.length ident) s)):nRest)
        where ident = L.takeWhile isAlphaNum s
parseExpressionFirst g (WhiteSpace _) (Text s:nRest) | isSpace (L.head s) =
    ([], (Text (L.drop (L.length spaces) s)):nRest)
        where spaces = L.takeWhile isSpace s
parseExpressionFirst g (WhiteSpace _) nodes = ([], nodes)
parseExpressionFirst g (Link name) nodes = case lookup name (ruleMap g) of
    Just e -> ([Element { name=name, attributes=empty, children=innerParse, orig=getOrigFromNodes nodes }], rest)
        where (innerParse, rest) = parseFirst g e nodes
    Nothing -> ([err "Unknown link rule" (Link name) nodes], [])
parseExpressionFirst g (Attribute name e) nodes = ([ParsedAttribute (L.initialize name) innerParse], rest)
    where (innerParse, rest) = parseFirst g e nodes
parseExpressionFirst g e nodes = ([err "Unknown case" e nodes], [])



getOrigFromNodes::[Node]->L.LText
getOrigFromNodes [] = L.initialize ""
getOrigFromNodes (first:rest) = (getOrigFromNode first) { L.finish = (L.finish $ getOrigFromNode $ last rest) }

getOrigFromNode::Node->L.LText
getOrigFromNode (Element { orig=orig }) = orig
getOrigFromNode (Text text) = text
getOrigFromNode (Parse _ nodes) = getOrigFromNodes nodes
getOrigFromNode (ParsedAttribute text _) = text
getOrigFromNode (Error { nodes=nodes }) = getOrigFromNodes nodes

chooseWinner::([Node], [Node])->([Node], [Node])->([Node], [Node])
chooseWinner x y = case (getError $ fst x, getError $ fst y) of
    (Nothing, _) -> x
    (Just xError, Nothing) -> y
    (Just xError, Just yError) -> if (xError > yError) then x else y

getError::[Node]->Maybe Node
getError (Element { children=children }:rest) = case getError children of
    Nothing -> getError rest
    x -> x
getError [] = Nothing
getError ((e@Error {}):rest) = Just e
getError (_:rest) = getError rest

getErrors::[Node]->[Node]
getErrors (Element { children=children }:rest) = getErrors children ++ getErrors rest
getErrors ((ParsedAttribute _ (nodes)):rest) = getErrors nodes ++ getErrors rest
getErrors [] = []
getErrors ((e@Error {}):rest) = e:getErrors rest
getErrors (_:rest) = getErrors rest

