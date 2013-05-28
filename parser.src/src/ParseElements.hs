{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  ParseElements
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

module ParseElements (
    parseElements
) where

import Text.XML
import Text.XML.Cursor
import Data.Text hiding (map, concat, foldl1, foldl, head, intercalate, tail, length)
import Data.Text.Lazy (toStrict, fromStrict)
import Data.List

import Context
--import ManyWorldsParser
import Parser
import Grammar hiding (tagName)
import ParseError

name2String::Name->String
name2String name = unpack $ nameLocalName name

tagName::Cursor->String
tagName c = case node c of
    NodeElement element -> name2String $ elementName element

fullTagName::Cursor->Name
fullTagName c = case node c of
    NodeElement element -> elementName element

isElement::Cursor->Bool
isElement c = case node c of
    NodeElement element -> True
    _ -> False

parseElements::Context->Cursor->Document
parseElements cx c = node2Document (input2Output cx c)

node2Document::Node->Document
node2Document (NodeElement element) = Document {
    documentPrologue=Prologue {prologueBefore=[], prologueDoctype=Nothing, prologueAfter=[]},
    documentRoot=element,
    documentEpilogue=[]
    }

getElement::Cursor->Element
getElement c = case node c of
    NodeElement element -> element

s2Name::String->Name
s2Name s  = Name (pack s) Nothing Nothing

getAttribute::String->Cursor->Maybe String
getAttribute name c = if (length ((hasAttribute (s2Name name)) c) /= 0) then Just $ concat (map unpack (attribute (s2Name name) c))
    else Nothing

errorElement::String->Node
errorElement message =
    NodeElement $ Element {
    elementName = "error",
    elementAttributes = [],
    elementNodes = [NodeContent $ pack message]
    }


input2Output::Context->Cursor->Node
input2Output cx c | isElement c && tagName c == "parse" =
    case getAttribute "using" c of
        Nothing -> errorElement "<rule> element missing 'using' attribute"
        Just ruleName ->
            case parseText def (fromStrict $ pack ret) of
                Left err -> errorElement (show err)
                Right doc -> NodeElement $ documentRoot doc
            where ret = (createParserForClass ruleName) cx (concat (map unpack (child c >>= content)))
input2Output cx c | isElement c = let element = getElement c in
    NodeElement $ Element {
    elementName = fullTagName c,
    elementAttributes = elementAttributes element,
    elementNodes = map (input2Output cx) (child c)
    }
input2Output g c = node c
