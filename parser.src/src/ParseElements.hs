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
import Data.List

import ManyWorldsParser
import Parser
import GrammarParser
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

parseElements::Grammar->Cursor->Document
parseElements g c = node2Document (input2Output g c)

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


input2Output::Grammar->Cursor->Node
input2Output g c | isElement c && tagName c == "parse" =
    case rule of
        Just name -> case getParser name of
            Just states -> case (parse "file" states (concat (map unpack (child c >>= content)))) of
                Left err -> errorElement ("There were errors:\n  " ++ showErrors err)
                Right [[val]] -> val
                Right _ -> errorElement "Parse was ambiguous"
            Nothing -> errorElement ("Missing rule in grammar: " ++ name)
        Nothing -> errorElement ("<rule> element missing 'using' attribute")
    where rule = getAttribute "using" c; getParser name = createParserWithStartRule name g
input2Output g c | isElement c = let element = getElement c in
    NodeElement $ Element {
    elementName = fullTagName c,
    elementAttributes = elementAttributes element,
    elementNodes = map (input2Output g) (child c)
    }
input2Output g c = node c
