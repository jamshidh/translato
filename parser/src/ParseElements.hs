{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
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
    parseElements,
    parseElementsMain
) where

import Data.Functor
import Data.List
import Data.Map as M hiding (map)
import Data.Text hiding (map, concat, foldl1, foldl, head, intercalate, tail, length)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Text.XML
import Text.XML.Cursor
import System.Console.GetOpt

import ArgOpts
import Parser
import Grammar hiding (tagName, Name)
import GrammarTools
import ParseError
import SequenceMap

--import JDebug

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
    elementAttributes = M.empty,
    elementNodes = [NodeContent $ pack message]
    }


input2Output::Grammar->Cursor->Node
input2Output g c | isElement c && tagName c == "parse" =
    case getAttribute "using" c of
        Nothing -> errorElement "<rule> element missing 'using' attribute"
        Just ruleName ->
            case parseText def (TL.fromStrict $ pack ret) of
                Left err -> errorElement (show err)
                Right doc -> NodeElement $ documentRoot doc
            where
                textContent = TL.concat $ TL.fromStrict <$> (child c >>= content)
                ret = createParserForClass (TL.pack ruleName) g textContent
input2Output sMap c | isElement c = let element = getElement c in
    NodeElement $ Element {
    elementName = fullTagName c,
    elementAttributes = elementAttributes element,
    elementNodes = map (input2Output sMap) (child c)
    }
input2Output g c = node c

----------------

data Options = Options { specFileName::String }
deflt = Options { specFileName = "file.spec" }

try::(Show err)=>Either err a->a
try (Left err) = error ("Error:" ++ show err)
try (Right a) = a

parseElementsMain::[String]->IO ()
parseElementsMain args = do
    let options = $(arg2Opts ''Options ["specFileName"]) args deflt
    grammar <- loadGrammarAndSimplifyForParse (specFileName options)
    contents<-TL.getContents
    let doc=try(parseText def contents)
    putStrLn (unpack $ TL.toStrict (renderText def (parseElements grammar (fromDocument doc))))









