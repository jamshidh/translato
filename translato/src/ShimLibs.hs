{-# LANGUAGE OverloadedStrings #-}

module ShimLibs (
  documentToLibs
  ) where

import Data.Functor
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Text.XML as XML
import Text.XML.Cursor

import ShimConfig

--Gets needed libs in a single file, but don't add libs needed by those libs
documentToLibs::XML.Document->[ShimName]
documentToLibs doc = 
      (ShimName <$> ((descendant $ fromDocument doc) >>= element "shimLib" >>= getNameAttribute))
      where --All of these helper functions are for error checking.  We need to make sure that each shimLib tag has one and only one "@name" tag.
        getNameAttribute::Cursor->[T.Text]
        getNameAttribute c = node2Name $ node c 
        node2Name::XML.Node->[T.Text]
        node2Name (XML.NodeElement XML.Element{XML.elementAttributes=atts}) = [uniqueNameValue $ M.toList atts]
        uniqueNameValue::[(XML.Name, T.Text)]->T.Text
        uniqueNameValue [] = error "A <shimLib> tag is missing its 'name' attribute."
        uniqueNameValue [("name", value)] = value
        uniqueNameValue atts = error ("unexpected attributes in <shimLib>: [" ++ intercalate ", " (showAttribute <$> atts) ++ "]\n----Remember, only 'name' is allowed.")
        showAttribute::(XML.Name, T.Text)->String
        showAttribute (n, v)=T.unpack (XML.nameLocalName n) ++ "='" ++ T.unpack v ++ "'"


