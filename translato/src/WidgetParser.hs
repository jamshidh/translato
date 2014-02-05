{-# LANGUAGE OverloadedStrings #-}

module WidgetParser (
    xml2Widget
) where

import Data.Functor
import qualified Data.Map as M
import Data.Text as T
import Text.XML

import Widget

xml2Widget::Element->Widget
xml2Widget el | nameLocalName (elementName el) /= pack "widget" = error "Error: Root element should be <widget>."
xml2Widget el =
    Widget{
        code = getUniqueTagText el "code",
        constructor= getUniqueTagText el "constructor",
        events=
            unpack <$>
            lookupOrError "name" "<event> tags need @name attributes" <$>
            [elementAttributes child|NodeElement child<-elementNodes el, elementName child == "event"],
        properties =
            xml2Property <$>
            [child|NodeElement child<-elementNodes el, elementName child == "property"],
        attributes =
            xml2Attribute <$>
            [child|NodeElement child<-elementNodes el, elementName child == "attribute"]
    }

lookupOrError::Ord key=>key->String->M.Map key value->value
lookupOrError key errorMessage theMap =
                    case M.lookup key theMap of
                        Just name -> name
                        _ -> error errorMessage

getUniqueTagText::Element->String->Maybe String
getUniqueTagText el tagName =
    case [child|NodeElement child<-elementNodes el, nameLocalName (elementName child) == pack tagName] of
        [] -> Nothing
        [Element{elementNodes=[NodeContent text]}] -> Just $ unpack text
        [Element{}] -> error ("Format of " ++ tagName ++ " element not correct- should be just text")
        _ -> error ("Error: Multiple " ++ tagName ++ " tags")

xml2Property::Element->Property
xml2Property el =
    Property {
        propName=
            unpack $
            lookupOrError "name" "<property> tags need @name attributes" (elementAttributes el),
        propGetter=getUniqueTagText el "getter",
        propSetter = getUniqueTagText el "setter"
    }

xml2Attribute::Element->Attribute
xml2Attribute el =
    Attribute {
        attName=
            unpack $
            lookupOrError "name" "<attribute> tags need @name attributes" (elementAttributes el),
        attGetter=getUniqueTagText el "getter",
        attSetter=getUniqueTagText el "setter",
        attRemover=getUniqueTagText el "remover"
    }





